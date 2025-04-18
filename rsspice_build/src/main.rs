//! Compile the SPICE Toolkit's FORTRAN implementation into Rust.
//!
//! You must download tspice.tar from https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/PC_Linux_64bit/
//! and extract into the workspace directory. (Be aware of its restrictions on distribution:
//! https://naif.jpl.nasa.gov/naif/rules.html)

use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use rayon::prelude::*;
use relative_path::PathExt;
use std::path::Path;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader, Write},
    path::PathBuf,
    time::Instant,
};
use tracing::{Level, error, info, span};
use walkdir::WalkDir;

use f2rust_compiler::{
    ast,
    file::{SourceLoc, parse_fixed, split_program_units},
    globan::{self, GlobalAnalysis},
    grammar,
};

fn safe_identifier(s: &str) -> String {
    // From https://doc.rust-lang.org/reference/keywords.html, 2024 edition
    const KEYWORDS: &[&str] = &[
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
        "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
        "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
        "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box", "do",
        "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
        "gen",
    ];
    if KEYWORDS.contains(&s) {
        format!("r#{s}")
    } else {
        s.to_owned()
    }
}

type Node = (String, String);
struct DepGraph {
    deps: HashMap<Node, HashSet<Node>>,
    trans: HashMap<Node, HashSet<Node>>,
    transparents: HashMap<Node, HashSet<Node>>,

    toposort_files: Vec<Node>,

    assigned: HashMap<Node, String>,
    crates: IndexMap<String, Vec<Node>>,
}

// Some crude stuff for building a subset of the code, and potentially splitting it
// into multiple crates to help build times
impl DepGraph {
    fn new(glob: &GlobalAnalysis) -> Self {
        Self {
            deps: glob.dependency_graph(),
            trans: HashMap::new(),
            transparents: HashMap::new(),

            toposort_files: Vec::new(),

            assigned: HashMap::new(),
            crates: IndexMap::new(),
        }
    }

    fn compute(&mut self) {
        for k in self.deps.keys().cloned().collect::<Vec<_>>() {
            self.find_transitive_deps(k);
        }

        for (from, to) in &self.trans {
            for to in to {
                self.transparents
                    .entry(to.clone())
                    .or_default()
                    .insert(from.clone());
            }
        }

        // We assume the dependency tree is acyclic. (This is true for spicelib,
        // not for other modules, but we only care about spicelib here.)
        //
        // Once we've calculated transitive dependencies of every file, sort by decreasing
        // number of ancestors to get a topological sort. (If X depends on Y, then Y
        // cannot have fewer ancestors than X.)
        //
        // We want to split the graph into crates like:
        //
        //      4
        //     / \
        //   3a   3b
        //    | X |
        //   2a   2b
        //    | X |
        //   1a   1b
        //
        // with each crate having ~250 files, where each level depends on the 2 crates in the
        // lower level, allowing some parallelism during builds.
        //
        // We pick a source file with many transitive dependencies, to all go into 1a.
        // Every other file that does not depend on 1a, goes into 1b.
        // Then we pick another for 2a, and everything else that depends only on 1a/1b goes
        // into 2b. Etc.

        self.toposort_files = self
            .deps
            .keys()
            .filter(|d| d.0 == "spicelib")
            .cloned()
            .collect::<Vec<_>>();
        self.toposort_files
            .sort_by_key(|f| (-(self.transparents[f].len() as i32), f.clone()));

        self.assign_files("spicelib-1a", "spicelib", |n| n == "pool");
        self.assign_rest("spicelib-1b", "spicelib", &[], None);
        self.assign_files("spicelib-2a", "spicelib", |n| n == "spkgeo");
        self.assign_rest(
            "spicelib-2b",
            "spicelib",
            &["spicelib-1a", "spicelib-1b"],
            None,
        );
        self.assign_files("spicelib-3a", "spicelib", |n| {
            n == "keeper" || n.starts_with("ek") || n.starts_with("zzek")
        });
        self.assign_rest(
            "spicelib-3b",
            "spicelib",
            &["spicelib-1a", "spicelib-1b", "spicelib-2a", "spicelib-2b"],
            None,
        );

        // for f in &self.toposort_files {
        //     println!(
        //         "{} {} {:?}",
        //         self.transparents[f].len(),
        //         self.trans[f]
        //             .iter()
        //             .filter(|d| !self.assigned.contains_key(d))
        //             .count(),
        //         f
        //     );
        // }

        self.assign_all("spicelib-4", "spicelib");
        self.assign_all("support", "support");
        self.assign_all("testutil", "testutil");

        self.toposort_files = self
            .deps
            .keys()
            .filter(|d| d.0 == "tspice")
            .cloned()
            .collect::<Vec<_>>();
        self.toposort_files
            .sort_by_key(|f| (-(self.transparents[f].len() as i32), f.clone()));

        let deps = &[
            "spicelib-1a",
            "spicelib-1b",
            "spicelib-2a",
            "spicelib-2b",
            "spicelib-3a",
            "spicelib-3b",
            "spicelib-4",
            "support",
            "testutil",
        ];
        self.assign_rest("tspice-1a", "tspice", deps, Some(100));
        self.assign_rest("tspice-1b", "tspice", deps, Some(100));
        self.assign_rest("tspice-1c", "tspice", deps, Some(100));
        self.assign_rest("tspice-1d", "tspice", deps, Some(1000));
        self.assign_all("tspice-2", "tspice");

        for ns in HashSet::<String>::from_iter(self.deps.keys().map(|d| d.0.clone())) {
            self.assign_all("programs", &ns);
        }

        for vs in self.crates.values_mut() {
            vs.sort();
        }
    }

    /// Assign a crate to all files matching the namespace and predicate,
    /// plus all their transitive dependencies
    fn assign_files<P: Fn(&str) -> bool>(
        &mut self,
        cratename: &str,
        namespace: &str,
        predicate: P,
    ) {
        for file in self
            .toposort_files
            .iter()
            .filter(|(ns, nm)| ns == namespace && predicate(nm))
        {
            for dep in &self.trans[file] {
                if !self.assigned.contains_key(dep) {
                    self.assigned.insert(dep.clone(), cratename.to_owned());
                    self.crates
                        .entry(cratename.to_owned())
                        .or_default()
                        .push(dep.clone());
                }
            }
        }
    }

    /// Assign a crate to all files matching the namespace, whose dependencies are
    /// either in this crate or in `deps`. Stop when the crate size reaches some limit
    fn assign_rest(
        &mut self,
        cratename: &str,
        namespace: &str,
        deps: &[&str],
        limit: Option<usize>,
    ) {
        for file in &self.toposort_files {
            if file.0 == namespace
                && !self.assigned.contains_key(file)
                && self.trans[file]
                    .iter()
                    .all(|dep| match self.assigned.get(dep) {
                        None => true,
                        Some(ass) => ass == cratename || deps.contains(&ass.as_str()),
                    })
            {
                self.assigned.insert(file.clone(), cratename.to_owned());
                self.crates
                    .entry(cratename.to_owned())
                    .or_default()
                    .push(file.clone());
            }

            if let Some(limit) = limit {
                if self.crates.get(cratename).is_some_and(|c| c.len() >= limit) {
                    break;
                }
            }
        }
    }

    /// Assign a crate to all files matching the namespace
    fn assign_all(&mut self, cratename: &str, namespace: &str) {
        for file in self.deps.keys() {
            if file.0 == namespace && !self.assigned.contains_key(file) {
                self.assigned.insert(file.clone(), cratename.to_owned());
                self.crates
                    .entry(cratename.to_owned())
                    .or_default()
                    .push(file.clone());
            }
        }
    }

    fn find_transitive_deps(&mut self, start: Node) -> HashSet<Node> {
        let mut found = HashSet::new();
        found.insert(start.clone());

        let mut open = vec![start.clone()];
        while let Some(node) = open.pop() {
            for next in self.deps.get(&node).unwrap() {
                if let Some(next_trans) = self.trans.get(next) {
                    found.extend(next_trans.iter().cloned());
                } else if found.insert(next.clone()) {
                    open.push(next.clone());
                }
            }
        }

        self.trans.insert(start.clone(), found.clone());
        found
    }

    fn dump(&self) {
        for (cname, files) in &self.crates {
            println!("Assigned to {cname}: {}", files.len());
            // println!(
            //     "  {}",
            //     files
            //         .iter()
            //         .map(|(_, b)| b.to_owned())
            //         .collect::<Vec<_>>()
            //         .join(" ")
            // );
        }
    }
}

// Extract the keywords from SPICE comments
fn read_keywords(path: &PathBuf) -> Result<Vec<String>> {
    let mut keywords = Vec::new();
    let mut section = "".to_owned();
    for line in BufReader::new(File::open(path)?).lines() {
        let line = line?;
        if let Some(s) = line.strip_prefix("C$ ") {
            section = s.to_owned();
        } else if line.trim() == "C-&" {
            section = "".to_owned();
        } else if let Some(k) = line.strip_prefix("C     ") {
            if section == "Keywords" || section == "Required_Reading" {
                for k in k.split(",") {
                    for k in k.split(" --- ") {
                        let k = k.trim();
                        if !k.is_empty() {
                            if section == "Required_Reading" {
                                if k.to_uppercase() != "NONE." {
                                    keywords.push("$".to_string() + &k.to_uppercase());
                                }
                            } else {
                                keywords.push(k.to_uppercase());
                            }
                        }
                    }
                }
            }
        }
    }

    keywords.sort();
    keywords.dedup();

    if keywords.is_empty() {
        keywords.push("_NONE".to_owned());
    }

    Ok(keywords)
}

struct GrammarPatcher {}

impl GrammarPatcher {
    fn new() -> Self {
        Self {}
    }

    // SWAPI/SWAPD/SWAPC are typically used to swap two elements of the same array.
    // There's no static guarantee that the elements are distinct (and maybe not even
    // a dynamic guarantee, so get_disjoint_mut etc might not even work, and would be
    // quite complicated to implement in codegen).
    //
    // So we patch the code, to replace them with something simpler.
    fn patch_swap(&mut self, stmt: &grammar::Statement) -> Option<grammar::Statement> {
        if let grammar::Statement::Call(name, args) = stmt {
            if !matches!(name.as_str(), "SWAPI" | "SWAPD" | "SWAPC") {
                return None;
            }
            if args.len() != 2 {
                return None;
            }
            if let (
                grammar::Expression::ArrayElementOrFunction(s0, idx0),
                grammar::Expression::ArrayElementOrFunction(s1, idx1),
            ) = (&args[0], &args[1])
            {
                if s0 != s1 || idx0.len() != idx1.len() {
                    return None;
                }

                let new_args = vec![
                    grammar::Expression::ArrayElementOrFunction(
                        "ARRAY_SUBSCRIPT_VALUE".to_owned(),
                        vec![args[0].clone()],
                    ),
                    grammar::Expression::ArrayElementOrFunction(
                        "ARRAY_SUBSCRIPT_VALUE".to_owned(),
                        vec![args[1].clone()],
                    ),
                    grammar::Expression::Symbol(s0.to_owned()),
                ];
                return Some(grammar::Statement::Call(format!("{name}_ARRAY"), new_args));
            }
        }

        None
    }

    fn patch(
        &mut self,
        code: Vec<(SourceLoc, grammar::Statement)>,
    ) -> Vec<(SourceLoc, grammar::Statement)> {
        code.into_iter()
            .map(|(loc, stmt)| {
                let stmt = self.patch_swap(&stmt).unwrap_or(stmt);
                (loc, stmt)
            })
            .collect()
    }
}

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .without_time()
        .with_target(false)
        .with_file(true)
        .with_line_number(true)
        .init();

    let src_root = PathBuf::from("tspice/src");
    let override_root = PathBuf::from("rsspice_build/override");
    let gen_root = PathBuf::from("generated");

    let t0 = Instant::now();

    let paths = WalkDir::new(&src_root)
        .into_iter()
        .collect::<walkdir::Result<Vec<_>>>()?;

    let patches = [
        ("spicelib", "swapi_array.f"),
        ("spicelib", "swapd_array.f"),
        ("spicelib", "swapc_array.f"),
    ];
    let patches: Vec<_> = patches
        .iter()
        .map(|(namespace, filename)| src_root.join(namespace).join(filename))
        .collect();

    let program_units = paths
        .iter()
        .filter(|entry| {
            matches!(
                entry.path().extension().and_then(|s| s.to_str()),
                Some("f") | Some("pgm")
            )
        })
        .map(|entry| entry.path())
        .chain(patches.iter().map(|p| p.as_path()))
        .par_bridge()
        .map(|path| -> Result<Vec<Result<_>>> {
            let namespace = path
                .parent()
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy();
            let filename = path.file_name().unwrap().to_string_lossy();

            let _span = span!(
                Level::INFO,
                "build",
                file = path.to_string_lossy().to_string()
            )
            .entered();

            let mut override_path = override_root.clone();
            override_path.push(&*namespace);
            override_path.push(&*filename);
            let path = if std::fs::exists(&override_path)? {
                override_path
            } else {
                path.to_path_buf()
            };

            let parsed = parse_fixed(&path.relative_to(".").unwrap(), Path::new("."))
                .context(format!("parsing {path:?}"))?;

            let mut patcher = GrammarPatcher::new();
            let parsed = patcher.patch(parsed);

            Ok(split_program_units(parsed)
                .into_iter()
                .enumerate()
                .map(|(i, parsed)| {
                    let ast = ast::Parser::new()
                        .parse(parsed)
                        .context(format!("parsing {path:?}"))?;

                    // Need to give each program unit a unique name
                    let basename = filename
                        .strip_suffix(".f")
                        .or(filename.strip_suffix(".pgm"))
                        .unwrap();
                    let pu_filename = if i == 0 {
                        basename.to_string()
                    } else {
                        format!("{basename}__{i}")
                    };

                    let keywords = (namespace.clone(), filename.clone(), read_keywords(&path)?);
                    Ok((
                        globan::ProgramUnit::new(&namespace, &pu_filename, ast),
                        keywords,
                    ))
                })
                .collect::<Vec<_>>())
        });

    // Extract grammar parsing errors
    let (program_units, errs): (Vec<_>, Vec<_>) = program_units.partition(|r| r.is_ok());

    if !errs.is_empty() {
        for err in errs {
            error!("Failed: {:?}", err.err().unwrap());
        }
        bail!("Compilation failed");
    }

    // Extract AST parsing errors
    let (program_units, errs): (Vec<_>, Vec<_>) = program_units
        .into_iter()
        .flat_map(|pu| pu.unwrap())
        .partition(|r| r.is_ok());

    if !errs.is_empty() {
        for err in errs {
            error!("Failed: {:?}", err.err().unwrap());
        }
        bail!("Compilation failed");
    }

    info!("Parsed all in {:?}", t0.elapsed());

    let (program_units, keywords): (Vec<_>, Vec<_>) =
        program_units.into_iter().map(|pu| pu.unwrap()).unzip();

    // Create a list of files that use each keyword
    let mut keyword_files = HashMap::new();
    for (ns, name, kws) in keywords {
        for kw in kws {
            keyword_files
                .entry(kw)
                .or_insert_with(Vec::new)
                .push((ns.to_string(), name.to_string()));
        }
    }

    let mut glob = globan::GlobalAnalysis::new(&["spicelib", "support", "testutil"], program_units);
    glob.analyse()?;

    let mut deps = DepGraph::new(&glob);
    deps.compute();

    deps.dump();
    println!("Unassigned: {}", deps.deps.len() - deps.assigned.len());

    // let mut sources = HashSet::new();
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_aaaaphsh".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_ab".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_bodvar".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_ckcov".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_ckgp".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_et2utc".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_euler".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_gfuds".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_m2q".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_moved".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_pxform".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_q2m".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_sclk".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_str2et".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_term".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_vector3".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_vectorg".to_owned())].clone());
    // sources.extend(deps.trans[&("tspice".to_owned(), "f_zzplat".to_owned())].clone());
    //
    // sources.extend(deps.trans[&("testutil".to_owned(), "tsetup".to_owned())].clone());
    // sources.extend(deps.trans[&("testutil".to_owned(), "tclose".to_owned())].clone());
    //
    // // Some old manual tests
    // sources.extend(deps.trans[&("spicelib".to_owned(), "ana".to_owned())].clone());
    // sources.extend(deps.trans[&("spicelib".to_owned(), "benum".to_owned())].clone());
    //
    // // EQUIVALENCE aliasing
    // sources.extend(deps.trans[&("support".to_owned(), "lbrem_1".to_owned())].clone());
    //
    // sources.extend(deps.trans[&("testutil".to_owned(), "t_pck09".to_owned())].clone());
    // sources.extend(deps.trans[&("spicelib".to_owned(), "zzgflong".to_owned())].clone());
    //
    // let mut sources = Vec::from_iter(sources.iter());

    let mut sources = deps.assigned.keys().collect::<Vec<_>>();

    sources.sort();

    println!("Compiling {} files", sources.len());

    for (name, ds) in [
        ("spicelib-1a", vec![]),
        ("spicelib-1b", vec![]),
        ("spicelib-2a", vec!["spicelib-1a", "spicelib-1b"]),
        ("spicelib-2b", vec!["spicelib-1a", "spicelib-1b"]),
        (
            "spicelib-3a",
            vec!["spicelib-1a", "spicelib-1b", "spicelib-2a", "spicelib-2b"],
        ),
        (
            "spicelib-3b",
            vec!["spicelib-1a", "spicelib-1b", "spicelib-2a", "spicelib-2b"],
        ),
        (
            "spicelib-4",
            vec![
                "spicelib-1a",
                "spicelib-1b",
                "spicelib-2a",
                "spicelib-2b",
                "spicelib-3a",
                "spicelib-3b",
            ],
        ),
        (
            "spicelib",
            vec![
                "spicelib-1a",
                "spicelib-1b",
                "spicelib-2a",
                "spicelib-2b",
                "spicelib-3a",
                "spicelib-3b",
                "spicelib-4",
            ],
        ),
        ("support", vec!["spicelib"]),
        ("testutil", vec!["spicelib", "support"]),
        ("tspice-1a", vec!["spicelib", "support", "testutil"]),
        ("tspice-1b", vec!["spicelib", "support", "testutil"]),
        ("tspice-1c", vec!["spicelib", "support", "testutil"]),
        ("tspice-1d", vec!["spicelib", "support", "testutil"]),
        (
            "tspice-2",
            vec![
                "spicelib",
                "support",
                "testutil",
                "tspice-1a",
                "tspice-1b",
                "tspice-1c",
                "tspice-1d",
            ],
        ),
        (
            "tspice",
            vec![
                "tspice-1a",
                "tspice-1b",
                "tspice-1c",
                "tspice-1d",
                "tspice-2",
            ],
        ),
        ("programs", vec!["spicelib", "support", "testutil"]),
    ] {
        let cratename = format!("rsspice_{name}");

        let path = gen_root.join(&cratename);

        std::fs::create_dir_all(path.join("src"))?;

        let cratefiles = deps.crates.get(name).cloned().unwrap_or_default();
        let mut namespaces: Vec<_> = cratefiles.iter().map(|(ns, _nm)| ns).collect();
        namespaces.sort();
        namespaces.dedup();
        for ns in &namespaces {
            let src = path.join("src").join(ns);

            if !std::fs::exists(&src)? {
                std::fs::create_dir(&src)?;
            }
        }

        let mut cargo = std::fs::File::create(path.join("Cargo.toml"))?;
        writeln!(cargo, "#\n# GENERATED FILE\n#\n")?;
        writeln!(cargo, "[package]")?;
        writeln!(cargo, r#"name = "{cratename}""#)?;
        writeln!(cargo, r#"version = "0.1.0""#)?;
        writeln!(cargo, r#"edition = "2024""#)?;
        writeln!(cargo)?;
        writeln!(cargo, "[dependencies]")?;
        writeln!(cargo, r#"f2rust_std = {{ path = "../../f2rust_std" }}"#)?;
        for d in &ds {
            writeln!(cargo, r#"rsspice_{d} = {{ path = "../rsspice_{d}" }}"#)?;
        }
        drop(cargo);

        let mut ignore = std::fs::File::create(path.join("src/.gitignore"))?;
        writeln!(ignore, "*/")?;
        drop(ignore);

        let mut librs = std::fs::File::create(path.join("src/lib.rs"))?;
        writeln!(librs, "//\n// GENERATED FILE\n//\n")?;
        writeln!(librs, "#![allow(unused_imports)]\n")?;
        if name == "spicelib" || name == "tspice" {
            writeln!(librs, "pub mod {name} {{")?;
            for d in &ds {
                let dcrate = format!("rsspice_{d}");
                let dmod = dcrate.replace("-", "_");
                writeln!(librs, "    pub use {dmod}::{name}::*;")?;
            }
            writeln!(librs, "}}")?;
        } else {
            for ns in &namespaces {
                writeln!(librs, "pub mod {ns};")?;
            }
        }
        if ds.contains(&"support") {
            writeln!(librs)?;
            writeln!(librs, "pub(crate) use rsspice_support as support;")?;
        }
        if ds.contains(&"testutil") {
            writeln!(librs)?;
            writeln!(librs, "pub(crate) use rsspice_testutil as testutil;")?;
        }
        drop(librs);

        for ns in &namespaces {
            let mut modrs = std::fs::File::create(path.join("src").join(ns).join("mod.rs"))?;
            writeln!(modrs, "//\n// GENERATED FILE\n//\n")?;
            writeln!(modrs, "#![allow(non_snake_case)]")?;
            writeln!(modrs, "#![allow(unused_parens, clippy::double_parens)]")?;
            writeln!(modrs, "#![allow(unused_mut, unused_assignments)]")?;
            writeln!(modrs, "#![allow(unused_imports)]")?;
            writeln!(modrs, "#![allow(unused_variables)]")?;
            writeln!(modrs, "#![allow(unreachable_code)]")?;
            writeln!(modrs, "#![allow(dead_code)]")?;
            writeln!(modrs, "#![allow(clippy::while_immutable_condition)]")?;
            writeln!(modrs, "#![allow(clippy::assign_op_pattern)]")?;
            writeln!(modrs, "#![allow(clippy::needless_return)]")?;
            writeln!(modrs, "#![allow(clippy::unnecessary_cast)]")?;
            writeln!(modrs, "#![allow(clippy::if_same_then_else)]")?;
            writeln!(modrs, "#![allow(clippy::needless_bool_assign)]")?;
            writeln!(modrs, "#![allow(clippy::collapsible_if)]")?;
            writeln!(modrs, "#![allow(clippy::too_many_arguments)]")?;
            writeln!(modrs, "#![allow(clippy::type_complexity)]")?;
            writeln!(modrs)?;

            for d in &ds {
                let dcrate = format!("rsspice_{d}");
                let dmod = dcrate.replace("-", "_");
                if d.starts_with("spicelib") && ns.starts_with("spicelib") {
                    writeln!(modrs, "use {dmod}::spicelib::*;")?;
                } else if d.starts_with("tspice") && ns.starts_with("tspice") {
                    writeln!(modrs, "use {dmod}::tspice::*;")?;
                } else {
                    writeln!(modrs, "use {dmod}::{d};")?;
                }
            }
            writeln!(modrs)?;

            let mut modnames = cratefiles
                .iter()
                .filter(|f| sources.contains(f))
                .map(|f| f.1.clone())
                .collect::<Vec<_>>();
            modnames.sort();
            for name in &modnames {
                let name_id = safe_identifier(name);
                writeln!(modrs, "mod {name_id};")?;
            }
            writeln!(modrs)?;
            for name in &modnames {
                let name_id = safe_identifier(name);
                writeln!(modrs, "pub use {name_id}::*;")?;
            }
        }
    }

    const PRETTY_PRINT: bool = false;

    let mut succeeded = 0;
    for node @ (namespace, filename) in &sources {
        // info!("Compiling");

        let code = glob.codegen(namespace, filename, PRETTY_PRINT);

        match code {
            Err(err) => {
                error!("Failed to compile {namespace}/{filename}: {:?}", err);
            }
            Ok(code) => {
                let file_root = PathBuf::from(filename).with_extension("");

                let cratename = format!("rsspice_{}", &deps.assigned[node]);

                let path = gen_root.join(&cratename);

                let src = path.join("src").join(namespace);

                let mut file = File::create(src.join(file_root.with_extension("rs")))?;
                writeln!(file, "//\n// GENERATED FILE\n//\n")?;
                writeln!(file, "use super::*;")?;
                file.write_all(code.as_bytes())?;

                succeeded += 1;
            }
        }
    }

    println!("Successfully built {succeeded}/{}", sources.len());

    Ok(())
}
