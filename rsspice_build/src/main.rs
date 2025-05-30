//! Compile the SPICE Toolkit's FORTRAN implementation into Rust.

use anyhow::{Context, Result, bail};
use html5ever::ParseOpts;
use html5ever::tendril::TendrilSink;
use html5ever::tree_builder::TreeBuilderOpts;
use indexmap::IndexMap;
use markup5ever_rcdom::{Handle, NodeData, RcDom};
use rayon::prelude::*;
use relative_path::PathExt;
use std::ffi::OsStr;
use std::fmt::Write as FmtWrite;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    time::Instant,
};
use tracing::{Level, error, info, span};
use walkdir::WalkDir;

use f2rust_compiler::util::parse_header_comments;
use f2rust_compiler::{
    ast, codegen,
    file::{SourceLoc, parse_fixed, split_program_units},
    globan::{self, GlobalAnalysis},
    grammar,
};

// This reduces build times by ~50% (and even better if there are lots of build errors)
// TODO: this option no longer works, we should probably just delete it
const SPLIT_SPLICELIB_CRATES: bool = false;

const BUILD_PROGRAMS: bool = false;

// From https://doc.rust-lang.org/reference/keywords.html, 2024 edition
const KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "gen",
];

fn safe_identifier(s: &str) -> String {
    if KEYWORDS.contains(&s) {
        format!("{s}_")
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

        if SPLIT_SPLICELIB_CRATES {
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
        } else {
            self.assign_all("spicelib", "spicelib");
        }

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

        if SPLIT_SPLICELIB_CRATES {
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
        } else {
            self.assign_all("tspice", "tspice");
        }

        if BUILD_PROGRAMS {
            for ns in HashSet::<String>::from_iter(self.deps.keys().map(|d| d.0.clone())) {
                self.assign_all("programs", &ns);
            }
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
                        "F2RUST_ARRAY_SUBSCRIPT_VALUE".to_owned(),
                        vec![args[0].clone()],
                    ),
                    grammar::Expression::ArrayElementOrFunction(
                        "F2RUST_ARRAY_SUBSCRIPT_VALUE".to_owned(),
                        vec![args[1].clone()],
                    ),
                    grammar::Expression::Symbol(s0.to_owned()),
                ];
                return Some(grammar::Statement::Call(format!("{name}_ARRAY"), new_args));
            }
        }

        None
    }

    // See zzholdd.f
    fn patch_zzholdd(&mut self, stmt: &grammar::Statement) -> Option<grammar::Statement> {
        if let grammar::Statement::Call(name, args) = stmt {
            if !matches!(name.as_str(), "ZZHOLDD") {
                return None;
            }
            if let grammar::Expression::Symbol(s) = &args[0] {
                if s == "ZZPUT" {
                    return Some(grammar::Statement::Call(
                        "ZZHOLDD_ZZPUT".to_owned(),
                        args.clone(),
                    ));
                }
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
                let stmt = self.patch_zzholdd(&stmt).unwrap_or(stmt);
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
    let gen_root = PathBuf::from("rsspice/src/generated");
    let bin_root = PathBuf::from("rsspice/src/bin");

    std::fs::create_dir_all(&gen_root)?;
    if BUILD_PROGRAMS {
        std::fs::create_dir_all(&bin_root)?;
    }

    translate_reqs(&src_root, &gen_root)?;
    translate_incs(&src_root, &gen_root)?;

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

            let parsed = parse_fixed(&path.relative_to(".").unwrap(), Path::new("."), false)
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

    let bins: Vec<_> = program_units
        .iter()
        .filter_map(|pu| {
            if matches!(pu.ast.ty, ast::ProgramUnitType::Program) {
                Some((pu.namespace.clone(), pu.filename.clone()))
            } else {
                None
            }
        })
        .collect();

    let mut exports = HashMap::new();
    let mut exports_api = HashMap::new();

    for pu in &program_units {
        let k = (pu.namespace.clone(), pu.filename.clone());
        for entry in &pu.ast.entries {
            exports
                .entry(k.clone())
                .or_insert_with(Vec::new)
                .push(entry.name.clone());

            if let Some(api) = &entry.api_name {
                if is_public_api(&pu.namespace, &pu.filename) {
                    exports_api
                        .entry(k.clone())
                        .or_insert_with(Vec::new)
                        .push(api.clone());
                }
            }
        }
    }

    let mut glob = globan::GlobalAnalysis::new(&["spicelib", "support", "testutil"], program_units);
    glob.analyse()?;

    let mut deps = DepGraph::new(&glob);
    deps.compute();

    deps.dump();
    println!("Unassigned: {}", deps.deps.len() - deps.assigned.len());

    let mut sources = deps.assigned.keys().collect::<Vec<_>>();

    sources.sort();

    println!("Compiling {} files", sources.len());

    let mut crates = Vec::new();
    if SPLIT_SPLICELIB_CRATES {
        crates.extend([
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
        ]);
    } else {
        crates.push(("spicelib", vec![]));
    }
    crates.extend([
        ("support", vec!["spicelib"]),
        ("testutil", vec!["spicelib", "support"]),
        ("tspice", vec!["spicelib", "support", "testutil"]),
    ]);

    {
        let mut modrs = File::create(gen_root.join("mod.rs"))?;
        writeln!(modrs, "//\n// GENERATED FILE\n//\n")?;
        writeln!(modrs, "#![allow(dead_code)]")?;
        writeln!(modrs, "#![allow(non_snake_case)]")?;
        writeln!(modrs, "#![allow(unreachable_code)]")?;
        writeln!(modrs, "#![allow(unused_imports)]")?;
        writeln!(modrs, "#![allow(unused_mut, unused_assignments)]")?;
        writeln!(modrs, "#![allow(unused_parens, clippy::double_parens)]")?;
        writeln!(modrs, "#![allow(unused_variables)]")?;
        // It can be useful to have clippy point out issues with the generated code.
        // But a lot of the issues are with the FORTRAN code which we can't fix,
        // or are false positives, or are cases where we deliberately emit non-ideal
        // code because it's easier. So turn lots of them off:
        writeln!(modrs, "#![allow(clippy::assign_op_pattern)]")?;
        writeln!(modrs, "#![allow(clippy::collapsible_else_if)]")?;
        writeln!(modrs, "#![allow(clippy::collapsible_if)]")?;
        writeln!(modrs, "#![allow(clippy::comparison_chain)]")?;
        writeln!(modrs, "#![allow(clippy::if_same_then_else)]")?;
        writeln!(modrs, "#![allow(clippy::let_and_return)]")?;
        writeln!(modrs, "#![allow(clippy::manual_range_contains)]")?;
        writeln!(modrs, "#![allow(clippy::needless_bool_assign)]")?;
        writeln!(modrs, "#![allow(clippy::needless_borrow)]")?;
        writeln!(modrs, "#![allow(clippy::needless_return)]")?;
        writeln!(modrs, "#![allow(clippy::needless_update)]")?;
        writeln!(modrs, "#![allow(clippy::nonminimal_bool)]")?;
        writeln!(modrs, "#![allow(clippy::self_assignment)]")?;
        writeln!(modrs, "#![allow(clippy::too_many_arguments)]")?;
        writeln!(modrs, "#![allow(clippy::type_complexity)]")?;
        writeln!(modrs, "#![allow(clippy::unnecessary_cast)]")?;
        writeln!(modrs, "#![allow(clippy::unnecessary_to_owned)]")?;
        writeln!(modrs, "#![allow(clippy::while_immutable_condition)]")?;
        writeln!(modrs)?;
        writeln!(modrs, "mod api;")?;
        writeln!(modrs, "pub mod required_reading;")?;
        writeln!(modrs)?;

        let mut namespaces: Vec<_> = sources.iter().map(|(ns, _nm)| ns).collect();
        namespaces.sort();
        namespaces.dedup();
        for ns in &namespaces {
            if *ns != "spicelib" {
                writeln!(modrs, "#[cfg(feature = \"tspice\")]")?;
            }
            writeln!(modrs, "pub mod {ns};")?;
            writeln!(modrs)?;
        }
    }

    for (name, ds) in crates {
        let path = &gen_root;

        let cratefiles = deps.crates.get(name).cloned().unwrap_or_default();
        let mut namespaces: Vec<_> = cratefiles.iter().map(|(ns, _nm)| ns).collect();
        namespaces.sort();
        namespaces.dedup();
        for ns in &namespaces {
            let src = path.join(ns);

            if !std::fs::exists(&src)? {
                std::fs::create_dir(&src)?;
            }
        }

        for ns in &namespaces {
            let mut modrs = File::create(path.join(ns).join("mod.rs"))?;
            writeln!(modrs, "//\n// GENERATED FILE\n//\n")?;

            for d in &ds {
                if d.starts_with("spicelib") && ns.starts_with("spicelib") {
                    writeln!(modrs, "use crate::generated::spicelib::*;")?;
                } else if d.starts_with("tspice") && ns.starts_with("tspice") {
                    writeln!(modrs, "use crate::generated::tspice::*;")?;
                } else {
                    writeln!(modrs, "use crate::generated::{d};")?;
                }
            }
            writeln!(modrs)?;

            if *ns == "spicelib" {
                writeln!(modrs, "pub mod inc;")?;
                writeln!(modrs)?;
            }

            let mut modnames = cratefiles
                .iter()
                .filter(|f| f.0 == **ns && sources.contains(f))
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
                for export in exports.get(&(ns.to_string(), name.to_string())).unwrap() {
                    writeln!(modrs, "pub use {name_id}::{export};")?;
                }
            }

            if *ns == "spicelib" {
                writeln!(modrs)?;
                writeln!(modrs, "pub mod api {{")?;
                writeln!(modrs, "pub use super::inc;")?;
                for name in &modnames {
                    if let Some(apis) = exports_api.get(&(ns.to_string(), name.to_string())) {
                        let name_id = safe_identifier(name);
                        for export in apis {
                            writeln!(
                                modrs,
                                "pub use super::{name_id}::{};",
                                safe_identifier(export)
                            )?;
                        }
                    }
                }
                writeln!(modrs, "}}")?;
            }
        }

        if name == "programs" {
            for (ns, bin) in &bins {
                let func = bin.to_ascii_uppercase();
                let module = if ns == "tspice" {
                    "crate::generated::tspice"
                } else {
                    "crate::generated::programs"
                };
                let mut binrs = File::create(bin_root.join(bin).with_extension("rs"))?;
                writeln!(binrs, "//\n// GENERATED FILE\n//\n")?;
                writeln!(binrs, "use std::process::ExitCode;")?;
                writeln!(binrs)?;
                writeln!(binrs, "fn main() -> f2rust_std::Result<ExitCode> {{")?;
                writeln!(binrs, "    let mut ctx = f2rust_std::Context::new();")?;
                writeln!(binrs, "    ctx.set_args(std::env::args().collect());")?;
                writeln!(binrs)?;
                writeln!(binrs, "    let ret = {module}::{ns}::{func}(&mut ctx);")?;
                writeln!(binrs, "    match ret {{")?;
                writeln!(binrs, "        Err(f2rust_std::Error::Terminated(code)) =>")?;
                writeln!(binrs, "            Ok(ExitCode::from(code as u8)),")?;
                writeln!(binrs, "        Err(e) => Err(e),")?;
                writeln!(binrs, "        Ok(()) => Ok(ExitCode::SUCCESS),")?;
                writeln!(binrs, "    }}")?;
                writeln!(binrs, "}}")?;
            }
        }
    }

    let mut apirs = File::create(gen_root.join("api.rs"))?;
    writeln!(apirs, "//\n// GENERATED FILE\n//\n")?;
    writeln!(apirs, "use super::spicelib::*;")?;
    writeln!(
        apirs,
        "use crate::{{Cell, CharCell, CharVec, Result, SpiceContext, raw}};"
    )?;
    writeln!(apirs, "use f2rust_std::Context;")?;
    writeln!(apirs)?;
    writeln!(apirs, "fn blank(len: i32) -> String {{")?;
    writeln!(apirs, "  \" \".repeat(len as usize)")?;
    writeln!(apirs, "}}")?;
    writeln!(apirs)?;
    writeln!(apirs, "fn trim(s: String) -> String {{")?;
    writeln!(apirs, "  s.trim_end_matches(' ').to_owned()")?;
    writeln!(apirs, "}}")?;
    writeln!(apirs)?;
    writeln!(apirs, "const LBCELL: i32 = -5;")?;
    writeln!(apirs)?;
    writeln!(apirs, "impl SpiceContext<'_> {{")?;

    let mut succeeded = 0;

    for (namespace, filename) in &sources {
        // info!("Compiling");

        let api = is_public_api(namespace, filename);

        match glob.codegen(namespace, filename, api) {
            Err(err) => {
                error!("Failed to compile {namespace}/{filename}: {:?}", err);
            }
            Ok((code, code_api)) => {
                let file_root = PathBuf::from(safe_identifier(filename)).with_extension("");

                let src = gen_root.join(namespace);

                let mut file = File::create(src.join(file_root.with_extension("rs")))?;
                writeln!(file, "//\n// GENERATED FILE\n//\n")?;
                writeln!(file, "use super::*;")?;
                writeln!(file, "use f2rust_std::*;")?;
                if api {
                    writeln!(file, "use crate::SpiceContext;")?;
                }
                writeln!(file)?;

                file.write_all(code.as_bytes())?;

                if api {
                    writeln!(apirs, "{}", code_api)?;
                }

                succeeded += 1;
            }
        }
    }

    writeln!(apirs, "}}")?; // impl SpiceContext

    println!("Successfully built {succeeded}/{} functions", sources.len());

    Ok(())
}

fn is_public_api(namespace: &str, filename: &str) -> bool {
    // Only spicelib is public API
    if !namespace.starts_with("spicelib") {
        return false;
    }

    // Skip private functions
    if filename.starts_with("zz") {
        return false;
    }

    // Skip ones we've overriding, because they don't have API docs
    if matches!(filename, "swapc_array" | "swapd_array" | "swapi_array") {
        return false;
    }

    // TOUCHC is awkward because it 'returns' a string,
    // and all the TOUCH functions are useless, so skip them
    if filename.starts_with("touch") {
        return false;
    }

    true
}

struct DocParser {
    in_body: bool,
    block: bool,
    out: String,
}

impl DocParser {
    fn new() -> Self {
        Self {
            in_body: false,
            block: true,
            out: String::new(),
        }
    }

    fn escape(&self, text: &str) -> String {
        text.replace("``", "\"")
            .replace("''", "\"")
            .replace("`", "'")
            .replace("[", "\\[")
            .replace("<", "\\<")
            .replace(
                "http://naif.jpl.nasa.gov/naif/utilities.html",
                "<http://naif.jpl.nasa.gov/naif/utilities.html>",
            )
    }

    fn create_link(&self, name: &str) -> String {
        if let Some(req) = name.strip_suffix(".req") {
            format!("[{name}](crate::required_reading::{req})")
        } else {
            format!("[{name}](crate::raw::{lc})", lc = name.to_ascii_lowercase())
        }
    }

    fn inner_text(&self, node: &Handle, raw: bool) -> Result<String> {
        match node.data {
            NodeData::Text { ref contents } => {
                if raw {
                    Ok(contents.borrow().to_string())
                } else {
                    Ok(self.escape(&contents.borrow()))
                }
            }
            NodeData::Element {
                ref name,
                ref attrs,
                ..
            } => {
                let text = node
                    .children
                    .borrow()
                    .iter()
                    .map(|n| self.inner_text(n, raw))
                    .collect::<Result<String>>()?;

                Ok(match name.local.as_ref() {
                    "a" if !raw
                        && !attrs.borrow().iter().any(|attr| &attr.name.local == "name")
                        && text != "FAILED" =>
                    {
                        self.create_link(&text)
                    }
                    _ => text,
                })
            }
            _ => panic!("unknown node type"),
        }
    }

    fn pretty_text(&self, text: &str) -> String {
        let a = text.trim_ascii_start();
        let b = a.trim_ascii_end();

        let mut out = String::new();
        if a != text {
            out += " ";
        }
        out += b;
        if b.len() != a.len() {
            out += " ";
        }
        out
    }

    fn walk(&mut self, node: &Handle) -> Result<()> {
        match node.data {
            NodeData::Element { ref name, .. } if &name.local == "h1" => {
                self.in_body = true;
            }
            _ => (),
        }

        if self.in_body {
            let text_full = self.inner_text(node, false)?;
            let text = self.pretty_text(&text_full);
            match node.data {
                NodeData::Element {
                    ref name,
                    ref attrs,
                    ..
                } => match name.local.as_ref() {
                    "h1" => {
                        if !self.block {
                            writeln!(self.out)?;
                            self.block = true;
                        }
                        writeln!(self.out, "# {text}\n")?
                    }
                    "h2" => {
                        if !self.block {
                            writeln!(self.out)?;
                            self.block = true;
                        }
                        writeln!(self.out, "## {text}\n")?
                    }
                    "h3" => {
                        if !self.block {
                            writeln!(self.out)?;
                            self.block = true;
                        }
                        writeln!(self.out, "### {text}\n")?
                    }
                    "p" if attrs
                        .borrow()
                        .iter()
                        .any(|attr| &attr.name.local == "align") => {}
                    "p" => {
                        if !text.is_empty() {
                            if !self.block {
                                writeln!(self.out, "\n")?;
                                self.block = true;
                            }
                            writeln!(self.out, "{text}\n")?;
                        }
                    }
                    "ul" | "dl" => {
                        self.block = true;
                        let text = text.trim_ascii_start().strip_prefix("-- ").unwrap_or(&text);
                        writeln!(self.out, "* {text}")?;
                    }
                    "br" | "hr" => (),
                    "pre" => writeln!(self.out, "```text\n{}```", self.inner_text(node, true)?)?,
                    "a" if attrs.borrow().iter().any(|attr| &attr.name.local == "name") => (),
                    "a" => {
                        if self.block {
                            writeln!(self.out)?;
                            self.block = false;
                        }
                        write!(
                            self.out,
                            "{}",
                            self.create_link(&self.inner_text(node, true)?)
                        )?
                    }
                    _ => panic!("unhandled HTML element"),
                },
                NodeData::Text { .. } => {
                    if !text.is_empty() {
                        if self.block {
                            writeln!(self.out)?;
                            self.block = false;
                        }
                        write!(self.out, "{text}")?;
                    }
                }
                _ => (),
            }
        } else {
            for child in node.children.borrow().iter() {
                self.walk(child)?;
            }
        }

        Ok(())
    }
}

fn translate_reqs(src_root: &Path, gen_root: &Path) -> Result<()> {
    let path = gen_root.join("required_reading");
    std::fs::create_dir_all(&path)?;

    for entry in WalkDir::new(src_root.join("../doc/html/req")) {
        let entry = entry?;

        if entry.path().extension() != Some(OsStr::new("html")) {
            continue;
        }

        let stem = entry.path().file_stem().unwrap().to_str().unwrap();
        if stem == "index" {
            continue;
        }

        let mut file = File::open(entry.path())?;

        let opts = ParseOpts {
            tree_builder: TreeBuilderOpts {
                drop_doctype: true,
                ..Default::default()
            },
            ..Default::default()
        };
        let dom = html5ever::parse_document(RcDom::default(), opts)
            .from_utf8()
            .read_from(&mut file)?;
        let mut doc = DocParser::new();
        doc.walk(&dom.document)?;

        let mut docrs = File::create(path.join(stem).with_extension("rs"))?;
        for line in doc.out.lines() {
            writeln!(docrs, "//! {}", line)?;
        }
    }

    let mut modrs = File::create(path.join("mod.rs"))?;
    let re = regex::Regex::new(r#"href="(\w+)\.html">\w+<\/a> - (.*)<br>"#)?;
    for m in re.captures_iter(&std::fs::read_to_string(
        src_root.join("../doc/html/req/index.html"),
    )?) {
        let (_, [name, desc]) = m.extract();
        writeln!(modrs, "/// {desc}")?;
        writeln!(modrs, "pub mod {name};")?;
    }

    Ok(())
}

fn translate_incs(src_root: &Path, gen_root: &Path) -> Result<()> {
    let consts_path = gen_root.join("spicelib/inc");
    std::fs::create_dir_all(&consts_path)?;

    let mut modrs = File::create(consts_path.join("mod.rs"))?;
    writeln!(modrs, "//\n// GENERATED FILE\n//\n")?;
    writeln!(modrs, "#![allow(unused_parens, clippy::double_parens)]")?;
    writeln!(modrs)?;

    for entry in WalkDir::new(src_root.join("spicelib")) {
        let entry = entry?;
        let path = entry.path();

        if path.extension() != Some(OsStr::new("inc")) {
            continue;
        }

        let stem = entry.path().file_stem().unwrap().to_str().unwrap();

        let parsed = parse_fixed(&path.relative_to(".").unwrap(), Path::new("."), false)
            .context(format!("parsing {path:?}"))?;

        let consts = ast::Parser::new()
            .parse_constants(parsed)
            .context(format!("parsing {path:?}"))?;

        let consts = codegen::api::emit_constants(consts)?;

        let mut incrs = File::create(consts_path.join(stem).with_extension("rs"))?;

        let lines: Vec<_> = std::fs::read_to_string(entry.path())?
            .lines()
            .map(|line| line.to_owned())
            .collect();

        let comments = parse_header_comments(
            &lines
                .iter()
                .filter_map(|line| line.strip_prefix("C").map(|s| s.to_owned()))
                .collect::<Vec<_>>(),
        )?;

        if let Some(abstr) = comments.get("Abstract") {
            // Escape Markdown reference/HTML characters
            let abstr: Vec<_> = abstr
                .iter()
                .map(|s| s.replace("<", "\\<").replace("[", "\\["))
                .collect();

            write!(
                incrs,
                "{}",
                codegen::format_comment_block(&abstr, "//! ", true).unwrap()
            )?;
        } else if let Some(abstr) = lines
            .iter()
            .filter_map(|c| c.strip_prefix("C     Include Section:  "))
            .next()
        {
            writeln!(incrs, "//! {abstr}")?;
        } else {
            writeln!(incrs, "//! Constants")?;
        }
        writeln!(incrs, "//!")?;

        writeln!(incrs, "//! ```text")?;
        for line in &lines {
            writeln!(incrs, "//! {}", line)?;
        }
        writeln!(incrs, "//! ```")?;
        writeln!(incrs)?;
        writeln!(incrs, "{}", consts)?;

        writeln!(modrs, "pub mod {stem};")?;
    }

    Ok(())
}
