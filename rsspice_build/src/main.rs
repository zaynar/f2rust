//! Compile the SPICE Toolkit's FORTRAN implementation into Rust.
//!
//! You must download tspice.tar from https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/PC_Linux_64bit/
//! and extract into the workspace directory. (Be aware of its restrictions on distribution:
//! https://naif.jpl.nasa.gov/naif/rules.html)

use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader, Write},
    path::PathBuf,
    time::Instant,
};

use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use rayon::prelude::*;
use tracing::{Level, error, info, span};
use walkdir::WalkDir;

use f2rust_compiler::{
    ast,
    file::{SourceLoc, parse_fixed},
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
    keywords: HashMap<String, Vec<Node>>,
    deps: HashMap<Node, HashSet<Node>>,
    trans: HashMap<Node, HashSet<Node>>,

    assigned: HashMap<Node, String>,
    crates: IndexMap<String, Vec<Node>>,
}

// Some crude stuff for building a subset of the code, and potentially splitting it
// into multiple crates to help build times
impl DepGraph {
    fn new(glob: &GlobalAnalysis, keywords: HashMap<String, Vec<Node>>) -> Self {
        Self {
            keywords,
            deps: glob.dependency_graph(),
            trans: HashMap::new(),
            assigned: HashMap::new(),
            crates: IndexMap::new(),
        }
    }

    fn compute(&mut self) {
        for k in self.deps.keys().cloned().collect::<Vec<_>>() {
            self.find_transitive_deps(k);
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

        self.trans.insert(start, found.clone());
        found
    }

    // Get files from the given namespace, matching either keyword or filename
    fn files(&self, ns: &str, kws: &[&str], files: &[&str]) -> Vec<(String, String)> {
        let mut r: Vec<_> = files
            .iter()
            .map(|f| (ns.to_string(), f.to_string()))
            .collect();
        for kw in kws {
            r.extend(self.keywords[*kw].clone());
        }
        r.retain(|n| n.0 == ns);
        r.sort();
        r
    }

    // Add the given set of files to the crate, as long as they only depend on
    // files already assigned to a crate
    fn assign_crate(&mut self, cname: &str, starts: &[(String, String)]) {
        let mut files = HashSet::new();

        loop {
            let mut dirty = false;
            for start in starts {
                if self.assigned.contains_key(start) {
                    continue;
                }

                let trans = self.trans.get(start).unwrap();

                if trans
                    .iter()
                    .all(|n| n == start || self.assigned.contains_key(n))
                {
                    self.assigned.insert(start.clone(), cname.to_owned());
                    files.insert(start.clone());
                    dirty = true;
                }
            }
            if !dirty {
                break;
            }
        }

        let entry = self.crates.entry(cname.to_owned()).or_default();
        entry.extend(files);
        entry.sort();
        entry.dedup();
    }

    // Add the given set of files to the crate, plus all their transitive dependencies
    fn assign_crate_trans(&mut self, cname: &str, starts: &[(String, String)]) {
        let mut files = HashSet::new();

        for start in starts {
            if self.assigned.contains_key(start) {
                continue;
            }

            let trans = self.trans.get(start).unwrap();
            files.extend(
                trans
                    .iter()
                    .filter(|n| !self.assigned.contains_key(n))
                    .cloned(),
            );
            for n in trans {
                self.assigned.insert(n.clone(), cname.to_owned());
            }
        }

        let entry = self.crates.entry(cname.to_owned()).or_default();
        entry.extend(files);
        entry.sort();
        entry.dedup();
    }

    fn dump(&self) {
        for (cname, files) in &self.crates {
            println!("Assigned to {cname}: {}", files.len());
            if false {
                println!(
                    "  {}",
                    files
                        .iter()
                        .map(|(_, b)| b.to_owned())
                        .collect::<Vec<_>>()
                        .join(" ")
                );
            }
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
    let gen_root = PathBuf::from("rsspice_gen/src/generated");

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
        .filter(|entry| {
            // Skip these because they have multiple procedures in one file, which we don't support yet
            ![
                "f_nearpt.f",
                "f_npedln.f",
                "f_npelpt.f",
                "f_ck06.f",
                "f_xdda.f",
                "f_zzlatbox.f",
                "f_dasa2l.f",
                "f_dascud.f",
                "f_gfudb.f",
                "f_gfuds.f",
                "f_inelpl.f",
                "f_slice.f",
                "f_swapac.f",
                "f_swapad.f",
                "f_swapai.f",
                "f_symtbi.f",
                "f_term.f",
                "f_zzgfrel.f",
                "f_zzgfrelx.f",
                "f_zzgfsolv.f",
                "f_zzgfsolvx.f",
                "f_zzocced.f",
                "f_zzpdtbox.f",
                "f_zzspkfun.f",
                "f_zztanslv.f",
                "f_insert.f",
                "f_symtbc.f",
                "f_symtbd.f",
                "f_zzocced2.f",
                "p_dasa2l.f",
                "brief.pgm",
                "dskexp.pgm",
                "tspice.pgm",
            ]
            .contains(&entry.path().file_name().unwrap().to_str().unwrap())
        })
        .map(|entry| entry.path())
        .chain(patches.iter().map(|p| p.as_path()))
        .par_bridge()
        .map(|path| {
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

            let parsed = parse_fixed(&path).context(format!("parsing {path:?}"))?;

            let mut patcher = GrammarPatcher::new();
            let parsed = patcher.patch(parsed);

            let ast = ast::Parser::new()
                .parse(parsed)
                .context(format!("parsing {path:?}"))?;

            let keywords = (namespace.clone(), filename.clone(), read_keywords(&path)?);
            Ok((
                globan::ProgramUnit::new(&namespace, &filename, ast),
                keywords,
            ))
        });

    let (program_units, errs): (Vec<Result<_>>, Vec<Result<_>>) =
        program_units.partition(|r| r.is_ok());

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

    let mut deps = DepGraph::new(&glob, keyword_files);
    deps.compute();

    // HACK: this was a very rough attempt to manually split the codebase into chunks
    // of roughly <200 files, each depending only on earlier chunks. We should do something
    // much cleaner and more sensible.

    // deps.assign_crate_trans(
    //     "test",
    //     &deps.files(
    //         "tspice",
    //         &[],
    //         &[
    //             "f_vector3.f",
    //         ],
    //     ),
    // );
    deps.assign_crate_trans("test", &deps.files("testutil", &[], &["tcase.f"]));
    deps.assign_crate(
        "early",
        &deps.files(
            "spicelib",
            &[],
            &[
                "vrotv.f", "vadd.f", "vaddg.f", "vnorm.f", "vhat.f", "vproj.f", "vsub.f",
                "vcrss.f", "vlcom.f", "vdot.f", "vscl.f", "moved.f", "pos.f", "cpos.f", "beint.f",
                "beuns.f", "benum.f", "bedec.f", "frstnb.f", "q2m.f", "ana.f", "ucase.f",
                "replch.f", "ljust.f", "isrchc.f",
            ],
        ),
    );

    deps.assign_crate(
        "trace",
        &deps.files(
            "spicelib",
            &["CHARACTER", "ALPHANUMERIC", "FILES", "ERROR"],
            &[],
        ),
    );
    deps.assign_crate_trans("trace", &deps.files("spicelib", &[], &["sigerr.f"]));

    deps.assign_crate(
        "base",
        &deps.files(
            "spicelib",
            &[
                "CONSTANTS",
                "CHARACTER",
                "STRING",
                "WORD",
                "ALPHANUMERIC",
                "CONVERSION",
                "FILES",
                "ERROR",
                "UTILITY",
            ],
            &[],
        ),
    );

    deps.assign_crate_trans("test", &deps.files("tspice", &[], &["f_vector3.f"]));

    deps.assign_crate("array", &deps.files("spicelib", &["ARRAY"], &[]));

    deps.assign_crate(
        "math",
        &deps.files(
            "spicelib",
            &[
                "MATRIX", "VECTOR", "MATH", "NUMBERS", "NUMBER", "NUMERIC", "INTEGER",
            ],
            &[],
        ),
    );

    deps.assign_crate(
        "lists",
        &deps.files(
            "spicelib",
            &[
                "LIST",
                "LINKED LIST",
                "AB LINKED LIST",
                "CELLS",
                "WINDOWS",
                "SETS",
            ],
            &[],
        ),
    );

    deps.assign_crate(
        "shapes",
        &deps.files(
            "spicelib",
            &[
                "CONIC",
                "ELLIPSE",
                "ELLIPSOID",
                "$PLANES",
                "$ELLIPSES",
                "LINE",
                "LATITUDE",
                "EXTREMA",
                "INTERSECTION",
            ],
            &[],
        ),
    );
    deps.assign_crate_trans("parsing", &deps.files("spicelib", &["PARSING"], &[]));
    deps.assign_crate_trans("constants", &deps.files("spicelib", &["CONSTANTS"], &[]));
    deps.assign_crate_trans("frames", &deps.files("spicelib", &["FRAME", "FRAMES"], &[]));
    deps.assign_crate_trans("files", &deps.files("spicelib", &["FILES"], &[]));
    deps.assign_crate_trans("ek", &deps.files("spicelib", &["EK"], &[]));
    deps.assign_crate_trans("ephemeris", &deps.files("spicelib", &["EPHEMERIS"], &[]));

    for lib in ["spicelib", "support", "testutil", "tspice"] {
        deps.assign_crate_trans(
            lib,
            &deps
                .trans
                .keys()
                .filter(|k| k.0 == lib)
                .cloned()
                .collect::<Vec<_>>(),
        );
    }

    deps.dump();
    println!("Unassigned: {}", deps.trans.len() - deps.assigned.len());

    let sources = deps.crates["test"].clone();

    // let mut sources = deps.crates["early"].clone();
    // sources.extend_from_slice(&deps.crates["trace"]);
    // sources.extend_from_slice(&deps.crates["base"]);
    // sources.extend_from_slice(&deps.crates["array"]);
    // sources.extend_from_slice(&deps.crates["math"]);

    println!("Compiling {} files", sources.len());

    const PRETTY_PRINT: bool = false;
    const SINGLE_FILE_PER_MOD: bool = false;

    let mut mods = HashMap::new();
    for (namespace, filename) in &sources {
        let path = src_root.join(namespace).join(filename);

        let _span = span!(
            Level::INFO,
            "codegen",
            file = path.to_string_lossy().to_string()
        )
        .entered();

        info!("Compiling");

        let code = glob.codegen(namespace, filename, PRETTY_PRINT);

        match code {
            Err(err) => {
                error!("Failed to compile {namespace}/{filename}: {:?}", err);
            }
            Ok(code) => {
                let file_root = PathBuf::from(filename).with_extension("");

                let path = gen_root
                    .join(namespace)
                    .join(file_root.with_extension("rs"));

                if !SINGLE_FILE_PER_MOD {
                    let mut file = File::create(path)?;
                    file.write_all(b"use super::*;\n")?;
                    file.write_all(code.as_bytes())?;
                }

                mods.entry(namespace)
                    .or_insert_with(Vec::new)
                    .push((file_root.to_string_lossy().to_string(), code));
            }
        }
    }

    for (modname, filenames) in &mods {
        let mut path = gen_root.clone();
        path.push(modname);
        path.push("mod.rs");

        let mut file = File::create(path)?;
        let mut filenames = filenames.clone();
        filenames.sort();
        for (name, code) in &filenames {
            let name_id = safe_identifier(name);
            if SINGLE_FILE_PER_MOD {
                file.write_all(format!("mod {name_id} {{\n\n{code}\n\n}}\n\n").as_bytes())?;
            } else {
                file.write_all(format!("mod {name_id};\n").as_bytes())?;
            }
        }

        file.write_all(b"\n")?;

        for (name, _code) in &filenames {
            let name_id = safe_identifier(name);
            file.write_all(format!("pub use {name_id}::*;\n").as_bytes())?;
        }

        file.write_all(b"\n")?;

        if !matches!(modname.as_str(), "spicelib") {
            file.write_all(b"pub use crate::generated::spicelib;\n")?;
        }
        if !matches!(modname.as_str(), "spicelib" | "support") {
            file.write_all(b"pub use crate::generated::support;\n")?;
        }
        if matches!(modname.as_str(), "tspice") {
            file.write_all(b"pub use crate::generated::testutil;\n")?;
        }
    }

    {
        let mut path = gen_root.clone();
        path.push("mod.rs");

        let mut file = File::create(path)?;
        file.write_all(b"//\n// GENERATED FILE\n//\n\n")?;
        file.write_all(b"#![allow(non_snake_case)]\n")?;
        file.write_all(b"#![allow(unused_parens, clippy::double_parens)]\n")?;
        file.write_all(b"#![allow(unused_mut, unused_assignments)]\n")?;
        file.write_all(b"#![allow(unused_imports)]\n")?;
        file.write_all(b"#![allow(unused_variables)]\n")?;
        file.write_all(b"#![allow(unreachable_code)]\n")?;
        file.write_all(b"#![allow(dead_code)]\n")?;
        file.write_all(b"#![allow(clippy::while_immutable_condition)]\n")?;
        file.write_all(b"#![allow(clippy::assign_op_pattern)]\n")?;
        file.write_all(b"#![allow(clippy::needless_return)]\n")?;
        file.write_all(b"#![allow(clippy::unnecessary_cast)]\n")?;
        file.write_all(b"#![allow(clippy::if_same_then_else)]\n")?;
        file.write_all(b"#![allow(clippy::needless_bool_assign)]\n")?;
        file.write_all(b"\n")?;

        let mut modnames = mods.keys().collect::<Vec<_>>();
        modnames.sort();
        for name in modnames {
            let name_id = safe_identifier(name);
            file.write_all(&format!("pub mod {name_id};\n").into_bytes())?;
        }
    }

    Ok(())
}
