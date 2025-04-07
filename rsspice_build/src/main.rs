//! Compile the SPICE Toolkit's FORTRAN implementation into Rust.
//!
//! You must download tspice.tar from https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/PC_Linux_64bit/
//! and extract into the workspace directory. (Be aware of its restrictions on distribution:
//! https://naif.jpl.nasa.gov/naif/rules.html)

use std::{collections::HashMap, fs::File, io::Write, path::PathBuf, time::Instant};

use anyhow::{Context, Result, bail};
use rayon::prelude::*;
use tracing::{Level, error, info, span};
use walkdir::WalkDir;

use f2rust_compiler::{ast, file::parse_fixed, globan};

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

    let program_units = paths
        // .iter()
        .par_iter()
        .filter(|entry| {
            matches!(
                entry.path().extension().and_then(|s| s.to_str()),
                Some("f") | Some("pgm")
            )
        })
        // .filter(|entry| {
        //     !matches!(
        //         entry.path().parent().unwrap().file_name().unwrap().to_str(),
        //         Some("tspice")
        //     )
        // })
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
        .map(|entry| {
            let path = entry.path();

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
            let ast = ast::Parser::new()
                .parse(parsed)
                .context(format!("parsing {path:?}"))?;

            Ok(globan::ProgramUnit::new(&namespace, &filename, ast))
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

    let program_units: Vec<_> = program_units.into_iter().map(|pu| pu.unwrap()).collect();

    // All files
    let _sources = program_units
        .iter()
        .map(|pu| (pu.namespace.clone(), pu.filename.clone()))
        .collect::<Vec<_>>();

    // All spicelib files
    let _sources = program_units
        .iter()
        .filter_map(|pu| {
            if pu.namespace == "spicelib" {
                Some((pu.namespace.clone(), pu.filename.clone()))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // Some files that currently work
    let sources = [
        ("spicelib", "vrotv.f"),
        ("spicelib", "vadd.f"),
        ("spicelib", "vaddg.f"),
        ("spicelib", "vnorm.f"),
        ("spicelib", "vhat.f"),
        ("spicelib", "vproj.f"),
        ("spicelib", "vsub.f"),
        ("spicelib", "vcrss.f"),
        ("spicelib", "vlcom.f"),
        ("spicelib", "vdot.f"),
        ("spicelib", "vscl.f"),
        ("spicelib", "moved.f"),
        //
        // ("spicelib", "cyadip.f"),
        ("spicelib", "pos.f"),
        ("spicelib", "cpos.f"),
        ("spicelib", "beint.f"),
        ("spicelib", "beuns.f"),
        ("spicelib", "benum.f"),
        ("spicelib", "bedec.f"),
        ("spicelib", "frstnb.f"),
        //
        ("spicelib", "q2m.f"),
        //
        ("spicelib", "ana.f"),
        ("spicelib", "ucase.f"),
        ("spicelib", "replch.f"),
        ("spicelib", "ljust.f"),
        ("spicelib", "isrchc.f"),
        // ("support", "m2core.f"),
        //
        // ("spicelib", "sclu01.f"),
    ];

    let mut glob = globan::GlobalAnalysis::new(&["spicelib", "support", "testutil"], program_units);
    glob.analyse()?;

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
        file.write_all(b"#![allow(clippy::while_immutable_condition)]\n")?;
        file.write_all(b"#![allow(clippy::assign_op_pattern)]\n")?;
        file.write_all(b"#![allow(clippy::needless_return)]\n")?;
        file.write_all(b"#![allow(clippy::unnecessary_cast)]\n")?;
        file.write_all(b"#![allow(clippy::if_same_then_else)]\n")?;
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
