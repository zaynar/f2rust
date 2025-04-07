use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use anyhow::Context;
use anyhow::Result;
use f2rust_compiler::ast;
use f2rust_compiler::file::parse_free;
use f2rust_compiler::globan;

fn main() -> Result<()> {
    let out_dir = env::var_os("OUT_DIR").unwrap();

    println!("cargo::rerun-if-changed=fortran/");

    let mut generated = vec![];

    for entry in fs::read_dir("fortran")? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().unwrap_or_default().to_string_lossy() == "f90" {
            let dst = Path::new(&out_dir)
                .join(path.file_name().unwrap())
                .with_extension("rs");

            let filename = path
                .with_extension("")
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned();

            build(&path, &dst, &filename)
                .context(format!("Building {}", std::path::absolute(path)?.display()))?;

            generated.push((dst, filename));
        }
    }

    {
        let gen_path = Path::new(&out_dir).join("generated_files.rs");
        let mut file = File::create(gen_path)?;

        for (dst, _filename) in &generated {
            file.write_all(format!("include!(r\"{}\");\n", dst.display()).as_bytes())?;
        }
        file.write_all(b"fn get_generated_files() -> Vec<(&'static str, fn(&mut Context))> {\n")?;
        file.write_all(b"  vec![\n")?;
        for (_dst, filename) in &generated {
            file.write_all(format!("    (\"{filename}\", {filename}::TEST),\n").as_bytes())?;
        }
        file.write_all(b"  ]\n")?;
        file.write_all(b"}\n")?;
    }

    Ok(())
}

fn build(src: &Path, dst: &Path, filename: &str) -> Result<()> {
    let parsed = parse_free(src)?;

    let ast = ast::Parser::new().parse(parsed)?;

    let namespace = "test";

    let program_unit = globan::ProgramUnit::new(namespace, filename, ast);

    let mut glob = globan::GlobalAnalysis::new(&[], vec![program_unit]);
    glob.analyse()?;

    let code = glob.codegen(namespace, filename, false)?;

    let code = format!("mod {filename} {{\n{code}\n}}\n");

    std::fs::write(dst, code.as_bytes())?;

    Ok(())
}
