//! Simple compiler CLI command, for testing.
//! This compiles a single, self-contained file (though it may use INCLUDE).

use anyhow::Result;
use clap::Parser;
use f2rust_compiler::{
    ast,
    file::{parse_fixed, parse_free},
    globan,
};
use relative_path::RelativePathBuf;
use std::path::Path;
use std::{io::Write, path::PathBuf};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// FORTRAN input file
    input: RelativePathBuf,

    /// Rust output file
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Automatic pretty-printing with rustfmt
    #[arg(short, long)]
    pretty: bool,

    /// Use free form syntax, instead of fixed form
    #[arg(short, long)]
    freeform: bool,
}

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .without_time()
        .with_target(false)
        .with_file(true)
        .with_line_number(true)
        .init();

    let cli = Cli::parse();

    let parsed = if cli.freeform {
        parse_free(&cli.input, Path::new("."), false)?
    } else {
        parse_fixed(&cli.input, Path::new("."), false)?
    };
    let ast = ast::Parser::new().parse(parsed)?;

    let namespace = "test".to_owned();
    let filename = cli.input.file_name().unwrap();

    let program_unit = globan::ProgramUnit::new(&namespace, filename, ast);

    let mut glob = globan::GlobalAnalysis::new(&[], vec![program_unit]);
    glob.analyse()?;

    let (code, _api) = glob.codegen(&namespace, filename, cli.pretty)?;

    if let Some(output) = cli.output {
        std::fs::write(output, code.as_bytes())?;
    } else {
        std::io::stdout().write_all(code.as_bytes())?;
    }

    Ok(())
}
