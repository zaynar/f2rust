Part of the [f2rust](https://github.com/zaynar/f2rust) project.

Some tests for the compiler, where we compile small FORTRAN programs and
compare their stdout against reference output from gfortran.

To generate the reference output run `make` in the `fortran/` directory.

`build.rs` uses `f2rust_compiler` to translate each program into Rust before
building, and then `main.rs` runs the built tests.
