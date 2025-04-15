# f2rust

A partial FORTRAN 77 to Rust compiler.

Current status: Basic proof-of-concept, very incomplete.

As a simple example, it translates this FORTRAN code:

```fortran
      SUBROUTINE VADDG ( V1, V2, NDIM, VOUT )
      IMPLICIT NONE
      INTEGER             NDIM
      DOUBLE PRECISION    V1   ( NDIM )
      DOUBLE PRECISION    V2   ( NDIM )
      DOUBLE PRECISION    VOUT ( NDIM )
      INTEGER             I
      DO I = 1, NDIM
         VOUT(I) = V1(I) + V2(I)
      END DO
      RETURN
      END
```

into this Rust code:

```rust
pub fn VADDG(V1: &[f64], V2: &[f64], NDIM: i32, VOUT: &mut [f64]) {
    let V1 = DummyArray::new(V1, 1..=NDIM);
    let V2 = DummyArray::new(V2, 1..=NDIM);
    let mut VOUT = DummyArrayMut::new(VOUT, 1..=NDIM);

    for I in 1..=NDIM {
        VOUT[I] = (V1[I] + V2[I]);
    }
}
```

## Running tests

```sh
cargo test -p f2rust_compiler
cargo test -p f2rust_std
cargo run --bin f2rust_test

# To translate the SPICE Toolkit:
# First, read SPICE's distribution rules at https://naif.jpl.nasa.gov/naif/rules.html
wget https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/PC_Linux_64bit/tspice.tar
tar xf tspice.tar

cargo run --bin rsspice_build
# The generated code will go into rsspice_gen/
cargo test -p rsspice
```

## Goals

The main goals for this project include:

* Support enough FORTRAN to translate the [SPICE Toolkit](https://naif.jpl.nasa.gov/naif/toolkit.html)
into pure Rust.

* Reasonably idiomatic Rust APIs:
appropriate use of pass-by-value and pass-by-`&mut`-reference,
arrays are passed as `&[T]` slices, strings are `&[u8]`, etc.
This reduces the need for a custom wrapper around the translated code.

* No `unsafe`. (Not counting the use of `bytemuck` for `EQUIVALENCE`.)

* Thread-safety. `SAVE` variables are implemented with a `Context` argument
that is passed between functions, instead of using `static` or any other global state.
You can have multiple `Context`s to allow concurrency across multiple threads.

* Preserve the structure of the original FORTRAN code, as far as possible,
to simplify manual review of the generated code.

Non-goals:

* Idiomatic Rust code. We do alright with the low-level syntax - e.g. simple loops are turned
into `for I in 1..=N { ... }` - but we never do any higher-level modifications
like changing array code from FORTRAN's typical 1-indexed to Rust's 0-indexed.

* Performance. In particular we do more heap allocation than is really needed,
for convenience.

* Full FORTRAN 77 language support. Some features could be easily added if there
was demand for them; others would likely be quite tricky.

* Any support for Fortran 90 or newer. A few features are included when needed
for compatibility, but the implementation is designed around F77.

* Error handling. Some invalid FORTRAN code may be detected and rejected,
but not necessarily with helpful error messages.
Some invalid FORTRAN may be accepted and result in invalid or buggy Rust.
The assumption is that the input has already been validated by a real compiler,
and that it doesn't rely on undefined or non-standard behaviour.

* Guaranteed correctness. This is all pretty ad hoc;
don't rely on it for anything mission-critical.
If you want to be sure the Rust code's behaviour matches the FORTRAN's,
you'll have to run your own tests on it.

## Workspace layout

* `f2rust_compiler`: The whole compiler. Primarily a library,
but also builds a command-line executable for basic testing.

* `f2rust_std`: The standard library used by generated code: contains the intrinsics,
string routines, dimensioned array types, `Context`, etc.

* `f2rust_test`: Integration tests for the compiler.
These mainly consist of FORTRAN code that prints some output;
we run these with `gfortran` to generate the expected output,
and compare the output from the translated Rust version.

* `rsspice_build`: Drives the compiler to generate the SPICE code.

* `rsspice_gen`: The generated SPICE code.

* `rsspice`: The public API for the generated code.
(Currently no actual API, just some unit tests.)
