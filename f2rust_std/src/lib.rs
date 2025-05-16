//! Standard library for FORTRAN-to-Rust translated programs.
//!
//! Part of the [f2rust](https://github.com/zaynar/f2rust) project.
//!
//! This includes some support for FORTRAN's formatted IO, but only the features
//! needed for implementing the [SPICE Toolkit](https://github.com/zaynar/rsspice).
//! It is not currently designed to work with any other FORTRAN programs.

#![allow(clippy::new_without_default)]
#![allow(clippy::collapsible_else_if)]

mod array;
mod chararray;
mod context;
pub mod data;
mod errors;
mod format;
pub mod fstr;
pub mod intrinsics;
pub mod io;
mod util;

pub use array::*;
pub use chararray::*;
pub use context::*;
pub use errors::*;
