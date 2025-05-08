//! Pure Rust port of the SPICE Toolkit for space geometry.
//!
//! This implementation is fully memory-safe and thread-safe,
//! and does not depend on any external C/FORTRAN libraries.
//! It implements nearly the entire SPICELIB API.
//!
//! The code has been automatically translated from the FORTRAN version
//! of the SPICE Toolkit into Rust.
//!
//! It is completely unofficial, unsupported, and not heavily tested
//! (though it does pass the Toolkit's regression tests).
//! Use at your own risk.
//!
//! # Usage example
//!
//! This code demonstrates the general design:
//!
//! ```no_run
//! use rsspice::*;
//!
//! const TIMFMT: &str = "YYYY MON DD HR:MN:SC.###### (TDB)::TDB";
//! const MAXWIN: usize = 2 * 100;
//! const LBCELL: i32 = -5;
//!
//! // Find solar eclipses as seen from the center of the Earth.
//! fn main() -> Result<()> {
//!     let mut spice = SpiceContext::new();
//!
//!     spice.furnsh("gfoclt_ex1.tm")?;
//!
//!     let mut confine = Cell::with_capacity(2);
//!     let mut result = Cell::with_capacity(MAXWIN);
//!
//!     let et0 = spice.str2et("2027 JAN 01 00:00:00 TDB")?;
//!     let et1 = spice.str2et("2029 JAN 01 00:00:00 TDB")?;
//!
//!     spice.wninsd(et0, et1, &mut confine)?;
//!
//!     spice.gfoclt(
//!         "ANY",
//!         "MOON", "ellipsoid", "IAU_MOON",
//!         "SUN", "ellipsoid", "IAU_SUN",
//!         "LT", "EARTH", 180.0, &confine,
//!         &mut result,
//!     )?;
//!
//!     for i in 1..=spice.wncard(&result)? {
//!         let (left, right) = spice.wnfetd(&result, i)?;
//!         println!(
//!             "Interval {i}: {} - {}",
//!             spice.timout(left, TIMFMT)?,
//!             spice.timout(right, TIMFMT)?
//!         );
//!     }
//!
//!     Ok(())
//! }
//! ```
//!
//! The `SpiceContext` object encapsulates all the SPICE state, such as loaded
//! kernels. There is no process-wide global state, so you can run multiple
//! `SpiceContext`s concurrently in separate threads.
//!
//! There is a 1:1 mapping between functions in the FORTRAN API and
//! in the Rust API, so you can refer to the extensive FORTRAN documentation.
//!
//! Arguments are mapped onto standard Rust types: `i32`, `&[f64]`, `&str`, etc.
//! Output arguments are mapped onto return values. (E.g. `wnfetd`
//! returns the tuple `(left, right)`).
//! Input-output arguments are passed as `&mut` references.
//!
//! SPICE errors are mapped onto Rust's error system, so they can
//! be easily handled with `Result` and `?`.
//!
//! FORTRAN arrays are typically indexed from 1, not 0.
//! Functions like `wnfetd` similarly start counting from 1; we do not attempt
//! any automatic translation of indexes.
//! (This differs from the CSPICE port, and wrappers around CSPICE,
//! which count from 0.)
//!
//! There is a special type for SPICE cells (including windows and sets).
//! But [`Cell`] only provides very basic functionality;
//! this library is not attempting to provide an idiomatic high-level API.
//!
//! # Background
//!
//! SPICE is "an observation geometry system for space science missions",
//! developed by NASA's
//! [Navigation and Ancillary Information Facility](https://naif.jpl.nasa.gov/)
//! (NAIF).
//!
//! A large amount of geometric data about planets, moons, and spacecraft is
//! publicly available as SPICE data, which can be processed using the SPICE Toolkit
//! software and APIs. NAIF also provides a lot of documentation
//! of the system.
//!
//! The SPICE Toolkit is originally developed in FORTRAN, with an official
//! translation to C. Official and unofficial bindings for the C library
//! are available in several other languages.
//! `rsspice` is an unofficial translation from FORTRAN to Rust, with
//! a number of benefits and drawbacks:
//!
//! * Memory-safe: Rust's bounds-checking ensures that many errors will be
//! detected at runtime and will not result in data corruption.
//!
//! * Thread-safe: The FORTRAN and C implementations depend heavily on global
//! state. `rsspice` moves that state into the `SpiceContext` object, allowing
//! concurrency within a single process.
//!
//! * Portability: This should work on any platform that Rust supports,
//! including WebAssembly (albeit with some complications around filesystem access).
//!
//! * Much less testing: `rsspice` includes a translation of the TSPICE
//! regression tests, which are reasonably extensive but do not have
//! full coverage of the whole API. There is a higher risk of bugs than
//! in a wrapper around the well-tested FORTRAN/C implementations.
//!
//!
//! # API mapping
//!
//! The API is mechanically translated from the FORTRAN API, following a number
//! of rules to make it closer to idiomatic Rust:
//!
//! ## Return values
//!
//! Arguments that are documented as "O" are turned into return values.
//! If there are multiple outputs, the function will return a tuple.
//! If one of the outputs is called `FOUND`, it will not be returned explicitly;
//! instead the function will return an `Option<_>`.
//!
//! There are some exceptions, including array 'output' arguments where the
//! implementation reads the size of the provided array. In this case,
//! they are mapped onto `&mut` parameters, and you must initialise the
//! array appropriately before the call.
//!
//! ## Errors
//!
//! If a function can fail, it will return [`rsspice::Result<_>`](Result).
//! This includes failures reported through SPICE's error system,
//! plus unhandled IO errors, attempts by the FORTRAN code to call `STOP`/`EXIT`,
//! and other internal errors.
//!
//! ## Strings
//!
//! Input strings are `&str`, outputs are `String`, mixed input-output
//! are `&mut str`.
//!
//! Since FORTRAN 77 does not have dynamic allocation, output strings are
//! allocated at the maximum possible size, and FORTRAN will pad the output
//! with trailing space characters.
//! We trim the trailing spaces before returning a `String`.
//! When using `&mut str`, you are responsible for allocating and trimming.
//!
//! FORTRAN 77 barely even understands ASCII, never mind UTF-8.
//! Input strings are simply interpreted as a sequence of bytes.
//! For outputs, the implementation will panic if it ends up producing a non-UTF-8
//! string; this should not happen unless you pass non-ASCII characters into the API
//! (so don't do that).
//!
//! ## Omitted functions
//!
//! A small number of functions are excluded from the API, because they are:
//! * Private
//! * Deprecated/obsolete
//! * Documented as "DO NOT CALL THIS ROUTINE", or return a `BOGUSENTRY` error
//! * Redundant with basic Rust functionality, particularly string manipulation
//!
//! # Raw API
//!
//! An alternative version of the API is available in the [`raw`] module.
//! This is a closer match to the FORTRAN API, without the special handling
//! of return values.
//! Each `raw` function reproduces the original FORTRAN API documentation,
//! detailing every input/output argument.
//! You probably shouldn't need to use this API directly,
//! but the documentation is very helpful.
//!
//!
//! # ...
//!
//! Because of its FORTRAN origins, the Rust API has a number of quirks:
//!
//! ## `SpiceContext`
//!
//! The SPICELIB API is fundamentally designed around global state.
//! To avoid any Rust globals, we encapsulate all of that state in the [`SpiceContext`] object;
//! any API that involves 'global' state takes a `SpiceContext` as the
//! first argument. Multiple threads can run concurrently with separate `SpiceContext`s.
//!
//! ## Error handling
//!
//! We integrate [SPICELIB's error handling mechanism](required_reading::error) with Rust's.
//! Any function that is documented as signaling a `SPICE(FOO)` exception
//! will return a corresponding [`Error::FOO`](Error) from the Rust API.
//!
//! Some exceptions are recoverable: you can detect them, react appropriately,
//! and continue using the API. Other exceptions may result in an inconsistent state.
//! By default you should treat errors as fatal (or return them up the call stack
//! with `?`), and check the documentation for which ones you can safely handle.
//!
//! We also return `Error` for some other cases, including unhandled IO errors,
//! and FORTRAN code attempting to terminate the process with `STOP` or `EXIT`.
//!
//! Specifically: we run SPICELIB in `RETURN` mode, meaning it will
//! report errors then return up the call stack. Once it reaches the Rust API wrapper,
//! we `RESET` the SPICELIB error state and return the `Error`.
//! You should not use SPICELIB's error API directly, as it will likely conflict
//! with this wrapper.
//!
//! ```
//! use rsspice::*;
//! let mut ctx = SpiceContext::new();
//! assert!(matches!(raw::dacosh(&mut ctx, 0.0), Err(Error::INVALIDARGUMENT(..))));
//! // You can continue using the same ctx after catching the error
//! assert_eq!(raw::dacosh(&mut ctx, 1.0).unwrap(), 0.0);
//! ```
//!
//! ## Array arguments
//!
//! 3D vector arguments are typically represented as `&[f64; 3]`.
//! You can conveniently use `nalgebra::Vector3` for these:
//!
//! ```
//! use rsspice::*;
//! use approx::assert_relative_eq;
//! use nalgebra as na;
//! let v = na::Vector3::new(1.0, 2.0, 3.0);
//! let mut r = na::Vector3::zeros();
//! raw::vrotv(v.as_ref(), &[0.0, 0.0, 1.0], std::f64::consts::FRAC_PI_2, r.as_mut());
//! assert_relative_eq!(r, na::Vector3::new(-2.0, 1.0, 3.0));
//! ```
//!
//! If you want to use an `&[f64]` slice (e.g. from a `Vec`), you can use
//! `try_into().unwrap()` (which will panic if the slice is too small):
//!
//! ```
//! use rsspice::*;
//! use approx::assert_relative_eq;
//! let v = [0.0, 1.0, 2.0, 3.0, 4.0];
//! let mut r = vec![0.0; 3];
//! raw::vrotv(&v[1..4].try_into().unwrap(),
//!     &[0.0, 0.0, 1.0],
//!     std::f64::consts::FRAC_PI_2,
//!     r.as_mut_slice().try_into().unwrap());
//! assert_relative_eq!(r.as_slice(), [-2.0, 1.0, 3.0].as_slice());
//! ```
//!
//! Matrices are represented as `&[[f64; 3]; 3]` in column-major order,
//! compatible with `nalgebra::Matrix3`:
//!
//! ```
//! use rsspice::*;
//! use approx::assert_relative_eq;
//! use nalgebra as na;
//! let m = na::Matrix3::new(
//!     0.0, -1.0, 0.0,
//!     0.5,  0.0, 0.0,
//!     0.0,  0.0, 1.0);
//! let mut mout = na::Matrix3::zeros();
//! raw::invert(
//!     m.as_ref(),
//!     mout.as_mut(),
//! );
//! assert_relative_eq!(
//!     mout,
//!     na::Matrix3::new(
//!          0.0, 2.0, 0.0,
//!         -1.0, 0.0, 0.0,
//!          0.0, 0.0, 1.0)
//! );
//! ```
//!
//! ## Strings
//!
//! Input strings are implemented as `&str`, and typically behave as you would expect.
//!
//! Output strings are implemented as `&mut str`, meaning the caller must allocate
//! enough space before the call. Read the function's documentation to find the requirements.
//! If the string is too small, the output may be truncated, or you may get a bounds-check
//! panic.
//!
//! FORTRAN strings are padded with space characters. You should typically fill the string
//! with spaces before the call, and use `trim_ascii_end()` to remove them afterwards.
//!
//! ```
//! use rsspice::*;
//! let mut ctx = SpiceContext::new();
//! let mut calstr = " ".repeat(48); // docs say this should be >=48 characters
//! raw::etcal(&mut ctx, 0.0, &mut calstr);
//! assert_eq!(calstr.trim_ascii_end(), "2000 JAN 01 12:00:00.000");
//! ```
//!
//! FORTRAN 77 barely even understands ASCII, never mind UTF-8.
//! Input strings are simply interpreted as a sequence of bytes.
//! For output strings, the implementation will panic if it ends up producing a non-UTF-8
//! string; this should not happen unless you pass non-ASCII characters into the API
//! (so don't do that).
//!
//! ## String arrays
//!
//! In FORTRAN, arrays of strings are effectively an array of bytes plus a string length.
//! Every string in the array must have the same length.
//! Short strings must padded with space characters.
//!
//! TODO: Design/document the API for this.

mod api;
mod generated;

#[cfg(feature = "tspice")]
mod tspice;

pub use api::*;

/// Collection of reference documents describing the various SPICE subsystems
///
/// This can also be read at <https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/index.html>
pub use crate::generated::required_reading;

/// Lower-level SPICELIB API
///
/// This module provides the complete SPICELIB API, with a very similar structure to the
/// original FORTRAN API, including the full API reference documentation.
///
/// Most applications should use the [`SpiceContext`] methods instead, as they are
/// slightly more convenient.
///
/// The `raw` functions require the caller to allocate space for outputs and pass them as
/// `&mut` references. `SpiceContext` automatically allocates the space and converts output
/// arguments into return values.
///
/// Most `raw` functions still require a `SpiceContext` instance to store their
/// 'global' state.
pub use crate::generated::spicelib::api as raw;
