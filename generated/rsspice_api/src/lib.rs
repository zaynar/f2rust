//! Pure Rust port of the SPICE Toolkit.
//!
//! This implementation is fully memory-safe and thread-safe.
//! The code and API has been automatically translated from the FORTRAN version
//! of the SPICE Toolkit.
//!
//! Because of its FORTRAN origins, the Rust API has a number of quirks:
//!
//! ## `SpiceContext`
//!
//! Any function that involves 'global' state takes a [SpiceContext] as the
//! first argument. Multiple threads can work in parallel with separate `SpiceContext`s.
//! You can also initialise one `SpiceContext` and then `clone` it for each thread.
//!
//! ## Error handling
//!
//! We run the SPICE Toolkit in `RETURN` mode, meaning it will
//! report errors then return up the call stack. At the Rust API, we translate the
//! error into [Error] so you can use Rust's standard error handling mechanism.
//! We also reset the SPICE error state so you can continue using the API
//! (although it its internal state may have been left inconsistent after the error,
//! so you need to be careful about which errors you can correctly recover from).
//!
//! ## Array arguments
//!
//! 3D vector arguments are typically represented as `&[f64; 3]`.
//! You can conveniently use `nalgebra::Vector3` for these:
//!
//! ```
//! use rsspice_api::*;
//! use approx::assert_relative_eq;
//! use nalgebra as na;
//! let v = na::Vector3::new(1.0, 2.0, 3.0);
//! let mut r = na::Vector3::zeros();
//! vrotv(v.as_ref(), &[0.0, 0.0, 1.0], std::f64::consts::FRAC_PI_2, r.as_mut());
//! assert_relative_eq!(r, na::Vector3::new(-2.0, 1.0, 3.0));
//! ```
//!
//! If you want to use an `&[f64]` slice (e.g. from a `Vec`), you can use
//! `try_into().unwrap()` (which will panic if the slice is too small):
//!
//! ```
//! use rsspice_api::*;
//! use approx::assert_relative_eq;
//! let v = [0.0, 1.0, 2.0, 3.0, 4.0];
//! let mut r = vec![0.0; 3];
//! vrotv(&v[1..4].try_into().unwrap(),
//!     &[0.0, 0.0, 1.0],
//!     std::f64::consts::FRAC_PI_2,
//!     r.as_mut_slice().try_into().unwrap());
//! assert_relative_eq!(r.as_slice(), [-2.0, 1.0, 3.0].as_slice());
//! ```
//!
//! Matrices are represented as `&[f64; 9]` in column-major order,
//! compatible with `nalgebra::Matrix3`:
//!
//! ```
//! use rsspice_api::*;
//! use approx::assert_relative_eq;
//! use nalgebra as na;
//! let m = na::Matrix3::new(
//!     0.0, -1.0, 0.0,
//!     0.5,  0.0, 0.0,
//!     0.0,  0.0, 1.0);
//! let mut mout = na::Matrix3::zeros();
//! invert(
//!     m.as_slice().try_into().unwrap(),
//!     mout.as_mut_slice().try_into().unwrap(),
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
//! Input strings are implemented as `&str`, and behave as you would expect.
//!
//! Output strings are implemented as `&mut str`, meaning the caller must allocate
//! enough space before the call. Read the function's documentation to find the requirements.
//! If the string is too small, the output may be truncated, or you may get a bounds-check
//! panic.
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

mod raw;
pub mod required_reading;

pub struct SpiceContext<'a> {
    ctx: f2rust_std::Context<'a>,
}

impl<'a> SpiceContext<'a> {
    pub fn new() -> Self {
        Self {
            ctx: f2rust_std::Context::new(),
        }
    }

    fn raw_context(&mut self) -> &mut f2rust_std::Context<'a> {
        &mut self.ctx
    }
}

pub use raw::*;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("internal error {0}")]
    InternalError(#[from] f2rust_std::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use crate as spicelib;
    use crate::SpiceContext;
    use approx::assert_relative_eq;
    use nalgebra as na;

    #[test]
    fn vadd() {
        let v1 = [1., 2., 3.];
        let v2 = [10., 20., 30.];
        let mut vout = [0.; 3];
        spicelib::vadd(&v1, &v2, &mut vout);
        assert_eq!(vout, [11., 22., 33.]);
    }

    #[test]
    fn vrotv() {
        let v = na::Vector3::new(1.0, 2.0, 3.0);
        let mut r = na::Vector3::zeros();
        spicelib::vrotv(
            v.as_ref(),
            &[0.0, 0.0, 1.0],
            std::f64::consts::FRAC_PI_2,
            r.as_mut(),
        );
        assert_relative_eq!(r, na::Vector3::new(-2.0, 1.0, 3.0));
    }

    #[test]
    fn invert() {
        let m = na::Matrix3::new(0.0, -1.0, 0.0, 0.5, 0.0, 0.0, 0.0, 0.0, 1.0);
        let mut mout = na::Matrix3::zeros();
        spicelib::invert(
            m.as_slice().try_into().unwrap(),
            mout.as_mut_slice().try_into().unwrap(),
        );
        assert_relative_eq!(
            mout,
            na::Matrix3::new(0.0, 2.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
        );
    }

    #[test]
    fn okay_utf8() {
        let mut ctx = SpiceContext::new();

        let mut s = String::from("abcd\u{1234}");
        spicelib::shiftc(&mut ctx, &s.clone(), 'L', 1, 'x', &mut s).unwrap();
        assert_eq!(s, "bcd\u{1234}x");
        spicelib::shiftc(&mut ctx, &s.clone(), 'L', 3, 'x', &mut s).unwrap();
        assert_eq!(s, "\u{1234}xxxx");
    }

    // #[test]
    // #[should_panic]
    // fn bad_utf8() {
    //     let mut ctx = SpiceContext::new();
    //
    //     let mut s = String::from("abcd\u{1234}");
    //     spicelib::shiftc(&mut ctx, &s.clone(), 'L', 5, 'x', &mut s).unwrap();
    // }
}
