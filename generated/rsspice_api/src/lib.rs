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
//! use rsspice_api::*;
//! let mut ctx = SpiceContext::new();
//! assert!(matches!(dacosh(&mut ctx, 0.0), Err(Error::INVALIDARGUMENT(..))));
//! // You can continue using the same ctx after an error
//! assert_eq!(dacosh(&mut ctx, 1.0).unwrap(), 0.0);
//! ```
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
//! use rsspice_api::*;
//! let mut ctx = SpiceContext::new();
//! let mut calstr = " ".repeat(48); // docs say this should be >=48 characters
//! etcal(&mut ctx, 0.0, &mut calstr);
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

use rsspice_spicelib::spicelib::{FAILED, GETLMS, GETSMS, RESET};

pub mod consts;
mod raw;
pub mod required_reading;

pub use raw::*;

mod errors;
pub use errors::*;

pub struct SpiceContext<'a> {
    ctx: f2rust_std::Context<'a>,
}

impl<'a> SpiceContext<'a> {
    pub fn new() -> Self {
        let mut ctx = f2rust_std::Context::new();

        // Don't print errors to stdout
        let mut list = b"NONE".to_vec();
        rsspice_spicelib::spicelib::ERRPRT(b"SET", &mut list, &mut ctx).unwrap();

        // Return errors so we can handle them nicely
        let mut action = b"RETURN".to_vec();
        rsspice_spicelib::spicelib::ERRACT(b"SET", &mut action, &mut ctx).unwrap();

        Self { ctx }
    }

    pub fn raw_context(&mut self) -> &mut f2rust_std::Context<'a> {
        &mut self.ctx
    }

    fn handle_errors(&mut self) -> Result<()> {
        if FAILED(&mut self.ctx) {
            // Get the error messages
            let mut short = vec![b' '; consts::errhnd::SMSGLN as usize];
            let mut long = vec![b' '; consts::errhnd::LMSGLN as usize];
            GETSMS(&mut short, &mut self.ctx);
            GETLMS(&mut long, &mut self.ctx);

            // Reset error state, so the user can continue after recoverable errors
            RESET(&mut self.ctx);

            // Return the error
            let short = String::from_utf8_lossy(short.trim_ascii_end());
            let long = String::from_utf8_lossy(long.trim_ascii_end());
            Err(Error::from_short(&short, &long))
        } else {
            Ok(())
        }
    }
}

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
