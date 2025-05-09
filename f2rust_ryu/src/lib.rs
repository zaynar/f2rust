//! Cut-down version of [ryu](https://crates.io/crates/ryu),
//! exposing the decimal mantissa/exponent representation.
//!
//! Inspired by [ryu_floating_decimal](https://crates.io/crates/ryu_floating_decimal),
//! but adapted from a more recent release of ryu (v1.0.20).
//!
//! Part of the [f2rust](https://github.com/zaynar/f2rust) project.

#![no_std]

mod common;
mod d2s;
#[cfg(not(feature = "small"))]
mod d2s_full_table;
mod d2s_intrinsics;
#[cfg(feature = "small")]
mod d2s_small_table;
mod f2s;
mod f2s_intrinsics;

pub use crate::d2s::FloatingDecimal64;
pub use crate::f2s::FloatingDecimal32;

pub fn d2d(val: f64) -> FloatingDecimal64 {
    let bits = val.to_bits();
    let ieee_mantissa = bits & ((1u64 << d2s::DOUBLE_MANTISSA_BITS) - 1);
    let ieee_exponent =
        (bits >> d2s::DOUBLE_MANTISSA_BITS) as u32 & ((1u32 << d2s::DOUBLE_EXPONENT_BITS) - 1);
    if ieee_mantissa == 0 && ieee_exponent == 0 {
        FloatingDecimal64 {
            mantissa: 0,
            exponent: 0,
        }
    } else {
        d2s::d2d(ieee_mantissa, ieee_exponent)
    }
}

pub fn f2d(val: f32) -> FloatingDecimal32 {
    let bits = val.to_bits();
    let ieee_mantissa = bits & ((1u32 << f2s::FLOAT_MANTISSA_BITS) - 1);
    let ieee_exponent =
        (bits >> f2s::FLOAT_MANTISSA_BITS) & ((1u32 << f2s::FLOAT_EXPONENT_BITS) - 1);
    if ieee_mantissa == 0 && ieee_exponent == 0 {
        FloatingDecimal32 {
            mantissa: 0,
            exponent: 0,
        }
    } else {
        f2s::f2d(ieee_mantissa, ieee_exponent)
    }
}
