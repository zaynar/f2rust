//! CHARACTER arrays. These have peculiar behaviour in FORTRAN: they are stored in memory as
//! a contiguous array of bytes, split into an array of equal-length strings, but that length
//! can vary.
//!
//! If a function declares a dummy argument as `CHARACTER*(*)`, it will use the same string
//! length as the actual argument provided by the caller. But if it declares `CHARACTER*(N)`,
//! the same bytes will be reinterpreted as strings of length `N`.
//!
//! To support this, the API uses `CharArray` which wraps a `&[u8]` slice and a string length.
//! Functions using `DummyCharArray` can either adopt this length or replace it.

use crate::util::{offset_1d, offset_2d, offset_3d, offset_4d, parse_bounds};
use std::ops::{Index, IndexMut, RangeBounds, RangeInclusive};
use std::slice::GetDisjointMutError;

/// Represents any N-dimensional array of CHARACTER, in the Rust API.
pub struct CharArray<'a> {
    data: &'a [u8],
    element_length: usize,
}

impl<'a> CharArray<'a> {
    pub fn from_ref(data: &'a [u8]) -> Self {
        let element_length = data.len();
        Self {
            data,
            element_length,
        }
    }

    pub fn to_owned(&self) -> OwnedCharArray {
        OwnedCharArray {
            data: self.data.to_vec(),
            element_length: self.element_length,
        }
    }
}

/// Represents any mutable N-dimensional array of CHARACTER, in the Rust API.
pub struct CharArrayMut<'a> {
    data: &'a mut [u8],
    element_length: usize,
}

impl<'a> CharArrayMut<'a> {
    pub fn from_mut(data: &'a mut [u8]) -> Self {
        let element_length = data.len();
        Self {
            data,
            element_length,
        }
    }

    pub fn to_owned(&self) -> OwnedCharArray {
        OwnedCharArray {
            data: self.data.to_vec(),
            element_length: self.element_length,
        }
    }
}

pub struct OwnedCharArray {
    data: Vec<u8>,
    element_length: usize,
}

impl OwnedCharArray {
    pub fn as_arg(&self) -> CharArray {
        CharArray {
            data: &self.data,
            element_length: self.element_length,
        }
    }

    pub fn as_arg_mut(&mut self) -> CharArrayMut {
        CharArrayMut {
            data: &mut self.data,
            element_length: self.element_length,
        }
    }
}

macro_rules! define_array {
    ($dims:expr, $actual:ident, $dummy:ident, $dummy_mut:ident, $offset:ident, $index:ty,
        ($($bn:ident: $Bn:ident),+)) => {
        /// Implementation of CHARACTER arrays used as actual arguments,
        /// which own their data.
        pub struct $actual {
            data: Vec<u8>,
            bounds: [(i32, i32); $dims],
            element_length: usize,
        }

        /// Implementation of CHARACTER arrays used as dummy arguments,
        /// which don't own their data.
        pub struct $dummy<'a> {
            data: &'a [u8],
            bounds: [(i32, i32); $dims],
            element_length: usize,
        }

        /// Implementation of CHARACTER arrays used as dummy arguments,
        /// which don't own their data.
        pub struct $dummy_mut<'a> {
            data: &'a mut [u8],
            bounds: [(i32, i32); $dims],
            element_length: usize,
        }

        impl $actual {
            // Use RangeInclusive instead of RangeBounds, to enforce an upper limit
            pub fn new(element_length: i32, $($bn: RangeInclusive<i32>),+) -> Self {
                let bounds = [$(parse_bounds($bn)),+];
                let size = bounds
                    .iter()
                    .map(|(lower, upper)| upper - lower + 1)
                    .product::<i32>();

                let len = element_length as usize;

                Self {
                    data: vec![b' '; len * size as usize],
                    bounds,
                    element_length: len,
                }
            }
        }

        impl<'a> $dummy<'a> {
            pub fn new<$($Bn: RangeBounds<i32>),+>(
                r: CharArray<'a>,
                element_length: Option<i32>,
                $($bn: $Bn),+
            ) -> Self {
                let bounds = [$(parse_bounds($bn)),+];

                let len = element_length
                    .map(|n| n as usize)
                    .unwrap_or(r.element_length);

                if bounds.last().unwrap().1 == i32::MAX {
                    Self {
                        data: r.data,
                        bounds,
                        element_length: len,
                    }
                } else {
                    let size = bounds
                        .iter()
                        .map(|(lower, upper)| upper - lower + 1)
                        .product::<i32>();
                    Self {
                        data: &r.data[0..len * size as usize],
                        bounds,
                        element_length: len,
                    }
                }
            }
        }

        impl<'a> $dummy_mut<'a> {
            pub fn new<$($Bn: RangeBounds<i32>),+>(
                r: CharArrayMut<'a>,
                element_length: Option<i32>,
                $($bn: $Bn),+
            ) -> Self {
                let bounds = [$(parse_bounds($bn)),+];

                let len = element_length
                    .map(|n| n as usize)
                    .unwrap_or(r.element_length);

                if bounds.last().unwrap().1 == i32::MAX {
                    Self {
                        data: r.data,
                        bounds,
                        element_length: len,
                    }
                } else {
                    let size = bounds
                        .iter()
                        .map(|(lower, upper)| upper - lower + 1)
                        .product::<i32>();
                    Self {
                        data: &mut r.data[0..len * size as usize],
                        bounds,
                        element_length: len,
                    }
                }
            }
        }

        impl Index<$index> for $actual {
            type Output = [u8];

            fn index(&self, index: $index) -> &Self::Output {
                let offset = self.offset(index);
                &self.data[offset..offset + self.element_length]
            }
        }

        impl Index<$index> for $dummy<'_> {
            type Output = [u8];

            fn index(&self, index: $index) -> &Self::Output {
                let offset = self.offset(index);
                &self.data[offset..offset + self.element_length]
            }
        }

        impl Index<$index> for $dummy_mut<'_> {
            type Output = [u8];

            fn index(&self, index: $index) -> &Self::Output {
                let offset = self.offset(index);
                &self.data[offset..offset + self.element_length]
            }
        }

        impl IndexMut<$index> for $actual {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                let offset = self.offset(index);
                &mut self.data[offset..offset + self.element_length]
            }
        }

        impl IndexMut<$index> for $dummy_mut<'_> {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                let offset = self.offset(index);
                &mut self.data[offset..offset + self.element_length]
            }
        }

        impl CharArrayOps<$index> for $actual {
            fn data(&self) -> &[u8] {
                &self.data
            }

            fn element_length(&self) -> usize {
                self.element_length
            }

            fn byte_offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }
        }

        impl CharArrayOpsMut<$index> for $actual {
            fn data_mut(&mut self) -> &mut [u8] {
                &mut self.data
            }
        }

        impl CharArrayOps<$index> for $dummy<'_> {
            fn data(& self) -> &[u8] {
                self.data
            }

            fn element_length(&self) -> usize {
                self.element_length
            }

            fn byte_offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }
        }

        impl CharArrayOps<$index> for $dummy_mut<'_> {
            fn data(& self) -> &[u8] {
                self.data
            }

            fn element_length(&self) -> usize {
                self.element_length
            }

            fn byte_offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }
        }

        impl CharArrayOpsMut<$index> for $dummy_mut<'_> {
            fn data_mut(&mut self) -> &mut [u8] {
                self.data
            }
        }
    }
}

define_array!(1, ActualCharArray, DummyCharArray, DummyCharArrayMut, offset_1d, i32, (b0: B0));
define_array!(2, ActualCharArray2D, DummyCharArray2D, DummyCharArrayMut2D, offset_2d, [i32; 2], (b0: B0, b1: B1));
define_array!(3, ActualCharArray3D, DummyCharArray3D, DummyCharArrayMut3D, offset_3d, [i32; 3], (b0: B0, b1: B1, b2: B2));
define_array!(4, ActualCharArray4D, DummyCharArray4D, DummyCharArrayMut4D, offset_4d, [i32; 4], (b0: B0, b1: B1, b2: B2, b3: B3));

pub trait CharArrayOps<I> {
    fn data(&self) -> &[u8];
    fn element_length(&self) -> usize;
    fn byte_offset(&self, index: I) -> usize;

    fn offset(&self, index: I) -> usize {
        self.byte_offset(index) * self.element_length()
    }

    fn to_owned(&self) -> OwnedCharArray {
        let element_length = self.element_length();
        OwnedCharArray {
            data: self.data().to_vec(),
            element_length,
        }
    }

    fn first(&self) -> &[u8] {
        let element_length = self.element_length();
        &self.data()[0..element_length]
    }

    fn as_arg(&self) -> CharArray {
        let element_length = self.element_length();
        CharArray {
            data: self.data(),
            element_length,
        }
    }

    // We can't use Index for element access, because `a[i]` is `*a.index(i)`
    // so it has type `[u8]`, and we want &[u8] for consistency with our other
    // character types. (TODO: or we could just improve the compiler?)
    fn get(&self, index: I) -> &[u8] {
        let offset = self.offset(index);
        let element_length = self.element_length();
        &self.data()[offset..offset + element_length]
    }

    fn subarray(&self, index: I) -> CharArray {
        let offset = self.offset(index);
        let element_length = self.element_length();
        CharArray {
            data: &self.data()[offset..],
            element_length,
        }
    }

    fn subscript(&self, index: I) -> i32 {
        self.byte_offset(index) as i32 + 1
    }
}

pub trait CharArrayOpsMut<I>: CharArrayOps<I> {
    fn data_mut(&mut self) -> &mut [u8];

    fn first_mut(&mut self) -> &mut [u8] {
        let element_length = self.element_length();
        &mut self.data_mut()[0..element_length]
    }

    fn as_arg_mut(&mut self) -> CharArrayMut {
        let element_length = self.element_length();
        CharArrayMut {
            data: self.data_mut(),
            element_length,
        }
    }

    fn get_mut(&mut self, index: I) -> &mut [u8] {
        let offset = self.offset(index);
        let element_length = self.element_length();
        &mut self.data_mut()[offset..offset + element_length]
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = &mut [u8]> {
        let element_length = self.element_length();
        self.data_mut().chunks_mut(element_length)
    }

    fn subarray_mut(&mut self, index: I) -> CharArrayMut {
        let offset = self.offset(index);
        let element_length = self.element_length();
        CharArrayMut {
            data: &mut self.data_mut()[offset..],
            element_length,
        }
    }

    fn get_disjoint_mut<const N: usize>(
        &mut self,
        indices: [I; N],
    ) -> Result<[&mut [u8]; N], GetDisjointMutError> {
        let offsets = indices.map(|index| self.offset(index));
        let ranges = offsets.map(|n| n..n + self.element_length());
        self.data_mut().get_disjoint_mut(ranges)
    }
}
