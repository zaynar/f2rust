//! CHARACTER arrays. These have peculiar behaviour in FORTRAN: they are stored in memory as
//! a contiguous array of bytes, split into an array of fixed-length strings, but that length
//! can vary.
//!
//! If a function declares a dummy argument as `CHARACTER*(*)`, it will use the same string
//! length as the actual argument provided by the caller. But if it declares `CHARACTER*(N)`,
//! the same bytes will be reinterpreted as strings of length `N`.
//!
//! To support this, the API uses `CharArray` which wraps a `&[u8]` slice and a string length.
//! Implementations using `DummyCharArray` can either copy this length or replace it.

use std::ops::{Index, IndexMut, RangeBounds};

use crate::util::{offset_1d, parse_bounds};

// TODO: N-dimensional arrays for N>1

/// Represents any N-dimensional array of CHARACTER, in the Rust API.
pub struct CharArray<'a> {
    data: &'a [u8],
    element_length: usize,
}

/// Represents any mutable N-dimensional array of CHARACTER, in the Rust API.
pub struct CharArrayMut<'a> {
    data: &'a mut [u8],
    element_length: usize,
}

/// Implementation of CHARACTER arrays used as actual arguments, which own their data.
pub struct ActualCharArray {
    data: Vec<u8>,
    bounds: [(i32, i32); 1],
    element_length: usize,
}

/// Implementation of CHARACTER arrays used as dummy arguments, which own their data.
pub struct DummyCharArray<'a> {
    data: &'a [u8],
    bounds: [(i32, i32); 1],
    element_length: usize,
}

pub struct DummyCharArrayMut<'a> {
    data: &'a mut [u8],
    bounds: [(i32, i32); 1],
    element_length: usize,
}

impl ActualCharArray {
    pub fn new<B0: RangeBounds<i32>>(element_length: i32, b0: B0) -> Self {
        let bounds = [parse_bounds(b0)];
        bounds
            .iter()
            .for_each(|b| debug_assert!(b.1 != i32::MAX, "actual array must have upper bound"));
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

    pub fn first(&self) -> &u8 {
        self.data.first().unwrap()
    }

    pub fn first_mut(&mut self) -> &mut u8 {
        self.data.first_mut().unwrap()
    }

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

    fn offset(&self, s: i32) -> usize {
        offset_1d(self.bounds, s) * self.element_length
    }

    pub fn get(&self, index: i32) -> &[u8] {
        let offset = self.offset(index);
        &self.data[offset..offset + self.element_length]
    }

    pub fn get_mut(&mut self, index: i32) -> &mut [u8] {
        let offset = self.offset(index);
        &mut self.data[offset..offset + self.element_length]
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut [u8]> {
        self.data.chunks_mut(self.element_length)
    }
}

impl<'a> DummyCharArray<'a> {
    // TODO: Support constructing from CharArrayMut, somehow

    pub fn new<B0: RangeBounds<i32>>(
        r: CharArray<'a>,
        element_length: Option<i32>,
        b0: B0,
    ) -> Self {
        let bounds = [parse_bounds(b0)];

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

    pub fn first(&self) -> &[u8] {
        &self.data[0..self.element_length]
    }

    pub fn as_arg(&self) -> CharArray {
        CharArray {
            data: self.data,
            element_length: self.element_length,
        }
    }

    fn offset(&self, s: i32) -> usize {
        offset_1d(self.bounds, s) * self.element_length
    }

    // We can't always use Index for these, because `a[i]` is `*a.index(i)`
    // so it has type `[u8]`, and we want &[u8] for consistency with our other
    // character types. (TODO: or we could just improve the compiler?)
    pub fn get(&self, index: i32) -> &[u8] {
        let offset = self.offset(index);
        &self.data[offset..offset + self.element_length]
    }
}

impl<'a> DummyCharArrayMut<'a> {
    pub fn new<B0: RangeBounds<i32>>(
        r: CharArrayMut<'a>,
        element_length: Option<i32>,
        b0: B0,
    ) -> Self {
        let bounds = [parse_bounds(b0)];

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

    pub fn first(&self) -> &[u8] {
        &self.data[0..self.element_length]
    }

    pub fn as_arg(&self) -> CharArray {
        CharArray {
            data: self.data,
            element_length: self.element_length,
        }
    }

    pub fn as_arg_mut(&mut self) -> CharArrayMut {
        CharArrayMut {
            data: self.data,
            element_length: self.element_length,
        }
    }

    fn offset(&self, s: i32) -> usize {
        offset_1d(self.bounds, s) * self.element_length
    }

    pub fn get(&self, index: i32) -> &[u8] {
        let offset = self.offset(index);
        &self.data[offset..offset + self.element_length]
    }

    pub fn get_mut(&mut self, index: i32) -> &mut [u8] {
        let offset = self.offset(index);
        &mut self.data[offset..offset + self.element_length]
    }
}

impl Index<i32> for ActualCharArray {
    type Output = [u8];

    fn index(&self, index: i32) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset..offset + self.element_length]
    }
}

impl Index<i32> for DummyCharArray<'_> {
    type Output = [u8];

    fn index(&self, index: i32) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset..offset + self.element_length]
    }
}

impl Index<i32> for DummyCharArrayMut<'_> {
    type Output = u8;

    fn index(&self, index: i32) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl IndexMut<i32> for ActualCharArray {
    fn index_mut(&mut self, index: i32) -> &mut Self::Output {
        let offset = self.offset(index);
        &mut self.data[offset..offset + self.element_length]
    }
}

impl IndexMut<i32> for DummyCharArrayMut<'_> {
    fn index_mut(&mut self, index: i32) -> &mut Self::Output {
        let offset = self.offset(index);
        &mut self.data[offset]
    }
}
