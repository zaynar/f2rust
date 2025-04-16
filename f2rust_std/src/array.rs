//! Array wrappers. These implement FORTRAN's dimension behaviour: an array is declared
//! with lower/upper bounds in each dimension, and indexes will be interpreted relative
//! to those bounds.
//!
//! 'Actual' arrays own their data. 'Dummy' arrays refer to data provided by a function
//! parameter (dummy argument); they can reinterpret the array with a different shape.
//!
//! In dummy arrays, the upper bound in the final dimension is optional.
//!
//! We implement Index for nicer syntax when accessing arrays.

use std::ops::{Index, IndexMut, RangeBounds, RangeInclusive};

use crate::util::{offset_1d, offset_2d, offset_3d, parse_bounds};

macro_rules! define_array {
    ($dims:expr, $actual:ident, $dummy:ident, $dummy_mut:ident, $offset:ident, $index:ty,
        ($($bn:ident: $Bn:ident),+)) => {
        pub struct $actual<T> {
            data: Vec<T>,
            bounds: [(i32, i32); $dims],
        }

        pub struct $dummy<'a, T> {
            data: &'a [T],
            bounds: [(i32, i32); $dims],
        }

        pub struct $dummy_mut<'a, T> {
            data: &'a mut [T],
            bounds: [(i32, i32); $dims],
        }

        impl<T> $actual<T>
        where
            T: Default + Copy,
        {
            pub fn new($($bn: RangeInclusive<i32>),+) -> Self {
                let bounds = [$(parse_bounds($bn)),+];
                let size = bounds
                    .iter()
                    .map(|(lower, upper)| (upper - lower + 1).max(0))
                    .product::<i32>();

                Self {
                    data: vec![Default::default(); size as usize],
                    bounds,
                }
            }
        }

        impl<T> $actual<T> {
            fn offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }

            pub fn first(&self) -> &T {
                self.data.first().unwrap()
            }

            pub fn first_mut(&mut self) -> &mut T {
                self.data.first_mut().unwrap()
            }

            pub fn as_slice(&self) -> &[T] {
                &self.data
            }

            pub fn as_slice_mut(&mut self) -> &mut [T] {
                &mut self.data
            }

            pub fn subarray(&self, index: $index) -> &[T] {
                let offset = self.offset(index);
                &self.data[offset..]
            }

            pub fn subarray_mut(&mut self, index: $index) -> &mut [T] {
                let offset = self.offset(index);
                &mut self.data[offset..]
            }

            pub fn iter(&self) -> impl Iterator<Item = &T> {
                self.data.iter()
            }

            pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
                self.data.iter_mut()
            }

            pub fn subscript(&self, index: $index) -> i32 {
                self.offset(index) as i32 + 1
            }

            pub fn get_disjoint_mut_unwrap<const N: usize>(&mut self, indices: [$index; N]) -> [&mut T; N] {
                let offsets = indices.map(|index| self.offset(index));
                self.data.get_disjoint_mut(offsets).expect("mutable array elements passed to function must have disjoint indexes")
            }
        }

        impl<'a, T> $dummy<'a, T> {
            // We need a separate generic RangeBounds for each dimension,
            // because we use a mixture of RangeInclusive and RangeFrom
            pub fn new<$($Bn: RangeBounds<i32>),+>(r: &'a [T], $($bn: $Bn),+) -> Self {
                let bounds = [$(parse_bounds($bn)),+];
                Self {
                    data: bounded_data(&bounds, r),
                    bounds,
                }
            }

            fn offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }

            pub fn first(&self) -> &T {
                self.data.first().unwrap()
            }

            pub fn as_slice(&self) -> &[T] {
                &self.data
            }

            pub fn subarray(&self, index: $index) -> &[T] {
                let offset = self.offset(index);
                &self.data[offset..]
            }

            pub fn iter(&self) -> impl Iterator<Item = &T> {
                self.data.iter()
            }

            pub fn subscript(&self, index: $index) -> i32 {
                self.offset(index) as i32 + 1
            }
        }

        impl<'a, T> $dummy_mut<'a, T> {
            pub fn new<$($Bn: RangeBounds<i32>),+>(r: &'a mut [T], $($bn: $Bn),+) -> Self {
                let bounds = [$(parse_bounds($bn)),+];
                Self {
                    data: bounded_data_mut(&bounds, r),
                    bounds,
                }
            }

            fn offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }

            pub fn first(&self) -> &T {
                self.data.first().unwrap()
            }

            pub fn first_mut(&mut self) -> &mut T {
                self.data.first_mut().unwrap()
            }

            pub fn as_slice(&self) -> &[T] {
                &self.data
            }

            pub fn as_slice_mut(&mut self) -> &mut [T] {
                &mut self.data
            }

            pub fn subarray(&self, index: $index) -> &[T] {
                let offset = self.offset(index);
                &self.data[offset..]
            }

            pub fn subarray_mut(&mut self, index: $index) -> &mut [T] {
                let offset = self.offset(index);
                &mut self.data[offset..]
            }

            pub fn iter(&self) -> impl Iterator<Item = &T> {
                self.data.iter()
            }

            pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
                self.data.iter_mut()
            }

            pub fn subscript(&self, index: $index) -> i32 {
                self.offset(index) as i32 + 1
            }

            pub fn get_disjoint_mut_unwrap<const N: usize>(&mut self, indices: [$index; N]) -> [&mut T; N] {
                let offsets = indices.map(|index| self.offset(index));
                self.data.get_disjoint_mut(offsets).expect("mutable array elements passed to function must have disjoint indexes")
            }
        }

        impl<T> Index<$index> for $actual<T> {
            type Output = T;

            fn index(&self, index: $index) -> &Self::Output {
                let offset = self.offset(index);
                &self.data[offset]
            }
        }

        impl<T> Index<$index> for $dummy<'_, T> {
            type Output = T;

            fn index(&self, index: $index) -> &Self::Output {
                let offset = self.offset(index);
                &self.data[offset]
            }
        }

        impl<T> Index<$index> for $dummy_mut<'_, T> {
            type Output = T;

            fn index(&self, index: $index) -> &Self::Output {
                let offset = self.offset(index);
                &self.data[offset]
            }
        }

        impl<T> IndexMut<$index> for $actual<T> {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                let offset = self.offset(index);
                &mut self.data[offset]
            }
        }

        impl<T> IndexMut<$index> for $dummy_mut<'_, T> {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                let offset = self.offset(index);
                &mut self.data[offset]
            }
        }
    }
}

define_array!(1, ActualArray, DummyArray, DummyArrayMut, offset_1d, i32, (b0: B0));
define_array!(2, ActualArray2D, DummyArray2D, DummyArrayMut2D, offset_2d, [i32; 2], (b0: B0, b1: B1));
define_array!(3, ActualArray3D, DummyArray3D, DummyArrayMut3D, offset_3d, [i32; 3], (b0: B0, b1: B1, b2: B2));

impl<'a, T> DummyArray<'a, T>
where
    T: bytemuck::Pod,
{
    pub fn from_equiv<S: bytemuck::Pod, B0: RangeBounds<i32>>(orig: &'a [S], b0: B0) -> Self {
        Self::new(bytemuck::must_cast_slice(orig), b0)
    }
}

impl<'a, T> DummyArrayMut<'a, T>
where
    T: bytemuck::Pod,
{
    pub fn from_equiv<S: bytemuck::Pod, B0: RangeBounds<i32>>(orig: &'a mut [S], b0: B0) -> Self {
        Self::new(bytemuck::must_cast_slice_mut(orig), b0)
    }
}

fn bounded_data<'a, T>(bounds: &[(i32, i32)], r: &'a [T]) -> &'a [T] {
    if bounds.last().unwrap().1 == i32::MAX {
        r
    } else {
        let size = bounds
            .iter()
            .map(|(lower, upper)| (upper - lower + 1).max(0))
            .product::<i32>();
        &r[0..size as usize]
    }
}

fn bounded_data_mut<'a, T>(bounds: &[(i32, i32)], r: &'a mut [T]) -> &'a mut [T] {
    if bounds.last().unwrap().1 == i32::MAX {
        r
    } else {
        let size = bounds
            .iter()
            .map(|(lower, upper)| (upper - lower + 1).max(0))
            .product::<i32>();
        &mut r[0..size as usize]
    }
}

#[test]
fn test_equivalence_array() {
    let mut orig: ActualArray<f64> = ActualArray::new(1..=2);
    orig[1] = 1.0;
    orig[2] = 2.0;

    let mut ints = DummyArrayMut::<i32>::from_equiv(orig.as_slice_mut(), 1..);

    assert_eq!(ints[1], 0x00000000);
    assert_eq!(ints[2], 0x3ff00000);
    assert_eq!(ints[3], 0x00000000);
    assert_eq!(ints[4], 0x40000000);

    ints[2] = 0x40000000;
    ints[4] = 0x3ff00000;

    assert_eq!(orig[1], 2.0);
    assert_eq!(orig[2], 1.0);
}

#[test]
fn test_equivalence_val() {
    let mut orig: f64 = 1.0;

    let mut ints = DummyArrayMut::<i32>::from_equiv(std::slice::from_mut(&mut orig), 1..);

    assert_eq!(ints[1], 0x00000000);
    assert_eq!(ints[2], 0x3ff00000);

    ints[2] = 0x40000000;

    assert_eq!(orig, 2.0);
}
