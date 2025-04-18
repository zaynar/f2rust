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

use crate::util::{offset_1d, offset_2d, offset_3d, offset_4d, parse_bounds};
use std::ops::{Index, IndexMut, RangeBounds, RangeInclusive};
use std::slice::GetDisjointMutError;

macro_rules! define_array {
    ($dims:expr, $actual:ident, $dummy:ident, $dummy_mut:ident, $offset:ident, $index:ty,
        ($($bn:ident: $Bn:ident),+)) => {
        pub struct $actual<T: 'static> {
            data: Vec<T>,
            bounds: [(i32, i32); $dims],
        }

        pub struct $dummy<'a, T: 'static> {
            data: &'a [T],
            bounds: [(i32, i32); $dims],
        }

        pub struct $dummy_mut<'a, T: 'static> {
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
        }

        impl<'a, T> $dummy_mut<'a, T> {
            pub fn new<$($Bn: RangeBounds<i32>),+>(r: &'a mut [T], $($bn: $Bn),+) -> Self {
                let bounds = [$(parse_bounds($bn)),+];
                Self {
                    data: bounded_data_mut(&bounds, r),
                    bounds,
                }
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

        impl<T> ArrayOps<T, $index> for $actual<T> {
            fn data(&self) -> &[T] {
                &self.data
            }

            fn offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }
        }

        impl<T> ArrayOpsMut<T, $index> for $actual<T> {
            fn data_mut(&mut self) -> &mut [T] {
                &mut self.data
            }
        }

        impl<T> ArrayOps<T, $index> for $dummy<'_, T> {
            fn data(& self) -> &[T] {
                self.data
            }

            fn offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }
        }

        impl<T> ArrayOps<T, $index> for $dummy_mut<'_, T> {
            fn data(& self) -> &[T] {
                self.data
            }

            fn offset(&self, index: $index) -> usize {
                $offset(self.bounds, index)
            }
        }

        impl<T> ArrayOpsMut<T, $index> for $dummy_mut<'_, T> {
            fn data_mut(&mut self) -> &mut [T] {
                self.data
            }
        }
    }
}

define_array!(1, ActualArray, DummyArray, DummyArrayMut, offset_1d, i32, (b0: B0));
define_array!(2, ActualArray2D, DummyArray2D, DummyArrayMut2D, offset_2d, [i32; 2], (b0: B0, b1: B1));
define_array!(3, ActualArray3D, DummyArray3D, DummyArrayMut3D, offset_3d, [i32; 3], (b0: B0, b1: B1, b2: B2));
define_array!(4, ActualArray4D, DummyArray4D, DummyArrayMut4D, offset_4d, [i32; 4], (b0: B0, b1: B1, b2: B2, b3: B3));

pub trait ArrayOps<T: 'static, I> {
    fn data(&self) -> &[T];
    fn offset(&self, index: I) -> usize;

    fn first(&self) -> &T {
        self.data().first().unwrap()
    }

    fn as_slice(&self) -> &[T] {
        self.data()
    }

    fn subarray(&self, index: I) -> &[T] {
        let offset = self.offset(index);
        &self.data()[offset..]
    }

    fn iter(&self) -> impl Iterator<Item = &T> {
        self.data().iter()
    }

    fn subscript(&self, index: I) -> i32 {
        self.offset(index) as i32 + 1
    }
}

pub trait ArrayOpsMut<T: 'static, I>: ArrayOps<T, I> {
    fn data_mut(&mut self) -> &mut [T];

    fn first_mut(&mut self) -> &mut T {
        self.data_mut().first_mut().unwrap()
    }

    fn as_slice_mut(&mut self) -> &mut [T] {
        self.data_mut()
    }

    fn subarray_mut(&mut self, index: I) -> &mut [T] {
        let offset = self.offset(index);
        &mut self.data_mut()[offset..]
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data_mut().iter_mut()
    }

    fn get_disjoint_mut<const N: usize>(
        &mut self,
        indices: [I; N],
    ) -> Result<[&mut T; N], GetDisjointMutError> {
        let offsets = indices.map(|index| self.offset(index));
        self.data_mut().get_disjoint_mut(offsets)
    }

    /// Get slices starting at the given indices, each as large as possible without
    /// overlapping.
    fn get_disjoint_slices_mut<const N: usize>(
        &mut self,
        indices: [I; N],
    ) -> Result<[&mut [T]; N], GetDisjointMutError> {
        // First, sort the indices by increasing offset
        let mut offsets: Vec<_> = indices
            .into_iter()
            .enumerate()
            .map(|(i, index)| (i, self.offset(index)))
            .collect();
        offsets.sort_by_key(|(i, index)| (*index, *i));

        // Add a dummy entry to represent the end of the list
        offsets.push((0, self.data_mut().len()));

        // Replace each offset, with a range from that offset until the next one
        let mut ranges: Vec<_> = offsets
            .windows(2)
            .map(|w| (w[0].0, w[0].1..w[1].1))
            .collect();

        // Put them back into argument order
        ranges.sort_by_key(|(i, _range)| *i);

        let ranges: Vec<_> = ranges.into_iter().map(|(_i, range)| range).collect();
        self.data_mut().get_disjoint_mut(ranges.try_into().unwrap())
    }
}

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

#[test]
fn test_disjoint_mut() {
    let mut data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    let mut arr = DummyArrayMut::new(&mut data, 1..);

    assert_eq!(arr[1], 1);
    assert_eq!(arr[2], 2);

    let [d1, d5, d4] = arr.get_disjoint_mut([1, 5, 4]).unwrap();
    assert_eq!(*d1, 1);
    assert_eq!(*d5, 5);
    assert_eq!(*d4, 4);
    *d4 = 40;

    assert_eq!(arr.as_slice(), [1, 2, 3, 40, 5, 6, 7, 8, 9, 10]);

    assert_eq!(
        arr.get_disjoint_mut([1, 5, 5]),
        Err(GetDisjointMutError::OverlappingIndices)
    );
    assert_eq!(
        arr.get_disjoint_mut([1, 20, 5]),
        Err(GetDisjointMutError::IndexOutOfBounds)
    );
}

#[test]
fn test_disjoint_slice() {
    let mut data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    let mut arr = DummyArrayMut::new(&mut data, 1..);

    assert_eq!(arr[1], 1);
    assert_eq!(arr[2], 2);

    let [d1, d5, d4] = arr.get_disjoint_slices_mut([1, 5, 4]).unwrap();
    assert_eq!(d1, [1, 2, 3]);
    assert_eq!(d5, [5, 6, 7, 8, 9, 10]);
    assert_eq!(d4, [4]);
    d4[0] = 40;

    assert_eq!(arr.as_slice(), [1, 2, 3, 40, 5, 6, 7, 8, 9, 10]);

    let [d1, d5a, d5b] = arr.get_disjoint_slices_mut([1, 5, 5]).unwrap();
    assert_eq!(d1, [1, 2, 3, 40]);
    assert_eq!(d5a, []);
    assert_eq!(d5b, [5, 6, 7, 8, 9, 10]);

    assert_eq!(
        arr.get_disjoint_slices_mut([1, 20, 5]),
        Err(GetDisjointMutError::IndexOutOfBounds)
    );
}
