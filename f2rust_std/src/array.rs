//! Array wrappers. These implement FORTRAN's dimension behaviour: an array is declared
//! with lower/upper bounds in each dimension, and indexes will be interpreted relative
//! to those bounds.
//!
//! 'Actual' arrays own their data. 'Dummy' arrays refer to data provided by a function
//! parameter (dummy argument); they can reinterpret the array with a different shape.
//!
//! In dummy arrays, the upper bound in the final dimension is optional.
//!
//! We implement Deref and Index for nicer syntax when accessing arrays.

use std::ops::{Deref, DerefMut, Index, IndexMut, RangeBounds};

use crate::util::{offset_1d, offset_2d, parse_bounds};

// TODO: implement N-dimensional arrays for N>2, using macros.
// (SPICE needs up to 3D.)

pub struct ActualArray<T> {
    data: Vec<T>,
    bounds: [(i32, i32); 1],
}

pub struct ActualArray2D<T> {
    data: Vec<T>,
    bounds: [(i32, i32); 2],
}

pub struct DummyArray<'a, T> {
    data: &'a [T],
    bounds: [(i32, i32); 1],
}

pub struct DummyArrayMut<'a, T> {
    data: &'a mut [T],
    bounds: [(i32, i32); 1],
}

pub struct DummyArray2D<'a, T> {
    data: &'a [T],
    bounds: [(i32, i32); 2],
}

pub struct DummyArrayMut2D<'a, T> {
    data: &'a mut [T],
    bounds: [(i32, i32); 2],
}

impl<T> ActualArray<T>
where
    T: Default + Copy,
{
    pub fn new<B0: RangeBounds<i32>>(b0: B0) -> Self {
        let bounds = [parse_bounds(b0)];
        bounds
            .iter()
            .for_each(|b| debug_assert!(b.1 != i32::MAX, "actual array must have upper bound"));
        let size = bounds
            .iter()
            .map(|(lower, upper)| upper - lower + 1)
            .product::<i32>();

        Self {
            data: vec![Default::default(); size as usize],
            bounds,
        }
    }
}

impl<T> ActualArray<T> {
    fn offset(&self, index: i32) -> usize {
        offset_1d(self.bounds, index)
    }

    pub fn first(&self) -> &T {
        self.data.first().unwrap()
    }

    pub fn first_mut(&mut self) -> &mut T {
        self.data.first_mut().unwrap()
    }

    pub fn slice(&self, index: i32) -> &[T] {
        let offset = self.offset(index);
        &self.data[offset..]
    }

    pub fn slice_mut(&mut self, index: i32) -> &mut [T] {
        let offset = self.offset(index);
        &mut self.data[offset..]
    }

    pub fn subscript(&self, index: i32) -> i32 {
        self.offset(index) as i32 + 1
    }
}

impl<T> ActualArray2D<T>
where
    T: Default + Copy,
{
    pub fn new<B0: RangeBounds<i32>, B1: RangeBounds<i32>>(b0: B0, b1: B1) -> Self {
        let bounds = [parse_bounds(b0), parse_bounds(b1)];
        bounds
            .iter()
            .for_each(|b| debug_assert!(b.1 != i32::MAX, "actual array must have upper bound"));
        let size = bounds
            .iter()
            .map(|(lower, upper)| upper - lower + 1)
            .product::<i32>();

        Self {
            data: vec![Default::default(); size as usize],
            bounds,
        }
    }
}

impl<T> ActualArray2D<T> {
    fn offset(&self, index: [i32; 2]) -> usize {
        offset_2d(self.bounds, index)
    }

    pub fn first(&self) -> &T {
        self.data.first().unwrap()
    }

    pub fn first_mut(&mut self) -> &mut T {
        self.data.first_mut().unwrap()
    }

    pub fn slice(&self, index: [i32; 2]) -> &[T] {
        let offset = self.offset(index);
        &self.data[offset..]
    }

    pub fn slice_mut(&mut self, index: [i32; 2]) -> &mut [T] {
        let offset = self.offset(index);
        &mut self.data[offset..]
    }

    pub fn subscript(&self, index: [i32; 2]) -> i32 {
        self.offset(index) as i32 + 1
    }
}

impl<'a, T> DummyArray<'a, T> {
    pub fn new<B0: RangeBounds<i32>>(r: &'a [T], b0: B0) -> Self {
        let bounds = [parse_bounds(b0)];
        if bounds.last().unwrap().1 == i32::MAX {
            Self { data: r, bounds }
        } else {
            let size = bounds
                .iter()
                .map(|(lower, upper)| upper - lower + 1)
                .product::<i32>();
            Self {
                data: &r[0..size as usize],
                bounds,
            }
        }
    }

    fn offset(&self, index: i32) -> usize {
        offset_1d(self.bounds, index)
    }

    pub fn first(&self) -> &T {
        self.data.first().unwrap()
    }

    pub fn slice(&self, index: i32) -> &[T] {
        let offset = self.offset(index);
        &self.data[offset..]
    }

    pub fn subscript(&self, index: i32) -> i32 {
        self.offset(index) as i32 + 1
    }
}

impl<'a, T> DummyArrayMut<'a, T> {
    pub fn new<B0: RangeBounds<i32>>(r: &'a mut [T], b0: B0) -> Self {
        let bounds = [parse_bounds(b0)];
        if bounds.last().unwrap().1 == i32::MAX {
            Self { data: r, bounds }
        } else {
            let size = bounds
                .iter()
                .map(|(lower, upper)| upper - lower + 1)
                .product::<i32>();
            Self {
                data: &mut r[0..size as usize],
                bounds,
            }
        }
    }

    fn offset(&self, index: i32) -> usize {
        offset_1d(self.bounds, index)
    }

    pub fn first(&self) -> &T {
        self.data.first().unwrap()
    }

    pub fn first_mut(&mut self) -> &mut T {
        self.data.first_mut().unwrap()
    }

    pub fn slice(&self, index: i32) -> &[T] {
        let offset = self.offset(index);
        &self.data[offset..]
    }

    pub fn slice_mut(&mut self, index: i32) -> &mut [T] {
        let offset = self.offset(index);
        &mut self.data[offset..]
    }

    pub fn subscript(&self, index: i32) -> i32 {
        self.offset(index) as i32 + 1
    }
}

fn bounded_data<'a, T>(bounds: &[(i32, i32)], r: &'a [T]) -> &'a [T] {
    if bounds.last().unwrap().1 == i32::MAX {
        r
    } else {
        let size = bounds
            .iter()
            .map(|(lower, upper)| upper - lower + 1)
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
            .map(|(lower, upper)| upper - lower + 1)
            .product::<i32>();
        &mut r[0..size as usize]
    }
}

impl<'a, T> DummyArray2D<'a, T> {
    pub fn new<B0: RangeBounds<i32>, B1: RangeBounds<i32>>(r: &'a [T], b0: B0, b1: B1) -> Self {
        let bounds = [parse_bounds(b0), parse_bounds(b1)];
        Self {
            data: bounded_data(&bounds, r),
            bounds,
        }
    }

    fn offset(&self, index: [i32; 2]) -> usize {
        offset_2d(self.bounds, index)
    }

    pub fn first(&self) -> &T {
        self.data.first().unwrap()
    }

    pub fn slice(&self, index: [i32; 2]) -> &[T] {
        let offset = self.offset(index);
        &self.data[offset..]
    }

    pub fn subscript(&self, index: [i32; 2]) -> i32 {
        self.offset(index) as i32 + 1
    }
}

impl<'a, T> DummyArrayMut2D<'a, T> {
    pub fn new<B0: RangeBounds<i32>, B1: RangeBounds<i32>>(r: &'a mut [T], b0: B0, b1: B1) -> Self {
        let bounds = [parse_bounds(b0), parse_bounds(b1)];
        Self {
            data: bounded_data_mut(&bounds, r),
            bounds,
        }
    }

    fn offset(&self, index: [i32; 2]) -> usize {
        offset_2d(self.bounds, index)
    }

    pub fn first(&self) -> &T {
        self.data.first().unwrap()
    }

    pub fn first_mut(&mut self) -> &mut T {
        self.data.first_mut().unwrap()
    }

    pub fn slice(&self, index: [i32; 2]) -> &[T] {
        let offset = self.offset(index);
        &self.data[offset..]
    }

    pub fn slice_mut(&mut self, index: [i32; 2]) -> &mut [T] {
        let offset = self.offset(index);
        &mut self.data[offset..]
    }

    pub fn subscript(&self, index: [i32; 2]) -> i32 {
        self.offset(index) as i32 + 1
    }
}

impl<T> Deref for DummyArray<'_, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<T> Deref for DummyArrayMut<'_, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<T> DerefMut for DummyArrayMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<T> Deref for ActualArray<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for ActualArray<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Index<i32> for ActualArray<T> {
    type Output = T;

    fn index(&self, index: i32) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl<T> Index<i32> for DummyArray<'_, T> {
    type Output = T;

    fn index(&self, index: i32) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl<T> Index<i32> for DummyArrayMut<'_, T> {
    type Output = T;

    fn index(&self, index: i32) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl<T> IndexMut<i32> for ActualArray<T> {
    fn index_mut(&mut self, index: i32) -> &mut Self::Output {
        let offset = self.offset(index);
        &mut self.data[offset]
    }
}

impl<T> IndexMut<i32> for DummyArrayMut<'_, T> {
    fn index_mut(&mut self, index: i32) -> &mut Self::Output {
        let offset = self.offset(index);
        &mut self.data[offset]
    }
}

impl<T> Deref for DummyArray2D<'_, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<T> Deref for DummyArrayMut2D<'_, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.data
    }
}

impl<T> DerefMut for DummyArrayMut2D<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data
    }
}

impl<T> Deref for ActualArray2D<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for ActualArray2D<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Index<[i32; 2]> for ActualArray2D<T> {
    type Output = T;

    fn index(&self, index: [i32; 2]) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl<T> Index<[i32; 2]> for DummyArray2D<'_, T> {
    type Output = T;

    fn index(&self, index: [i32; 2]) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl<T> Index<[i32; 2]> for DummyArrayMut2D<'_, T> {
    type Output = T;

    fn index(&self, index: [i32; 2]) -> &Self::Output {
        let offset = self.offset(index);
        &self.data[offset]
    }
}

impl<T> IndexMut<[i32; 2]> for ActualArray2D<T> {
    fn index_mut(&mut self, index: [i32; 2]) -> &mut Self::Output {
        let offset = self.offset(index);
        &mut self.data[offset]
    }
}

impl<T> IndexMut<[i32; 2]> for DummyArrayMut2D<'_, T> {
    fn index_mut(&mut self, index: [i32; 2]) -> &mut Self::Output {
        let offset = self.offset(index);
        &mut self.data[offset]
    }
}
