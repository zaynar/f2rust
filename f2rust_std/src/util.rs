//! Internal utility functions

use std::ops::RangeBounds;

pub(crate) fn parse_bounds<B: RangeBounds<i32>>(b: B) -> (i32, i32) {
    let lower = match b.start_bound() {
        std::ops::Bound::Included(n) => *n,
        std::ops::Bound::Excluded(n) => *n + 1,
        std::ops::Bound::Unbounded => 1,
    };
    let upper = match b.end_bound() {
        std::ops::Bound::Included(n) => *n,
        std::ops::Bound::Excluded(n) => *n - 1,
        std::ops::Bound::Unbounded => i32::MAX,
    };
    debug_assert!(lower <= upper, "upper bound cannot be below lower bound");

    (lower, upper)
}

pub(crate) fn offset_1d(bounds: [(i32, i32); 1], s: i32) -> usize {
    debug_assert!(s >= bounds[0].0 && s <= bounds[0].1);
    (s - bounds[0].0) as usize
}

pub(crate) fn offset_2d(bounds: [(i32, i32); 2], s: [i32; 2]) -> usize {
    debug_assert!(s[0] >= bounds[0].0 && s[0] <= bounds[0].1);
    debug_assert!(s[1] >= bounds[1].0 && s[1] <= bounds[1].1);
    ((s[0] - bounds[0].0) + (s[1] - bounds[1].0) * (bounds[0].1 - bounds[0].0 + 1)) as usize
}
