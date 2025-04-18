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
    debug_assert!(
        upper >= lower,
        "upper bound {upper} must be >= lower bound {lower}"
    );

    (lower, upper)
}

pub(crate) fn offset_1d(bounds: [(i32, i32); 1], s: i32) -> usize {
    debug_assert!(s >= bounds[0].0 && s <= bounds[0].1);
    (s - bounds[0].0) as usize
}

pub(crate) fn offset_2d(bounds: [(i32, i32); 2], s: [i32; 2]) -> usize {
    debug_assert!(s[0] >= bounds[0].0 && s[0] <= bounds[0].1);
    debug_assert!(s[1] >= bounds[1].0 && s[1] <= bounds[1].1);
    let n = s[1] - bounds[1].0;
    let n = s[0] - bounds[0].0 + n * (bounds[0].1 - bounds[0].0 + 1);
    n as usize
}

pub(crate) fn offset_3d(bounds: [(i32, i32); 3], s: [i32; 3]) -> usize {
    debug_assert!(s[0] >= bounds[0].0 && s[0] <= bounds[0].1);
    debug_assert!(s[1] >= bounds[1].0 && s[1] <= bounds[1].1);
    debug_assert!(s[2] >= bounds[2].0 && s[2] <= bounds[2].1);
    let n = s[2] - bounds[2].0;
    let n = s[1] - bounds[1].0 + n * (bounds[1].1 - bounds[1].0 + 1);
    let n = s[0] - bounds[0].0 + n * (bounds[0].1 - bounds[0].0 + 1);
    n as usize
}

pub(crate) fn offset_4d(bounds: [(i32, i32); 4], s: [i32; 4]) -> usize {
    debug_assert!(s[0] >= bounds[0].0 && s[0] <= bounds[0].1);
    debug_assert!(s[1] >= bounds[1].0 && s[1] <= bounds[1].1);
    debug_assert!(s[2] >= bounds[2].0 && s[2] <= bounds[2].1);
    debug_assert!(s[3] >= bounds[3].0 && s[3] <= bounds[3].1);
    let n = s[3] - bounds[3].0;
    let n = s[2] - bounds[2].0 + n * (bounds[2].1 - bounds[2].0 + 1);
    let n = s[1] - bounds[1].0 + n * (bounds[1].1 - bounds[1].0 + 1);
    let n = s[0] - bounds[0].0 + n * (bounds[0].1 - bounds[0].0 + 1);
    n as usize
}
