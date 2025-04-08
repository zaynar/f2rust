//! FORTRAN string operations.
//!
//! Strings are conceptually padded with blanks (spaces) before comparing and assigning.

use std::{cmp::Ordering, ops::RangeBounds};

/// For use in `const s: &[u8; N] = &fstr::extend_const::<N>("Hello world")`,
/// converting an arbitrary-length string into the required length by padding with blank.
///
/// If N is too small, this will give a compile-time error.
pub const fn extend_const<const N: usize>(s: &[u8]) -> [u8; N] {
    let mut ret = [b' '; N];
    let mut i = 0;
    while i < s.len() {
        ret[i] = s[i];
        i += 1;
    }
    ret
}

/// Blank-extended string comparison
fn cmp(a1: &[u8], a2: &[u8]) -> Ordering {
    // Compare the prefix normally. If equal, compare the rest of the longer string against blanks
    let p = usize::min(a1.len(), a2.len());

    a1[..p].cmp(&a2[..p]).then_with(|| {
        if a1.len() < a2.len() {
            for c2 in &a2[p..] {
                match b' '.cmp(c2) {
                    r @ (Ordering::Less | Ordering::Greater) => return r,
                    Ordering::Equal => (),
                }
            }
        } else {
            for c1 in &a1[p..] {
                match c1.cmp(&b' ') {
                    r @ (Ordering::Less | Ordering::Greater) => return r,
                    Ordering::Equal => (),
                }
            }
        }
        Ordering::Equal
    })
}

pub fn eq(a1: &[u8], a2: &[u8]) -> bool {
    cmp(a1, a2).is_eq()
}

pub fn ne(a1: &[u8], a2: &[u8]) -> bool {
    cmp(a1, a2).is_ne()
}

pub fn lt(a1: &[u8], a2: &[u8]) -> bool {
    cmp(a1, a2).is_lt()
}

pub fn le(a1: &[u8], a2: &[u8]) -> bool {
    cmp(a1, a2).is_le()
}

pub fn ge(a1: &[u8], a2: &[u8]) -> bool {
    cmp(a1, a2).is_ge()
}

pub fn gt(a1: &[u8], a2: &[u8]) -> bool {
    cmp(a1, a2).is_gt()
}

fn substr_bounds<R: RangeBounds<i32>>(range: R) -> (usize, Option<usize>) {
    let lower = match range.start_bound() {
        std::ops::Bound::Included(n) => *n,
        std::ops::Bound::Excluded(n) => *n + 1,
        std::ops::Bound::Unbounded => 1,
    };
    assert!(lower >= 1, "substring lower bound must be positive");

    let upper = match range.end_bound() {
        std::ops::Bound::Included(n) => Some(*n),
        std::ops::Bound::Excluded(n) => Some(*n - 1),
        std::ops::Bound::Unbounded => None,
    };

    if let Some(upper) = upper {
        assert!(lower <= upper, "substring must have non-zero positive size");
    }

    (lower as usize - 1, upper.map(|n| n as usize - 1))
}

pub fn substr<R: RangeBounds<i32>>(a: &[u8], range: R) -> &[u8] {
    match substr_bounds(range) {
        (lower, None) => &a[lower..],
        (lower, Some(upper)) => &a[lower..=upper],
    }
}

pub fn substr_mut<R: RangeBounds<i32>>(a: &mut [u8], range: R) -> &mut [u8] {
    match substr_bounds(range) {
        (lower, None) => &mut a[lower..],
        (lower, Some(upper)) => &mut a[lower..=upper],
    }
}

pub fn assign(a1: &mut [u8], a2: &[u8]) {
    let p = usize::min(a1.len(), a2.len());
    a1[..p].copy_from_slice(&a2[..p]);
    a1[p..].fill(b' ');
}

pub fn concat(a1: &[u8], a2: &[u8]) -> Vec<u8> {
    let mut r = a1.to_vec();
    r.extend(a2);
    r
}

#[cfg(test)]
mod tests {
    use crate::fstr;

    #[test]
    fn eq() {
        assert!(fstr::eq(b"test", b"test"));
        assert!(fstr::eq(b"test   ", b"test"));
        assert!(fstr::eq(b"test", b"test   "));
        assert!(!fstr::eq(b"test", b" test"));

        assert!(!fstr::ne(b"test", b"test"));
        assert!(!fstr::ne(b"test   ", b"test"));
        assert!(!fstr::ne(b"test", b"test   "));
        assert!(fstr::ne(b"test", b" test"));
    }

    #[test]
    fn substr() {
        assert_eq!(fstr::substr(b"test", 2..=3), b"es");
        assert_eq!(fstr::substr(b"test", 2..), b"est");
    }

    #[test]
    fn cmp() {
        assert!(!fstr::lt(b"test", b"test"));
        assert!(fstr::le(b"test", b"test"));
        assert!(fstr::ge(b"test", b"test"));
        assert!(!fstr::gt(b"test", b"test"));

        assert!(!fstr::lt(b"test   ", b"test"));
        assert!(fstr::le(b"test   ", b"test"));
        assert!(fstr::ge(b"test   ", b"test"));
        assert!(!fstr::gt(b"test   ", b"test"));

        assert!(!fstr::lt(b"test", b"test   "));
        assert!(fstr::le(b"test", b"test   "));
        assert!(fstr::ge(b"test", b"test   "));
        assert!(!fstr::gt(b"test", b"test   "));

        assert!(fstr::lt(b"A", b"B"));
        assert!(fstr::le(b"A", b"B"));
        assert!(!fstr::ge(b"A", b"B"));
        assert!(!fstr::gt(b"A", b"B"));

        assert!(!fstr::lt(b"B", b"A"));
        assert!(!fstr::le(b"B", b"A"));
        assert!(fstr::ge(b"B", b"A"));
        assert!(fstr::gt(b"B", b"A"));
    }
}
