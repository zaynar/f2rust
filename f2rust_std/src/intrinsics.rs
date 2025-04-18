#![allow(non_snake_case)]

// TODO: implement all the rest of the intrinsics

pub fn ICHAR(a: &[u8]) -> i32 {
    a[0] as i32
}

pub fn CHAR(a: i32) -> [u8; 1] {
    [a as u8]
}

pub fn NINT(a: f32) -> i32 {
    a.round() as i32
}

pub fn IDNINT(a: f64) -> i32 {
    a.round() as i32
}

pub fn MOD(a1: i32, a2: i32) -> i32 {
    a1 % a2
}

pub fn AMOD(a1: f32, a2: f32) -> f32 {
    a1 % a2
}

pub fn DMOD(a1: f64, a2: f64) -> f64 {
    a1 % a2
}

// TODO: IDIM, DIM, DDIM
// TODO: DPROD

pub fn MAX0(n: &[i32]) -> i32 {
    n.iter().copied().reduce(i32::max).unwrap()
}

pub fn AMAX1(n: &[f32]) -> f32 {
    n.iter().copied().reduce(f32::max).unwrap()
}

pub fn DMAX1(n: &[f64]) -> f64 {
    n.iter().copied().reduce(f64::max).unwrap()
}

pub fn AMAX0(n: &[i32]) -> f32 {
    n.iter().copied().reduce(i32::max).unwrap() as f32
}

pub fn MAX1(n: &[f32]) -> i32 {
    n.iter().copied().reduce(f32::max).unwrap() as i32
}

pub fn MIN0(n: &[i32]) -> i32 {
    n.iter().copied().reduce(i32::min).unwrap()
}

pub fn AMIN1(n: &[f32]) -> f32 {
    n.iter().copied().reduce(f32::min).unwrap()
}

pub fn DMIN1(n: &[f64]) -> f64 {
    n.iter().copied().reduce(f64::min).unwrap()
}

pub fn AMIN0(n: &[i32]) -> f32 {
    n.iter().copied().reduce(i32::min).unwrap() as f32
}

pub fn MIN1(n: &[f32]) -> i32 {
    n.iter().copied().reduce(f32::min).unwrap() as i32
}

pub fn LEN(a: &[u8]) -> i32 {
    a.len() as i32
}

pub fn INDEX(a1: &[u8], a2: &[u8]) -> i32 {
    if a1.len() >= a2.len() {
        for i in 0..=a1.len() - a2.len() {
            if &a1[i..i + a2.len()] == a2 {
                return i as i32 + 1;
            }
        }
    }
    0
}

pub fn ISHFT(i: i32, shift: i32) -> i32 {
    // Fortran 90 requires SHIFT < BIT_SIZE(I), which matches Rust's
    // overflow rules for shifts, so we don't need to handle that case specially
    if shift > 0 {
        i << shift
    } else {
        ((i as u32) >> -shift) as i32
    }
}

// TODO: ISHFTC
// TODO: IBITS
// TODO: MVBITS
// TODO: BTEST
// TODO: IBSET, IBCLR

/// Implement the `**` operator for (possibly negative) integers
pub const fn pow(i1: i32, i2: i32) -> i32 {
    if i2 < 0 {
        1 / i1.pow(i2.unsigned_abs())
    } else {
        i1.pow(i2 as u32)
    }
}

pub struct Range {
    do_var: i32,
    count: i32,
    m3: i32,
}

impl Iterator for Range {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.count <= 0 {
            None
        } else {
            let ret = self.do_var;
            self.count -= 1;
            self.do_var += self.m3;
            Some(ret)
        }
    }
}

// TODO: move this, it isn't really intrinsics
pub fn range(m1: i32, m2: i32, m3: i32) -> Range {
    assert_ne!(m3, 0, "loop step must not be 0");
    Range {
        do_var: m1,
        count: (m2 - m1 + m3) / m3,
        m3,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index() {
        assert_eq!(INDEX(b"s", b"test"), 0);
        assert_eq!(INDEX(b"test", b"test"), 1);
        assert_eq!(INDEX(b"test", b"s"), 3);
        assert_eq!(INDEX(b"test", b" "), 0);
        assert_eq!(INDEX(b"xyz", b"z"), 3);
    }

    #[test]
    fn test_range() {
        assert_eq!(range(5, 10, 1).collect::<Vec<_>>(), [5, 6, 7, 8, 9, 10]);
        assert_eq!(range(5, 10, 2).collect::<Vec<_>>(), [5, 7, 9]);
        assert_eq!(range(7, 2, -1).collect::<Vec<_>>(), [7, 6, 5, 4, 3, 2]);
    }
}
