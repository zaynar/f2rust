#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;
    use std::f64;

    use f2rust_std::Context;
    use rsspice_gen::spicelib;

    #[test]
    fn vadd() {
        let v1 = [1., 2., 3., 4., 5.];
        let v2 = [10., 20., 30., 40., 50.];
        let mut vout = [0.; 5];
        spicelib::VADD(&v1, &v2, &mut vout);
        assert_eq!(vout, [11., 22., 33., 0., 0.]);
    }

    #[test]
    fn vaddg() {
        let v1 = [1., 2., 3., 4., 5.];
        let v2 = [10., 20., 30., 40., 50.];
        let mut vout = [0.; 5];
        spicelib::VADDG(&v1, &v2, vout.len() as i32, &mut vout);
        assert_eq!(vout, [11., 22., 33., 44., 55.]);
    }

    #[test]
    fn vnorm() {
        assert_eq!(spicelib::VNORM(&[0.0, 0.0, 0.0]), 0.0);
        assert_eq!(spicelib::VNORM(&[1.0, 4.0, 8.0]), 9.0);
        assert_eq!(spicelib::VNORM(&[-5.0, 10.0, 10.0]), 15.0);
    }

    #[test]
    fn vrotv() {
        let mut ret = [0.0; 3];

        spicelib::VROTV(
            &[1.0, 2.0, 3.0],
            &[0.0, 0.0, 1.0],
            f64::consts::FRAC_PI_2,
            &mut ret,
        );
        assert_abs_diff_eq!(ret.as_ref(), [-2.0, 1.0, 3.0].as_ref());

        spicelib::VROTV(
            &[1.0, 0.0, 0.0],
            &[0.0, 0.0, 1.0],
            f64::consts::FRAC_PI_2,
            &mut ret,
        );
        assert_abs_diff_eq!(ret.as_ref(), [0.0, 1.0, 0.0].as_ref());

        spicelib::VROTV(
            &[0.0, 1.0, 0.0],
            &[0.0, 0.0, 1.0],
            f64::consts::FRAC_PI_2,
            &mut ret,
        );
        assert_abs_diff_eq!(ret.as_ref(), [-1.0, 0.0, 0.0].as_ref());
    }

    #[test]
    fn benum() {
        // Documentation tests:

        for s in ["0", "21", "21994217453648"] {
            let b = s.as_bytes();
            assert!(spicelib::BEUNS(b));
            assert!(spicelib::BEINT(b));
            assert!(spicelib::BEDEC(b));
            assert!(spicelib::BENUM(b));
        }

        for s in ["+0", "-13", "+21946"] {
            let b = s.as_bytes();
            assert!(!spicelib::BEUNS(b));
            assert!(spicelib::BEINT(b));
            assert!(spicelib::BEDEC(b));
            assert!(spicelib::BENUM(b));
        }

        for s in ["1.23", "12.", ".17", "+4.1", "-.25"] {
            let b = s.as_bytes();
            assert!(!spicelib::BEUNS(b));
            assert!(!spicelib::BEINT(b));
            assert!(spicelib::BEDEC(b));
            assert!(spicelib::BENUM(b));
        }

        for s in ["2.3e17", "17.D-13275849", "-.194265E+0004"] {
            let b = s.as_bytes();
            assert!(!spicelib::BEUNS(b));
            assert!(!spicelib::BEINT(b));
            assert!(!spicelib::BEDEC(b));
            assert!(spicelib::BENUM(b));
        }

        for s in [
            "3/4",
            "37+14",
            "E12",
            "217,346.91",
            "3.14 159 264",
            "PI",
            "FIVE",
            "CXIV",
        ] {
            let b = s.as_bytes();
            assert!(!spicelib::BEUNS(b));
            assert!(!spicelib::BEINT(b));
            assert!(!spicelib::BEDEC(b));
            assert!(!spicelib::BENUM(b));
        }

        // Extra tests:

        for s in ["0 ", " 0", "    0    "] {
            let b = s.as_bytes();
            assert!(spicelib::BEUNS(b));
            assert!(spicelib::BEINT(b));
            assert!(spicelib::BEDEC(b));
            assert!(spicelib::BENUM(b));
        }

        for s in [" ", "    "] {
            let b = s.as_bytes();
            assert!(!spicelib::BEUNS(b));
            assert!(!spicelib::BEINT(b));
            assert!(!spicelib::BEDEC(b));
            assert!(!spicelib::BENUM(b));
        }
    }

    #[test]
    fn q2m() {
        let q = [
            f64::consts::SQRT_2 / 2.0,
            0.0,
            0.0,
            -f64::consts::SQRT_2 / 2.0,
        ];
        let mut r = [0.0; 9];
        spicelib::Q2M(&q, &mut r);

        assert_eq!(r, [0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0]);
    }

    #[test]
    fn ucase() {
        let mut ctx = Context::new();

        let mut out = vec![0; 256];

        spicelib::UCASE(&mut ctx, b"Hello World 1234 @ [] ` {}", &mut out);
        assert_eq!(out.trim_ascii_end(), b"HELLO WORLD 1234 @ [] ` {}");

        spicelib::UCASE(&mut ctx, b"test 2", &mut out);
        assert_eq!(out.trim_ascii_end(), b"TEST 2");
    }

    #[test]
    fn ana() {
        let mut ctx = Context::new();

        let mut out = vec![0; 256];

        spicelib::ANA(&mut ctx, b"new", b"L", &mut out);
        assert_eq!(out.trim_ascii_end(), b"a");

        spicelib::ANA(&mut ctx, b"new", b"U", &mut out);
        assert_eq!(out.trim_ascii_end(), b"A");

        spicelib::ANA(&mut ctx, b"existing", b"L", &mut out);
        assert_eq!(out.trim_ascii_end(), b"an");

        spicelib::ANA(&mut ctx, b"hour", b"L", &mut out);
        assert_eq!(out.trim_ascii_end(), b"an");

        spicelib::ANA(&mut ctx, b"once", b"L", &mut out);
        assert_eq!(out.trim_ascii_end(), b"a");
    }
}
