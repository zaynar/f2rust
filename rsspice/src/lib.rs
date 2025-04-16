#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;
    use f2rust_std::{ActualCharArray, Context, Error, Result, fstr};
    use rsspice_spicelib::spicelib;
    use tempfile::TempDir;
    // use rsspice_support::support;
    use rsspice_testutil::testutil;
    use rsspice_tspice::tspice;

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
            std::f64::consts::FRAC_PI_2,
            &mut ret,
        );
        assert_abs_diff_eq!(ret.as_ref(), [-2.0, 1.0, 3.0].as_ref());

        spicelib::VROTV(
            &[1.0, 0.0, 0.0],
            &[0.0, 0.0, 1.0],
            std::f64::consts::FRAC_PI_2,
            &mut ret,
        );
        assert_abs_diff_eq!(ret.as_ref(), [0.0, 1.0, 0.0].as_ref());

        spicelib::VROTV(
            &[0.0, 1.0, 0.0],
            &[0.0, 0.0, 1.0],
            std::f64::consts::FRAC_PI_2,
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
            std::f64::consts::SQRT_2 / 2.0,
            0.0,
            0.0,
            -std::f64::consts::SQRT_2 / 2.0,
        ];
        let mut r = [0.0; 9];
        spicelib::Q2M(&q, &mut r);

        assert_eq!(r, [0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0]);
    }

    #[test]
    fn ucase() {
        let mut ctx = Context::new();

        let mut out = vec![0; 256];

        spicelib::UCASE(b"Hello World 1234 @ [] ` {}", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"HELLO WORLD 1234 @ [] ` {}");

        spicelib::UCASE(b"test 2", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"TEST 2");
    }

    #[test]
    fn ana() {
        let mut ctx = Context::new();

        let mut out = vec![0; 256];

        spicelib::ANA(b"new", b"L", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"a");

        spicelib::ANA(b"new", b"U", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"A");

        spicelib::ANA(b"existing", b"L", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"an");

        spicelib::ANA(b"hour", b"L", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"an");

        spicelib::ANA(b"once", b"L", &mut out, &mut ctx);
        assert_eq!(out.trim_ascii_end(), b"a");
    }

    #[test]
    fn chk() -> Result<()> {
        let mut stdout = vec![];
        let mut ctx = Context::new();
        ctx.set_stdout(&mut stdout);

        spicelib::CHKIN(b"TEST", &mut ctx)?;
        spicelib::SETMSG(b"Test message.", &mut ctx);
        spicelib::ERRINT(b"#", 123, &mut ctx);
        assert!(matches!(
            spicelib::SIGERR(b"SPICE(NOTSUPPORTED)", &mut ctx),
            Err(Error::Terminated(1))
        ));

        drop(ctx);

        let str = String::from_utf8_lossy(&stdout);
        // println!("{str}");
        assert!(str.contains("SPICE(NOTSUPPORTED) --\n \nTest message."));

        Ok(())
    }

    #[test]
    fn shellc() {
        let mut input = ActualCharArray::new(10, 0..8);
        fstr::assign(&mut input[0], b"Hello");
        fstr::assign(&mut input[1], b"world");
        fstr::assign(&mut input[2], b"abc");
        fstr::assign(&mut input[3], b"defg");
        fstr::assign(&mut input[4], b"v");
        fstr::assign(&mut input[5], b"lmnopqrstu");
        fstr::assign(&mut input[6], b"wxyz");
        fstr::assign(&mut input[7], b"hijk");

        spicelib::SHELLC(8, input.as_arg_mut());

        assert!(input.iter_mut().is_sorted());
        assert_eq!(
            input.iter_mut().collect::<Vec<_>>(),
            [
                b"Hello     ",
                b"abc       ",
                b"defg      ",
                b"hijk      ",
                b"lmnopqrstu",
                b"v         ",
                b"world     ",
                b"wxyz      "
            ]
        )
    }

    #[allow(dead_code)]
    fn tspice_verbose<F>(testcase: F) -> Result<()>
    where
        F: FnOnce(&mut bool, &mut Context) -> Result<()>,
    {
        tspice_cfg(testcase, true)
    }

    fn tspice<F>(testcase: F) -> Result<()>
    where
        F: FnOnce(&mut bool, &mut Context) -> Result<()>,
    {
        tspice_cfg(testcase, false)
    }

    fn tspice_cfg<F>(testcase: F, verbose: bool) -> Result<()>
    where
        F: FnOnce(&mut bool, &mut Context) -> Result<()>,
    {
        let mut stdout = vec![];
        let mut ctx = Context::new();
        ctx.set_stdout(&mut stdout);
        let tmp = TempDir::with_prefix("rsspice-")?;

        ctx.set_cwd(tmp.path());

        if verbose {
            println!("Temp path: {}", tmp.path().display());

            // Prevent TempDir deleting the path
            let _ = tmp.into_path();
        }

        let mut cmline = if verbose {
            b"-v".to_vec()
        } else {
            b" ".to_vec()
        };
        testutil::TSETUP(
            &mut cmline,
            b"spice{0-9}{0-9}{0-9}{0-9}.log",
            b"RSSPICE 0.01",
            &mut ctx,
        )?;

        let mut ok = false;
        testcase(&mut ok, &mut ctx)?;
        assert!(ok);

        testutil::TCLOSE(&mut ctx)?;

        Ok(())
    }

    #[test]
    fn F_AAAAPHSH() -> Result<()> {
        tspice(tspice::F_AAAAPHSH)
    }

    #[test]
    fn F_AB() -> Result<()> {
        tspice(tspice::F_AB)
    }

    #[test]
    fn F_BODVAR() -> Result<()> {
        tspice(tspice::F_BODVAR)
    }

    #[test]
    fn F_CKCOV() -> Result<()> {
        tspice(tspice::F_CKCOV)
    }

    #[test]
    fn F_CKGP() -> Result<()> {
        tspice(tspice::F_CKGP)
    }

    #[test]
    fn F_ET2UTC() -> Result<()> {
        tspice(tspice::F_ET2UTC)
    }

    #[test]
    fn F_EULER() -> Result<()> {
        tspice(tspice::F_EULER)
    }

    #[test]
    fn F_M2Q() -> Result<()> {
        tspice(tspice::F_M2Q)
    }

    #[test]
    fn F_MOVED() -> Result<()> {
        tspice(tspice::F_MOVED)
    }

    #[test]
    fn F_PXFORM() -> Result<()> {
        tspice(tspice::F_PXFORM)
    }

    #[test]
    fn F_Q2M() -> Result<()> {
        tspice(tspice::F_Q2M)
    }

    #[test]
    fn F_SCLK() -> Result<()> {
        tspice(tspice::F_SCLK)
    }

    #[test]
    fn F_STR2ET() -> Result<()> {
        tspice(tspice::F_STR2ET)
    }

    #[test]
    fn F_VECTOR3() -> Result<()> {
        tspice(tspice::F_VECTOR3)
    }

    #[test]
    fn F_VECTORG() -> Result<()> {
        tspice(tspice::F_VECTORG)
    }

    #[test]
    fn F_ZZPLAT() -> Result<()> {
        tspice(tspice::F_ZZPLAT)
    }
}
