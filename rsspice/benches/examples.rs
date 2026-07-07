use rsspice::*;

const MAXWIN: usize = 2 * 100;

fn main() {
    divan::main();
}

#[divan::bench(sample_count = 10)]
fn gfoclt() {
    try_gfoclt().unwrap()
}

fn try_gfoclt() -> Result<()> {
    let mut spice = SpiceContext::new();

    spice.trcoff();

    spice.furnsh("testdata/spk/planets/de432s.bsp")?;
    spice.furnsh("testdata/pck/pck00011.tpc")?;
    spice.furnsh("testdata/lsk/naif0012.tls")?;

    let mut confine = Cell::with_capacity(2);
    let mut result = Cell::with_capacity(MAXWIN);

    let et0 = spice.str2et("2027 JAN 01 00:00:00 TDB")?;
    let et1 = spice.str2et("2028 JAN 01 00:00:00 TDB")?;

    spice.wninsd(et0, et1, &mut confine)?;

    spice.gfoclt(
        "ANY",
        "MOON",
        "ellipsoid",
        "IAU_MOON",
        "SUN",
        "ellipsoid",
        "IAU_SUN",
        "LT",
        "EARTH",
        180.0,
        &confine,
        &mut result,
    )?;

    for i in 1..=spice.wncard(&result)? {
        divan::black_box(spice.wnfetd(&result, i)?);
    }

    Ok(())
}
