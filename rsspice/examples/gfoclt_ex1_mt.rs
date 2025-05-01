//! GFOCLT example 1 - multithreaded
//!
//! Find occultations of the Sun by the Moon (that is, solar
//! eclipses) as seen from the center of the Earth over the month
//! December, 2001.
//!
//! Use light time corrections to model apparent positions of Sun
//! and Moon. Stellar aberration corrections are not specified
//! because they don't affect occultation computations.

use itertools::Itertools;
use rayon::prelude::*;
use rsspice::*;

const TIMFMT: &str = "YYYY MON DD HR:MN:SC.###### (TDB)::TDB";
const MAXWIN: i32 = 2 * 100;
const LBCELL: i32 = -5;

fn main() -> Result<()> {
    let mut spice = SpiceContext::new();

    spice.furnsh("gfoclt_ex1.tm")?;

    let et0 = spice.str2et("2020 JAN 01 00:00:00 TDB")?;
    let et1 = spice.str2et("2030 JAN 01 00:00:00 TDB")?;

    let num_confines = 8;

    // Split (et0, et1) into [(et0, a), (a, b), (b, c), ...]
    let confines: Vec<(f64, f64)> = (0..=num_confines)
        .map(|i| et0 + (i as f64 * (et1 - et0) / num_confines as f64))
        .tuple_windows()
        .collect();

    // Do the computations in parallel and merge the results
    let intervals: Vec<(f64, f64)> = confines
        .into_par_iter()
        .flat_map(|(et0, et1)| occults(et0, et1).unwrap())
        .collect();

    for (i, (left, right)) in intervals.into_iter().enumerate() {
        println!(
            "Interval {i}: {} - {}",
            spice.timout(left, TIMFMT)?,
            spice.timout(right, TIMFMT)?
        );
    }

    Ok(())
}

fn occults(et0: f64, et1: f64) -> Result<Vec<(f64, f64)>> {
    let mut spice = SpiceContext::new();

    spice.furnsh("gfoclt_ex1.tm")?;

    let mut confine = vec![0.0; (2 + 1 - LBCELL) as usize];
    let mut result = vec![0.0; (MAXWIN + 1 - LBCELL) as usize];

    spice.ssized(2, &mut confine)?;
    spice.ssized(MAXWIN, &mut result)?;

    spice.wninsd(et0, et1, &mut confine)?;

    spice.gfoclt(
        "ANY",
        "MOON", "ellipsoid", "IAU_MOON",
        "SUN", "ellipsoid", "IAU_SUN",
        "LT",
        "EARTH",
        180.0,
        &confine,
        &mut result,
    )?;

    (1..=spice.wncard(&result)?)
        .map(|i| spice.wnfetd(&result, i))
        .collect()
}
