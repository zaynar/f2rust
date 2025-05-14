use crate::format;
use crate::format::{EditDescriptor, Nonrepeatable, ParsedFormatSpecIter, Repeatable};
use crate::io::RecFileRef;
use std::io::{SeekFrom, Write};
use std::iter::repeat_n;

pub trait Writer {
    fn start(&mut self) -> crate::Result<()>;
    fn finish(&mut self) -> crate::Result<()>;
    fn write_i32(&mut self, n: i32) -> crate::Result<()>;
    fn write_f32(&mut self, n: f32) -> crate::Result<()>;
    fn write_f64(&mut self, n: f64) -> crate::Result<()>;
    fn write_bool(&mut self, n: bool) -> crate::Result<()>;
    fn write_str(&mut self, str: &[u8]) -> crate::Result<()>;
}

fn overflow(w: usize) -> Vec<u8> {
    vec![b'*'; w]
}

fn format_i(n: i32, w: usize, m: Option<usize>, plus: Option<bool>) -> Vec<u8> {
    if n == 0 && m == Some(0) {
        vec![b' '; w]
    } else {
        let unsigned = n.abs().to_string().into_bytes();

        let sign: &[u8] = if n < 0 {
            b"-"
        } else if plus == Some(true) {
            b"+"
        } else {
            b""
        };

        let leading_zeroes = if let Some(m) = m {
            m.saturating_sub(unsigned.len())
        } else {
            0
        };

        let len = sign.len() + leading_zeroes + unsigned.len();
        if len > w {
            overflow(w)
        } else {
            let mut v: Vec<u8> = Vec::with_capacity(w);
            v.extend(repeat_n(b' ', w - len));
            v.extend(sign);
            v.extend(repeat_n(b'0', leading_zeroes));
            v.extend(unsigned);
            v
        }
    }
}

fn format_edf_nonfinite(n: f64, w: usize) -> Option<Vec<u8>> {
    let mut out = String::new();

    // Match gfortran's behaviour for non-finite numbers
    if n.is_infinite() {
        if n < 0.0 {
            out += "-";
        }
        if out.len() + 8 <= w {
            out += "Infinity";
        } else if out.len() + 3 <= w {
            out += "Inf";
        } else {
            return Some(overflow(w));
        }
    } else if n.is_nan() {
        if out.len() + 3 <= w {
            out += "NaN";
        } else {
            return Some(overflow(w));
        }
    } else {
        return None;
    }

    Some(Vec::from_iter(
        repeat_n(b' ', w - out.len()).chain(out.into_bytes()),
    ))
}

// TODO: This needs a lot more testing
fn format_f(n: f64, w: usize, d: usize, plus: Option<bool>) -> Vec<u8> {
    if let Some(r) = format_edf_nonfinite(n, w) {
        return r;
    }

    let mut out = String::new();

    let dec = f2rust_ryu::d2d(n);

    // dec represents n as m*10^e
    //
    // d is the desired number of fractional digits, after the decimal point.
    //
    // If the number is <1.0, there is an optional "0" before the "."
    // (unless there are no other digits, in which case it's mandatory).
    // gfortran always includes this, *unless* the output would exceed width w,
    // in which case it omits the "0".
    //
    // Some cases to consider:
    //
    //  m=12345 e=0 d=4 =>  "12345.0000"
    //  m=12345 e=1 d=4 => "123450.0000" - these are easy, just output 'm' then some 0s
    //
    //  m=12345 e=-1 d=4 =>  "1234.5000" - just need to put the '.' and '0's in the right place
    //  m=12345 e=-5 d=4 =>     "0.1235" - we need to round the number, and truncate

    if n.signum() < 0.0 {
        out += "-";
    } else if plus.unwrap_or(false) {
        out += "+";
    }

    if dec.exponent >= 0 {
        // Output the mantissa, *unless* this number is 0 in which case the leading 0 is
        // omitted if it's not the only digit and would otherwise cause an overflow
        if dec.mantissa != 0 || d == 0 || (dec.mantissa == 0 && out.len() + 2 + d <= w) {
            out += &dec.mantissa.to_string();
        }

        if out.len() + dec.exponent as usize + 1 + d > w {
            return overflow(w);
        } else {
            out.extend(repeat_n('0', dec.exponent as usize));
            out.push('.');
            out.extend(repeat_n('0', d));
        }
    } else {
        let nexp = (-dec.exponent) as usize;

        // Round away from zero
        let rounded_man = if nexp > d {
            dec.mantissa + 5 * u64::pow(10, (nexp - d) as u32 - 1)
        } else {
            dec.mantissa
        };

        let digits = rounded_man.to_string();
        if digits.len() > nexp {
            // Last `-dec.exponent` digits are the fraction
            let (integer, fraction) = digits.split_at(digits.len() - nexp);

            out += integer;
            out.push('.');

            if out.len() + d > w {
                return overflow(w);
            } else {
                if fraction.len() > d {
                    out += &fraction[0..d];
                } else {
                    out += fraction;
                    out.extend(repeat_n('0', d - fraction.len()));
                }
            }
        } else {
            // Integer part is 0
            let fraction = digits;

            // Add the optional `0.` unless that would cause us to overflow,
            // unless it's the only digit in which case it's mandatory
            if out.len() + 2 + d <= w || d == 0 {
                out.push('0');
            }
            out.push('.');

            if out.len() + d > w {
                return overflow(w);
            } else if nexp - fraction.len() >= d {
                out.extend(repeat_n('0', d));
            } else {
                let fill = nexp - fraction.len();
                out.extend(repeat_n('0', fill));

                if fraction.len() > d - fill {
                    out += &fraction[0..d - fill];
                } else {
                    out += &fraction;
                    out.extend(repeat_n('0', d - fill - fraction.len()));
                }
            }
        }
    }

    Vec::from_iter(repeat_n(b' ', w - out.len()).chain(out.into_bytes()))
}

fn format_ed(
    echar: char,
    n: f64,
    w: usize,
    d: usize,
    e: Option<usize>,
    plus: Option<bool>,
) -> Vec<u8> {
    if e.is_some() {
        todo!("Ew.dEe not supported");
    }

    if let Some(r) = format_edf_nonfinite(n, w) {
        return r;
    }

    let dec = f2rust_ryu::d2d(n);

    let mut out = String::new();

    if n.signum() < 0.0 {
        out += "-";
    } else if plus.unwrap_or(false) {
        out += "+";
    }

    // Round away from zero
    let man_digits = dec
        .mantissa
        .checked_ilog10()
        .and_then(|i| Some(i + 1))
        .unwrap_or(0);
    let rounded_man = if man_digits > d as u32 {
        dec.mantissa + 5 * u64::pow(10, man_digits - d as u32 - 1)
    } else {
        dec.mantissa
    };

    let fraction = rounded_man.to_string();

    let exp = if dec.mantissa == 0 {
        format!("{echar}+00")
    } else {
        let e = dec.exponent + fraction.len() as i32;
        if e.abs() <= 99 {
            format!("{echar}{e:+03}")
        } else {
            format!("{echar}{e:+04}")
        }
    };

    // Add the optional `0.` unless that would cause us to overflow
    if out.len() + 2 + d + exp.len() <= w {
        out.push('0');
    }
    out.push('.');

    if out.len() + d + exp.len() > w {
        return overflow(w);
    } else {
        if fraction.len() > d {
            out += &fraction[0..d];
        } else {
            out += &fraction;
            out.extend(repeat_n('0', d - fraction.len()));
        }
    }

    out += &exp;

    Vec::from_iter(repeat_n(b' ', w - out.len()).chain(out.into_bytes()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_f() {
        for (n, s) in [
            (0.0, "    0.0000"),
            (1.0, "    1.0000"),
            (50.0, "   50.0000"),
            (88000.0, "88000.0000"),
            (888000.0, "**********"),
            (-50.0, "  -50.0000"),
            (1.1, "    1.1000"),
            (0.11, "    0.1100"),
            (0.011, "    0.0110"),
            (0.0011, "    0.0011"),
            (0.00011, "    0.0001"),
            (0.000011, "    0.0000"),
            (0.0000011, "    0.0000"),
            (5.55554, "    5.5555"),
            (5.55555, "    5.5556"),
            (0.00554, "    0.0055"),
            (0.00555, "    0.0056"),
            (-5.55554, "   -5.5555"),
            (-5.55555, "   -5.5556"),
            (-0.00554, "   -0.0055"),
            (-0.00555, "   -0.0056"),
            (-0.0, "   -0.0000"),
        ] {
            assert_eq!(String::from_utf8_lossy(&format_f(n, 10, 4, None)), s);
        }

        for (n, w, d, s) in [
            (0.1, 4, 1, " 0.1"),
            (0.1, 3, 1, "0.1"),
            (0.1, 2, 1, ".1"),
            (0.1, 1, 1, "*"),
            (0.001, 4, 1, " 0.0"),
            (0.001, 3, 1, "0.0"),
            (0.001, 2, 1, ".0"),
            (0.001, 1, 1, "*"),
            (0.001, 3, 0, " 0."),
            (0.001, 2, 0, "0."),
            (0.001, 1, 0, "*"),
            (0.0, 4, 1, " 0.0"),
            (0.0, 3, 1, "0.0"),
            (0.0, 2, 1, ".0"),
            (0.0, 1, 1, "*"),
            (0.0, 3, 0, " 0."),
            (0.0, 2, 0, "0."),
            (0.0, 1, 0, "*"),
            (9.94, 7, 1, "    9.9"),
            (9.95, 7, 1, "   10.0"),
            (99999.94, 7, 1, "99999.9"),
            (99999.95, 7, 1, "*******"),
        ] {
            assert_eq!(String::from_utf8_lossy(&format_f(n, w, d, None)), s);
        }

        assert_eq!(
            String::from_utf8_lossy(&format_f(50.0, 10, 4, Some(true))),
            "  +50.0000"
        );
    }

    #[test]
    fn test_e() {
        for (n, s) in [
            (0.0, "  0.0000E+00"),
            (1.0, "  0.1000E+01"),
            (-1.0, " -0.1000E+01"),
            (0.01, "  0.1000E-01"),
            (50.0, "  0.5000E+02"),
            (0.5e90, "  0.5000E+90"),
            (0.5e100, " 0.5000E+100"),
            (0.5e-90, "  0.5000E-90"),
            (0.5e-100, " 0.5000E-100"),
            (0.55554, "  0.5555E+00"),
            (0.55555, "  0.5556E+00"),
            (-0.55554, " -0.5555E+00"),
            (-0.55555, " -0.5556E+00"),
            (0.99999, "  0.1000E+01"),
            (-0.0, " -0.0000E+00"),
        ] {
            assert_eq!(
                String::from_utf8_lossy(&format_ed('E', n, 12, 4, None, None)),
                s
            );
        }

        for (n, w, d, s) in [
            (0.1, 8, 1, " 0.1E+00"),
            (0.1, 7, 1, "0.1E+00"),
            (0.1, 6, 1, ".1E+00"),
            (0.1, 5, 1, "*****"),
            (0.0, 8, 1, " 0.0E+00"),
            (0.0, 7, 1, "0.0E+00"),
            (0.0, 6, 1, ".0E+00"),
            (0.0, 5, 1, "*****"),
            (-0.1, 8, 1, "-0.1E+00"),
            (-0.1, 7, 1, "-.1E+00"),
            (-0.1, 6, 1, "******"),
        ] {
            assert_eq!(
                String::from_utf8_lossy(&format_ed('E', n, w, d, None, None)),
                s
            );
        }
    }

    #[test]
    fn test_nonfinite() {
        for (n, w, d, s) in [
            (f64::INFINITY, 10, 1, "  Infinity"),
            (f64::INFINITY, 8, 1, "Infinity"),
            (f64::INFINITY, 7, 1, "    Inf"),
            (f64::INFINITY, 2, 1, "**"),
            (f64::NEG_INFINITY, 9, 1, "-Infinity"),
            (f64::NEG_INFINITY, 8, 1, "    -Inf"),
            (f64::NEG_INFINITY, 3, 1, "***"),
            (f64::NAN, 4, 1, " NaN"),
            (f64::NAN, 3, 1, "NaN"),
            (f64::NAN, 2, 1, "**"),
        ] {
            assert_eq!(String::from_utf8_lossy(&format_f(n, w, d, None)), s);
            assert_eq!(
                String::from_utf8_lossy(&format_ed('E', n, w, d, None, None)),
                s
            );
        }
    }
}

pub struct FormattedWriter<'a> {
    file: RecFileRef<'a>,

    iter: ParsedFormatSpecIter,
    awaiting: Option<Repeatable>,

    record: Option<Vec<u8>>,
    pos: i64,
    recnum: Option<i32>,

    plus: Option<bool>,
}

impl<'a> FormattedWriter<'a> {
    pub fn new(file: RecFileRef<'a>, recnum: Option<i32>, fmt: &[u8]) -> crate::Result<Self> {
        let fmt = format::FormatParser::new(fmt).parse()?;

        Ok(Self {
            file,

            iter: fmt.into_iter(),
            awaiting: None,

            record: None,
            pos: 0,
            recnum,

            plus: None,
        })
    }

    fn flush(&mut self) -> crate::Result<()> {
        if let Some(record) = self.record.take() {
            self.file.borrow_mut().write(self.recnum, &record)?;

            if let Some(recnum) = &mut self.recnum {
                *recnum += 1;
            }
        }

        self.pos = 0;

        Ok(())
    }

    fn write_all(&mut self, buf: &[u8]) {
        let pos = self.pos as usize;
        let record = self.record.get_or_insert_with(Vec::new);

        if pos == record.len() {
            record.extend(buf);
        } else {
            // Inserting in the middle or off the end. Fill any unused space with blanks
            if pos + buf.len() > record.len() {
                record.resize(pos + buf.len(), b' ');
            }

            record[pos..pos + buf.len()].copy_from_slice(buf);
        }

        self.pos += buf.len() as i64;
    }

    fn seek(&mut self, pos: SeekFrom) {
        match pos {
            SeekFrom::Current(n) => self.pos = (self.pos + n).max(0),
            SeekFrom::Start(n) => self.pos = n as i64,
            SeekFrom::End(_n) => panic!("not supported"),
        }
    }

    fn continue_until_rep(&mut self) -> crate::Result<()> {
        loop {
            match self.iter.next().unwrap() {
                EditDescriptor::Repeatable(d) => {
                    self.awaiting = Some(d);
                    return Ok(());
                }
                EditDescriptor::Nonrepeatable(d) => {
                    self.handle_nonrep(&d)?;
                }
            }
        }
    }

    fn handle_nonrep(&mut self, d: &Nonrepeatable) -> crate::Result<()> {
        match d {
            Nonrepeatable::Char { s } => self.write_all(s),
            Nonrepeatable::T { c: _ } => todo!(),
            Nonrepeatable::TL { c: _ } => todo!(),
            Nonrepeatable::TR { c: _ } => todo!(),
            Nonrepeatable::X { n } => self.seek(SeekFrom::Current(*n as i64)),
            Nonrepeatable::Slash => todo!(),
            Nonrepeatable::Colon => todo!(),
            Nonrepeatable::S => self.plus = None,
            Nonrepeatable::SP => self.plus = Some(true),
            Nonrepeatable::SS => self.plus = Some(false),
            Nonrepeatable::P { k: _ } => todo!(),
            Nonrepeatable::BN | Nonrepeatable::BZ => (), // no effect on output
            Nonrepeatable::Dollar => todo!(),
            Nonrepeatable::EndOfRecord => {
                self.flush()?;
            }
        }
        Ok(())
    }
}

impl Writer for FormattedWriter<'_> {
    fn start(&mut self) -> crate::Result<()> {
        self.continue_until_rep()
    }

    fn finish(&mut self) -> crate::Result<()> {
        self.flush()
    }

    fn write_i32(&mut self, n: i32) -> crate::Result<()> {
        let str = match self.awaiting.take() {
            Some(Repeatable::I { w, m }) => format_i(n, w, m, self.plus),
            _ => panic!("write_i32: expecting {:?}", self.awaiting),
        };
        self.write_all(&str);

        self.continue_until_rep()
    }

    fn write_f32(&mut self, _n: f32) -> crate::Result<()> {
        todo!();
    }

    fn write_f64(&mut self, n: f64) -> crate::Result<()> {
        let str = match self.awaiting.take() {
            Some(Repeatable::F { w, d }) => format_f(n, w, d, self.plus),
            Some(Repeatable::E { w, d, e }) => format_ed('E', n, w, d, e, self.plus),
            Some(Repeatable::D { w, d }) => format_ed('D', n, w, d, None, self.plus),
            Some(Repeatable::G { .. }) => todo!(),
            _ => panic!("write_f64: expecting {:?}", self.awaiting),
        };
        self.write_all(&str);

        self.continue_until_rep()
    }

    fn write_bool(&mut self, _n: bool) -> crate::Result<()> {
        todo!();
    }

    fn write_str(&mut self, str: &[u8]) -> crate::Result<()> {
        match self.awaiting.take() {
            Some(Repeatable::A { w: Some(w) }) => {
                if str.len() < w {
                    self.write_all(&vec![b' '; w - str.len()]);
                    self.write_all(str);
                } else {
                    self.write_all(&str[0..w]);
                }
            }
            Some(Repeatable::A { w: None }) => {
                self.write_all(str);
            }
            _ => panic!("write_str: expecting {:?}", self.awaiting),
        };

        self.continue_until_rep()
    }
}

pub struct ListDirectedWriter<'a> {
    w: FormattedWriter<'a>,

    prev_char: bool,
}

impl<'a> ListDirectedWriter<'a> {
    pub fn new(file: RecFileRef<'a>, recnum: Option<i32>) -> crate::Result<Self> {
        Ok(Self {
            w: FormattedWriter::new(file, recnum, b"(A)")?,
            prev_char: false,
        })
    }
}

// Outputs in (roughly) the same style as gfortran, to simplify testing
impl Writer for ListDirectedWriter<'_> {
    fn start(&mut self) -> crate::Result<()> {
        self.w.start()
    }

    fn finish(&mut self) -> crate::Result<()> {
        self.w.finish()
    }

    fn write_i32(&mut self, n: i32) -> crate::Result<()> {
        self.prev_char = false;
        self.w.write_all(b" ");

        let str = format_i(n, 11, None, None);
        self.w.write_all(&str);
        Ok(())
    }

    fn write_f32(&mut self, n: f32) -> crate::Result<()> {
        self.prev_char = false;
        self.w.write_all(b" ");
        self.w.write_all(&format_f(n as f64, 12, 8, None));
        self.w.write_all(b"    ");
        Ok(())
    }

    fn write_f64(&mut self, n: f64) -> crate::Result<()> {
        self.prev_char = false;
        self.w.write_all(b" ");
        self.w.write_all(&format_f(n, 21, 16, None));
        self.w.write_all(b"    ");
        Ok(())
    }

    fn write_bool(&mut self, _n: bool) -> crate::Result<()> {
        todo!();
    }

    fn write_str(&mut self, str: &[u8]) -> crate::Result<()> {
        if !self.prev_char {
            self.w.write_all(b" ");
            self.prev_char = true;
        }

        self.w.write_all(str);
        Ok(())
    }
}

pub struct UnformattedWriter<'a> {
    file: RecFileRef<'a>,
    record: Option<Vec<u8>>,
    recnum: Option<i32>,
}

impl<'a> UnformattedWriter<'a> {
    pub fn new(file: RecFileRef<'a>, recnum: Option<i32>) -> crate::Result<Self> {
        Ok(Self {
            file,
            record: None,
            recnum,
        })
    }

    fn flush(&mut self) -> crate::Result<()> {
        if let Some(record) = self.record.take() {
            self.file.borrow_mut().write(self.recnum, &record)?;

            if let Some(recnum) = &mut self.recnum {
                *recnum += 1;
            }
        }
        Ok(())
    }

    fn record(&mut self) -> &mut Vec<u8> {
        self.record.get_or_insert_with(Vec::new)
    }
}

impl Writer for UnformattedWriter<'_> {
    fn start(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn finish(&mut self) -> crate::Result<()> {
        self.flush()
    }

    fn write_i32(&mut self, n: i32) -> crate::Result<()> {
        Ok(self.record().write_all(&n.to_le_bytes())?)
    }

    fn write_f32(&mut self, n: f32) -> crate::Result<()> {
        Ok(self.record().write_all(&n.to_le_bytes())?)
    }

    fn write_f64(&mut self, n: f64) -> crate::Result<()> {
        Ok(self.record().write_all(&n.to_le_bytes())?)
    }

    fn write_bool(&mut self, b: bool) -> crate::Result<()> {
        let n: u32 = if b { 1 } else { 0 };
        Ok(self.record().write_all(&n.to_le_bytes())?)
    }

    fn write_str(&mut self, str: &[u8]) -> crate::Result<()> {
        Ok(self.record().write_all(str)?)
    }
}
