use crate::format;
use crate::format::{EditDescriptor, Nonrepeatable, ParsedFormatSpecIter, Repeatable};
use crate::io::RecFileRef;
use std::io::{Cursor, Write};
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

pub struct FormattedWriter<'a> {
    file: RecFileRef<'a>,

    iter: ParsedFormatSpecIter,
    awaiting: Option<Repeatable>,

    record: Option<Cursor<Vec<u8>>>,
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
            recnum,

            plus: None,
        })
    }

    fn flush(&mut self) -> crate::Result<()> {
        if let Some(record) = self.record.take() {
            self.file
                .borrow_mut()
                .write(self.recnum, &record.into_inner())?;

            if let Some(recnum) = &mut self.recnum {
                *recnum += 1;
            }
        }
        Ok(())
    }

    fn record(&mut self) -> &mut Cursor<Vec<u8>> {
        self.record.get_or_insert_with(|| Cursor::new(Vec::new()))
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
            Nonrepeatable::Char { s } => self.record().write_all(s)?,
            Nonrepeatable::T { c: _ } => todo!(),
            Nonrepeatable::TL { c: _ } => todo!(),
            Nonrepeatable::TR { c: _ } => todo!(),
            Nonrepeatable::X { n: _ } => todo!(),
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
        self.record().write_all(&str)?;

        self.continue_until_rep()
    }

    fn write_f32(&mut self, _n: f32) -> crate::Result<()> {
        todo!();
    }

    fn write_f64(&mut self, _n: f64) -> crate::Result<()> {
        todo!();
    }

    fn write_bool(&mut self, _n: bool) -> crate::Result<()> {
        todo!();
    }

    fn write_str(&mut self, str: &[u8]) -> crate::Result<()> {
        match self.awaiting.take() {
            Some(Repeatable::A { w: Some(w) }) => {
                if str.len() < w {
                    self.record().write_all(&vec![b' '; w - str.len()])?;
                    self.record().write_all(str)?;
                } else {
                    self.record().write_all(&str[0..w])?;
                }
            }
            Some(Repeatable::A { w: None }) => {
                self.record().write_all(str)?;
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

impl Writer for ListDirectedWriter<'_> {
    fn start(&mut self) -> crate::Result<()> {
        self.w.start()
    }

    fn finish(&mut self) -> crate::Result<()> {
        self.w.finish()
    }

    fn write_i32(&mut self, n: i32) -> crate::Result<()> {
        self.prev_char = false;
        self.w.record().write_all(b" ")?;

        let str = format_i(n, 11, None, None);
        self.w.record().write_all(&str)?;
        Ok(())
    }

    fn write_f32(&mut self, n: f32) -> crate::Result<()> {
        self.prev_char = false;
        self.w.record().write_all(b" ")?;

        // XXX
        let width = 16;
        let mut str = format!("{n:.8}    ");
        while str.len() < width {
            str.insert(0, ' ');
        }
        self.w.record().write_all(&str.into_bytes())?;
        Ok(())
    }

    fn write_f64(&mut self, n: f64) -> crate::Result<()> {
        self.prev_char = false;
        self.w.record().write_all(b" ")?;

        // XXX
        let width = 25;
        let mut str = format!("{n:.16}    ");
        while str.len() < width {
            str.insert(0, ' ');
        }
        self.w.record().write_all(&str.into_bytes())?;
        Ok(())
    }

    fn write_bool(&mut self, _n: bool) -> crate::Result<()> {
        todo!();
    }

    fn write_str(&mut self, str: &[u8]) -> crate::Result<()> {
        if !self.prev_char {
            self.w.record().write_all(b" ")?;
            self.prev_char = true;
        }

        self.w.record().write_all(str)?;
        Ok(())
    }
}

pub struct UnformattedWriter<'a> {
    file: RecFileRef<'a>,
    record: Option<Cursor<Vec<u8>>>,
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
            self.file
                .borrow_mut()
                .write(self.recnum, &record.into_inner())?;

            if let Some(recnum) = &mut self.recnum {
                *recnum += 1;
            }
        }
        Ok(())
    }

    fn record(&mut self) -> &mut Cursor<Vec<u8>> {
        self.record.get_or_insert_with(|| Cursor::new(Vec::new()))
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
