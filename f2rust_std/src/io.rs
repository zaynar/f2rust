use std::{cell::RefCell, iter::repeat_n, rc::Rc};

use crate::{
    Result,
    format::{
        self, EditDescriptor, Nonrepeatable, ParsedFormatSpec, ParsedFormatSpecIter, Repeatable,
    },
};

pub enum Format {
    Unformatted,
    Formatted(format::ParsedFormatSpec),
    ListDirected,
}

pub struct WriterBuilder<'a> {
    stdout: Rc<RefCell<dyn std::io::Write + 'a>>,
    unit: Option<i32>,
    fmt: Format,
    iostat: Option<&'a mut i32>,
}

pub trait Writer {
    fn start(&mut self) -> Result<()>;
    fn finish(&mut self) -> Result<()>;
    fn write_i32(&mut self, n: i32) -> Result<()>;
    fn write_f32(&mut self, n: f32) -> Result<()>;
    fn write_f64(&mut self, n: f64) -> Result<()>;
    fn write_bool(&mut self, n: bool) -> Result<()>;
    fn write_str(&mut self, str: &[u8]) -> Result<()>;
}

impl<'a> WriterBuilder<'a> {
    pub fn new(stdout: Rc<RefCell<dyn std::io::Write + 'a>>) -> Self {
        Self {
            stdout,
            unit: None,
            fmt: Format::Unformatted,
            iostat: None,
        }
    }

    pub fn unit(self, unit: i32) -> Self {
        Self {
            unit: Some(unit),
            ..self
        }
    }

    pub fn fmt(self, fmt: &[u8]) -> Result<Self> {
        let fmt = format::FormatParser::new(fmt).parse()?;

        Ok(Self {
            fmt: Format::Formatted(fmt),
            ..self
        })
    }

    pub fn fmt_list(self) -> Self {
        Self {
            fmt: Format::ListDirected,
            ..self
        }
    }

    pub fn iostat(self, iostat: &'a mut i32) -> Self {
        Self {
            iostat: Some(iostat),
            ..self
        }
    }

    pub fn build(self) -> Result<Box<dyn Writer + 'a>> {
        let file = match self.unit {
            None => self.stdout,
            Some(n) => panic!("unsupported UNIT={n}"),
        };

        let mut writer: Box<dyn Writer + 'a> = match self.fmt {
            Format::Unformatted => todo!(),
            Format::Formatted(fmt) => Box::new(FormattedWriter::new(file, fmt, self.iostat)),
            Format::ListDirected => Box::new(ListDirectedWriter::new(file, self.iostat)),
        };

        writer.start()?;
        Ok(writer)
    }
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
    file: Rc<RefCell<dyn std::io::Write + 'a>>,
    // iostat: Option<&'a mut i32>,
    awaiting: Option<Repeatable>,
    iter: ParsedFormatSpecIter,
    // We should output '\n' on finish, unless we outputted one at EndOfRecord
    // and haven't output any entries for the next record
    end_of_record: bool,

    plus: Option<bool>,
}

impl<'a> FormattedWriter<'a> {
    pub fn new(
        file: Rc<RefCell<dyn std::io::Write + 'a>>,
        fmt: ParsedFormatSpec,
        _iostat: Option<&'a mut i32>,
    ) -> Self {
        Self {
            file,
            // iostat,
            awaiting: None,
            iter: fmt.into_iter(),
            end_of_record: false,

            plus: None,
        }
    }

    fn continue_until_rep(&mut self) -> Result<()> {
        self.end_of_record = false;
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

    fn handle_nonrep(&mut self, d: &Nonrepeatable) -> std::io::Result<()> {
        match d {
            Nonrepeatable::Char { s } => self.file.borrow_mut().write_all(s)?,
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
            Nonrepeatable::BN => todo!(),
            Nonrepeatable::BZ => todo!(),
            Nonrepeatable::Dollar => todo!(),
            Nonrepeatable::EndOfRecord => {
                self.end_of_record = true;
                self.file.borrow_mut().write_all(b"\n")?;
            }
        }
        Ok(())
    }
}

impl Writer for FormattedWriter<'_> {
    fn start(&mut self) -> Result<()> {
        self.continue_until_rep()
    }

    fn finish(&mut self) -> Result<()> {
        if !self.end_of_record {
            self.file.borrow_mut().write_all(b"\n")?;
        }
        Ok(())
    }

    fn write_i32(&mut self, n: i32) -> Result<()> {
        let str = match self.awaiting.take() {
            Some(Repeatable::I { w, m }) => format_i(n, w, m, self.plus),
            _ => panic!("write_i32: expecting {:?}", self.awaiting),
        };
        self.file.borrow_mut().write_all(&str)?;

        self.continue_until_rep()
    }

    fn write_f32(&mut self, _n: f32) -> Result<()> {
        todo!();
    }

    fn write_f64(&mut self, _n: f64) -> Result<()> {
        todo!();
    }

    fn write_bool(&mut self, _n: bool) -> Result<()> {
        todo!();
    }

    fn write_str(&mut self, str: &[u8]) -> Result<()> {
        let mut file = self.file.borrow_mut();

        match self.awaiting.take() {
            Some(Repeatable::A { w: Some(w) }) => {
                if str.len() < w {
                    file.write_all(&vec![b' '; w - str.len()])?;
                    file.write_all(str)?;
                } else {
                    file.write_all(&str[0..w])?;
                }
            }
            Some(Repeatable::A { w: None }) => {
                file.write_all(str)?;
            }
            _ => panic!("write_str: expecting {:?}", self.awaiting),
        };

        drop(file);
        self.continue_until_rep()
    }
}

pub struct ListDirectedWriter<'a> {
    file: Rc<RefCell<dyn std::io::Write + 'a>>,
    // iostat: Option<&'a mut i32>,
    prev_char: bool,
}

impl<'a> ListDirectedWriter<'a> {
    pub fn new(file: Rc<RefCell<dyn std::io::Write + 'a>>, _iostat: Option<&'a mut i32>) -> Self {
        Self {
            file,
            // iostat,
            prev_char: false,
        }
    }
}

impl Writer for ListDirectedWriter<'_> {
    fn start(&mut self) -> Result<()> {
        Ok(())
    }

    fn finish(&mut self) -> Result<()> {
        self.file.borrow_mut().write_all(b"\n")?;
        Ok(())
    }

    fn write_i32(&mut self, n: i32) -> Result<()> {
        self.prev_char = false;
        self.file.borrow_mut().write_all(b" ")?;

        let str = format_i(n, 11, None, None);
        self.file.borrow_mut().write_all(&str)?;
        Ok(())
    }

    fn write_f32(&mut self, n: f32) -> Result<()> {
        self.prev_char = false;
        self.file.borrow_mut().write_all(b" ")?;

        // XXX
        let width = 16;
        let mut str = format!("{n:.8}    ");
        while str.len() < width {
            str.insert(0, ' ');
        }
        self.file.borrow_mut().write_all(&str.into_bytes())?;
        Ok(())
    }

    fn write_f64(&mut self, n: f64) -> Result<()> {
        self.prev_char = false;
        self.file.borrow_mut().write_all(b" ")?;

        // XXX
        let width = 25;
        let mut str = format!("{n:.16}    ");
        while str.len() < width {
            str.insert(0, ' ');
        }
        self.file.borrow_mut().write_all(&str.into_bytes())?;
        Ok(())
    }

    fn write_bool(&mut self, _n: bool) -> Result<()> {
        todo!();
    }

    fn write_str(&mut self, str: &[u8]) -> Result<()> {
        if !self.prev_char {
            self.file.borrow_mut().write_all(b" ")?;
            self.prev_char = true;
        }

        self.file.borrow_mut().write_all(str)?;
        Ok(())
    }
}
