use crate::format::{EditDescriptor, Nonrepeatable, ParsedFormatSpecIter, Repeatable};
use crate::io::RecFile;
use crate::{Context, Error, format};
use std::cell::RefCell;
use std::io::{Cursor, Read, Seek};
use std::rc::Rc;

pub trait Reader {
    fn start(&mut self) -> crate::Result<()>;
    fn finish(&mut self) -> crate::Result<()>;
    fn read_i32(&mut self) -> crate::Result<i32>;
    fn read_f32(&mut self) -> crate::Result<f32>;
    fn read_f64(&mut self) -> crate::Result<f64>;
    fn read_bool(&mut self) -> crate::Result<bool>;
    fn read_str(&mut self, str: &mut [u8]) -> crate::Result<()>;
}

pub struct FormattedReader<'a> {
    file: Rc<RefCell<dyn RecFile + 'a>>,

    iter: ParsedFormatSpecIter,
    awaiting: Option<Repeatable>,

    record: Option<Cursor<Vec<u8>>>,
    recnum: Option<i32>,
}

impl<'a> FormattedReader<'a> {
    pub fn new(
        ctx: &'a mut Context,
        unit: Option<i32>,
        recnum: Option<i32>,
        fmt: &[u8],
    ) -> crate::Result<Self> {
        let file = ctx.read_unit(unit)?;
        let fmt = format::FormatParser::new(fmt).parse()?;

        Ok(Self {
            file,

            iter: fmt.into_iter(),
            awaiting: None,

            record: None,
            recnum,
        })
    }

    fn record(&mut self) -> crate::Result<&mut Cursor<Vec<u8>>> {
        if self.record.is_none() {
            self.record = Some(Cursor::new(self.file.borrow_mut().read(self.recnum)?));

            if let Some(recnum) = &mut self.recnum {
                *recnum += 1;
            }
        }
        Ok(self.record.as_mut().unwrap())
    }

    /// Read from current record into buf, filling with blanks if needed
    fn read_exact(&mut self, buf: &mut [u8]) -> crate::Result<()> {
        let read = self.record()?.read(buf)?;
        buf[read..].fill(b' ');
        Ok(())
    }

    fn seek_relative(&mut self, offset: i64) -> crate::Result<()> {
        Ok(self.record()?.seek_relative(offset)?)
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
            Nonrepeatable::Char { .. } => return Err(Error::InvalidDescOnInput),
            Nonrepeatable::T { c: _ } => todo!(),
            Nonrepeatable::TL { c: _ } => todo!(),
            Nonrepeatable::TR { c: _ } => todo!(),
            Nonrepeatable::X { n: _ } => todo!(),
            Nonrepeatable::Slash => todo!(),
            Nonrepeatable::Colon => todo!(),
            Nonrepeatable::S | Nonrepeatable::SP | Nonrepeatable::SS => (), // no effect on input
            Nonrepeatable::P { k: _ } => todo!(),
            Nonrepeatable::BN => todo!(),
            Nonrepeatable::BZ => todo!(),
            Nonrepeatable::Dollar => todo!(),
            Nonrepeatable::EndOfRecord => {
                assert!(self.record.is_some()); // TODO: is this guaranteed?
                self.record = None;
            }
        }
        Ok(())
    }
}

impl Reader for FormattedReader<'_> {
    fn start(&mut self) -> crate::Result<()> {
        self.continue_until_rep()
    }

    fn finish(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn read_i32(&mut self) -> crate::Result<i32> {
        todo!();
    }

    fn read_f32(&mut self) -> crate::Result<f32> {
        todo!();
    }

    fn read_f64(&mut self) -> crate::Result<f64> {
        todo!();
    }

    fn read_bool(&mut self) -> crate::Result<bool> {
        todo!();
    }

    fn read_str(&mut self, str: &mut [u8]) -> crate::Result<()> {
        match self.awaiting.take() {
            Some(Repeatable::A { w: Some(w) }) => {
                if str.len() < w {
                    self.seek_relative((w - str.len()) as i64)?;
                    self.read_exact(str)?;
                } else {
                    let start = str.len() - w;
                    str[0..start].fill(b' ');
                    self.read_exact(&mut str[start..])?;
                }
            }
            Some(Repeatable::A { w: None }) => {
                self.read_exact(str)?;
            }
            _ => panic!("read_str: expecting {:?}", self.awaiting),
        };

        self.continue_until_rep()
    }
}

/*
pub struct ListDirectedReader<'a> {
    file: Rc<RefCell<dyn ReadWriteSeek + 'a>>,
    prev_char: bool,
}

impl<'a> ListDirectedReader<'a> {
    pub fn new(ctx: &'a mut Context, unit: Option<i32>, _rec: Option<i32>) -> crate::Result<Self> {
        let file = ctx.read_unit(unit)?;

        Ok(Self {
            file,
            prev_char: false,
        })
    }
}

impl Reader for ListDirectedReader<'_> {
    fn start(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn finish(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn read_i32(&mut self) -> crate::Result<i32> {
        todo!()
    }

    fn read_f32(&mut self) -> crate::Result<f32> {
        todo!()
    }

    fn read_f64(&mut self) -> crate::Result<f64> {
        todo!()
    }

    fn read_bool(&mut self) -> crate::Result<bool> {
        todo!()
    }

    fn read_str(&mut self, _str: &mut [u8]) -> crate::Result<()> {
        todo!()
    }
}
*/

pub struct UnformattedReader<'a> {
    file: Rc<RefCell<dyn RecFile + 'a>>,

    record: Option<Cursor<Vec<u8>>>,
    recnum: Option<i32>,
}

impl<'a> UnformattedReader<'a> {
    pub fn new(
        ctx: &'a mut Context,
        unit: Option<i32>,
        recnum: Option<i32>,
    ) -> crate::Result<Self> {
        let file = ctx.read_unit(unit)?;

        Ok(Self {
            file,

            record: None,
            recnum,
        })
    }

    fn record(&mut self) -> crate::Result<&mut Cursor<Vec<u8>>> {
        if self.record.is_none() {
            self.record = Some(Cursor::new(self.file.borrow_mut().read(self.recnum)?));

            if let Some(recnum) = &mut self.recnum {
                *recnum += 1;
            }
        }
        Ok(self.record.as_mut().unwrap())
    }

    /// Read from current record into buf, filling with NUL bytes if needed
    fn read_exact(&mut self, buf: &mut [u8]) -> crate::Result<()> {
        let read = self.record()?.read(buf)?;
        buf[read..].fill(0);
        Ok(())
    }
}

impl Reader for UnformattedReader<'_> {
    fn start(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn finish(&mut self) -> crate::Result<()> {
        Ok(())
    }

    fn read_i32(&mut self) -> crate::Result<i32> {
        let mut buf = [0; 4];
        self.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    fn read_f32(&mut self) -> crate::Result<f32> {
        let mut buf = [0; 4];
        self.read_exact(&mut buf)?;
        Ok(f32::from_le_bytes(buf))
    }

    fn read_f64(&mut self) -> crate::Result<f64> {
        let mut buf = [0; 8];
        self.read_exact(&mut buf)?;
        Ok(f64::from_le_bytes(buf))
    }

    fn read_bool(&mut self) -> crate::Result<bool> {
        let mut buf = [0; 4];
        self.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf) != 0)
    }

    fn read_str(&mut self, str: &mut [u8]) -> crate::Result<()> {
        self.read_exact(str)
    }
}
