mod writer;
pub use writer::*;

use crate::{Error, Result, fstr};
use std::collections::HashMap;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::{cell::RefCell, rc::Rc};

pub fn capture_iostat<F: FnOnce() -> Result<()>>(f: F) -> Result<i32> {
    match f() {
        Ok(()) => Ok(0),
        Err(Error::IO(err)) => Ok(err.raw_os_error().unwrap_or(i32::MAX)),
        // TODO: return negative value for EOF
        Err(e) => Err(e),
    }
}

#[derive(Default)]
pub struct InquireSpecs<'a> {
    pub unit: Option<i32>,
    pub file: Option<&'a [u8]>,
    pub exist: Option<&'a mut bool>,
    pub opened: Option<&'a mut bool>,
    pub number: Option<&'a mut i32>,
    pub named: Option<&'a mut bool>,
    pub name: Option<&'a mut [u8]>,
    // pub access: Option<&'a mut [u8]>,
    // pub sequential: Option<&'a mut [u8]>,
    // pub direct: Option<&'a mut [u8]>,
    // pub form: Option<&'a mut [u8]>,
    // pub formatted: Option<&'a mut [u8]>,
    // pub unformatted: Option<&'a mut [u8]>,
    // pub recl: Option<&'a mut i32>,
    // pub nextrec: Option<&'a mut i32>,
    // pub blank: Option<&'a mut [u8]>,
}

#[derive(Default)]
pub struct OpenSpecs<'a> {
    pub unit: Option<i32>,
    pub file: Option<&'a [u8]>,
    pub status: Option<&'a [u8]>,
    pub access: Option<&'a [u8]>,
    pub form: Option<&'a [u8]>,
    pub recl: Option<i32>,
    // pub blank: Option<&'a [u8]>,
}

#[derive(Default)]
pub struct CloseSpecs<'a> {
    pub unit: Option<i32>,
    pub status: Option<&'a [u8]>,
}

pub trait ReadWriteSeek: Read + Write + Seek {
    // fn as_read(&mut self) -> &mut dyn Read;
    // fn as_write(&mut self) -> &mut dyn Write;
    // fn as_seek(&mut self) -> &mut dyn Seek;
}
impl<T> ReadWriteSeek for T
where
    T: Read + Write + Seek,
{
    // fn as_read(&mut self) -> &mut dyn Read {
    //     self
    // }
    // fn as_write(&mut self) -> &mut dyn Write {
    //     self
    //     }
    // fn as_seek(&mut self) -> &mut dyn Seek {
    //     self
    // }
}

struct StdoutUnit<'a> {
    writer: Box<dyn Write + 'a>,
}

impl Read for StdoutUnit<'_> {
    fn read(&mut self, _buf: &mut [u8]) -> std::io::Result<usize> {
        panic!("cannot read from stdout");
    }
}

impl Write for StdoutUnit<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

impl Seek for StdoutUnit<'_> {
    fn seek(&mut self, _pos: SeekFrom) -> std::io::Result<u64> {
        panic!("cannot seek in stdout");
    }
}

struct Unit<'a> {
    stream: Rc<RefCell<dyn ReadWriteSeek + 'a>>,
}

impl<'a> Unit<'a> {
    fn new<S: ReadWriteSeek + 'a>(stream: S) -> Self {
        Self {
            stream: Rc::new(RefCell::new(stream)),
        }
    }
}

pub struct FileManager<'a> {
    units: HashMap<i32, Unit<'a>>,
}

impl<'a> FileManager<'a> {
    pub fn new() -> Self {
        Self {
            units: HashMap::from([
                // (5, StdinUnit {}),
                (
                    6,
                    Unit::new(StdoutUnit {
                        writer: Box::new(std::io::stdout()),
                    }),
                ),
            ]),
        }
    }

    fn io_unit(&mut self, unit: i32) -> Result<Rc<RefCell<dyn ReadWriteSeek + 'a>>> {
        match self.units.get(&unit) {
            None => panic!("TODO: report missing unit"),
            Some(u) => Ok(Rc::clone(&u.stream)),
        }
    }

    pub fn read_unit(&mut self, unit: Option<i32>) -> Result<Rc<RefCell<dyn ReadWriteSeek + 'a>>> {
        self.io_unit(unit.unwrap_or(5))
    }

    pub fn write_unit(&mut self, unit: Option<i32>) -> Result<Rc<RefCell<dyn ReadWriteSeek + 'a>>> {
        self.io_unit(unit.unwrap_or(6))
    }

    pub fn set_stdout<W: Write + 'a>(&mut self, stdout: W) {
        self.units.insert(
            6,
            Unit::new(StdoutUnit {
                writer: Box::new(stdout),
            }),
        );
    }

    pub fn inquire(&mut self, specs: InquireSpecs) -> Result<()> {
        if let Some(file) = specs.file {
            let path = PathBuf::from(
                std::str::from_utf8(file.trim_ascii_end()).map_err(|_| Error::NonUnicodePath)?,
            );
            if path.try_exists()? {
                if let Some(v) = specs.exist {
                    *v = true;
                }

                if let Some(v) = specs.named {
                    *v = true;
                }
                if let Some(v) = specs.name {
                    fstr::assign(
                        v,
                        path.as_os_str()
                            .to_str()
                            .ok_or(Error::NonUnicodePath)?
                            .as_bytes(),
                    );
                }

                // TODO: sequential, direct, formatted, unformatted

                let opened = false; // TODO
                if let Some(v) = specs.opened {
                    *v = opened;
                }
                if opened {
                    if let Some(v) = specs.number {
                        *v = 0;
                    }
                    // TODO: access, form, recl, nextrec, blank
                }
            } else {
                if let Some(v) = specs.exist {
                    *v = false;
                }
            }
        } else if let Some(unit) = specs.unit {
            if let Some(v) = specs.exist {
                // We support unlimited units, so any valid number exists
                *v = unit >= 0;
            }

            let opened = false;
            if let Some(v) = specs.opened {
                *v = opened;
            }
            if opened {
                // TODO: number, named, name, access, sequential, direct, form,
                // formatted unformatted, recl, nextrec, blank
            }
        } else {
            // codegen should prevent this case
            panic!("INQUIRE must have either FILE or UNIT");
        }

        Ok(())
    }

    pub fn open(&mut self, specs: OpenSpecs) -> Result<()> {
        let unit = specs.unit.expect("OPEN must have UNIT");

        // println!(
        //     "OPEN {unit} file={:?} status={:?} access={:?} form={:?} recl={:?}",
        //     specs.file.map(|f| std::str::from_utf8(f)),
        //     specs.status.map(|f| std::str::from_utf8(f)),
        //     specs.access.map(|f| std::str::from_utf8(f)),
        //     specs.form.map(|f| std::str::from_utf8(f)),
        //     specs.recl,
        // );

        if self.units.contains_key(&unit) {
            panic!("TODO: OPEN of already-open unit");
        }

        enum Status {
            Old,
            New,
        }

        let status = match specs.status.unwrap_or(b"UNKNOWN").trim_ascii_end() {
            b"OLD" => Status::Old,
            b"NEW" => Status::New,
            b"SCRATCH" => todo!(),
            b"UNKNOWN" => todo!(),
            v => panic!("OPEN: invalid STATUS={}", String::from_utf8_lossy(v)),
        };

        let _sequential = match specs.access.unwrap_or(b"SEQUENTIAL").trim_ascii_end() {
            b"SEQUENTIAL" => true,
            b"DIRECT" => todo!(),
            v => panic!("OPEN: invalid ACCESS={}", String::from_utf8_lossy(v)),
        };

        let _formatted = match specs.form.unwrap_or(b"UNFORMATTED").trim_ascii_end() {
            b"FORMATTED" => true,
            b"UNFORMATTED" => todo!(),
            v => panic!("OPEN: invalid FORM={}", String::from_utf8_lossy(v)),
        };

        if let Some(file) = specs.file {
            let path = PathBuf::from(
                std::str::from_utf8(file.trim_ascii_end()).map_err(|_| Error::NonUnicodePath)?,
            );

            let stream = std::fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(matches!(status, Status::New))
                .open(path)?;

            self.units.insert(unit, Unit::new(stream));
        } else {
            panic!("TODO: OPEN with no FILE")
        }

        Ok(())
    }

    pub fn close(&mut self, specs: CloseSpecs) -> Result<()> {
        let unit = specs.unit.expect("CLOSE must have UNIT");

        let _remove_file = match specs.status.map(|s| s.trim_ascii_end()) {
            None => false, // TODO: should DELETE if SCRATCH
            Some(b"KEEP") => false,
            Some(b"DELETE") => todo!(),
            Some(v) => panic!("CLOSE: invalid STATUS={}", String::from_utf8_lossy(v)),
        };

        match self.units.get(&unit) {
            None => panic!("TODO: report missing unit"),
            Some(_u) => {
                self.units.remove(&unit);
            }
        }

        Ok(())
    }
}
