mod reader;
mod writer;
pub use reader::*;
pub use writer::*;

use crate::{Error, Result, fstr};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, BufWriter, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::{cell::RefCell, rc::Rc};

pub fn capture_iostat<F: FnOnce() -> Result<()>>(f: F) -> Result<i32> {
    match f() {
        Ok(()) => Ok(0),
        Err(Error::IO(err)) => {
            Ok(err.raw_os_error().unwrap_or(i32::MAX))
        }
        Err(Error::EndOfFile) => Ok(-1),
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

// Sequential IO:
// Records are arbitrary length, terminated with '\n' (if formatted;
// what if unformatted?)
//
// Reading consumes the next record.
// Writing always appends to the end, and doesn't affect the read position.
//
// For performance, we want to use BufReader/BufWriter. But they both want
// exclusive access to the file, so we use Options to swap between them
// (flushing internal buffers as needed).
//
// Direct IO:
// Records are fixed length.
// Read/write always specify RECN.

pub trait RecFile {
    fn read_seq(&mut self) -> Result<Vec<u8>>;
    fn write_seq(&mut self, record: &[u8]) -> Result<()>;
    fn read_direct(&mut self, recn: i32) -> Result<Vec<u8>>;
    fn write_direct(&mut self, recn: i32, record: &[u8]) -> Result<()>;
}

pub struct FsRecFile {
    read_pos: u64,
    reader: Option<BufReader<std::fs::File>>,
    writer: Option<BufWriter<std::fs::File>>,
}

impl FsRecFile {
    fn new(file: std::fs::File) -> Self {
        Self {
            read_pos: 0,
            reader: Some(BufReader::new(file)),
            writer: None,
        }
    }

    fn reader(&mut self) -> Result<&mut BufReader<std::fs::File>> {
        if let Some(mut w) = self.writer.take() {
            w.flush()?;
            let mut r = BufReader::new(w.into_inner().map_err(|e| e.into_error())?);
            r.seek(SeekFrom::Start(self.read_pos))?;
            self.reader = Some(r);
        }

        Ok(self.reader.as_mut().unwrap())
    }

    fn writer(&mut self) -> Result<&mut BufWriter<std::fs::File>> {
        if let Some(mut r) = self.reader.take() {
            self.read_pos = r.stream_position()?;
            let mut w = BufWriter::new(r.into_inner());
            w.seek(SeekFrom::End(0))?;
            self.writer = Some(w);
        }

        Ok(self.writer.as_mut().unwrap())
    }
}

impl RecFile for FsRecFile {
    fn read_seq(&mut self) -> Result<Vec<u8>> {
        let mut buf = Vec::new();
        if self.reader()?.read_until(b'\n', &mut buf)? == 0 {
            return Err(Error::EndOfFile);
        }
        buf.pop(); // remove the '\n'
        Ok(buf)
    }

    fn write_seq(&mut self, record: &[u8]) -> Result<()> {
        let w = self.writer()?;
        w.write_all(record)?;
        w.write_all(&[b'\n'])?;
        Ok(())
    }

    fn read_direct(&mut self, _recn: i32) -> Result<Vec<u8>> {
        todo!()
    }

    fn write_direct(&mut self, _recn: i32, _record: &[u8]) -> Result<()> {
        todo!()
    }
}

pub struct WriterRecFile<W: Write> {
    writer: W,
}

impl<W: Write> RecFile for WriterRecFile<W> {
    fn read_seq(&mut self) -> Result<Vec<u8>> {
        todo!();
    }

    fn write_seq(&mut self, record: &[u8]) -> Result<()> {
        self.writer.write_all(record)?;
        self.writer.write_all(b"\n")?;
        Ok(())
    }

    fn read_direct(&mut self, _recn: i32) -> Result<Vec<u8>> {
        todo!()
    }

    fn write_direct(&mut self, _recn: i32, _record: &[u8]) -> Result<()> {
        todo!()
    }
}

struct Unit<'a> {
    path: Option<PathBuf>,
    file: Rc<RefCell<dyn RecFile + 'a>>,

    // TODO: do we actually need this?
    #[allow(dead_code)]
    formatted: bool,
}

impl<'a> Unit<'a> {
    fn new<F: RecFile + 'a>(path: Option<PathBuf>, file: F, formatted: bool) -> Self {
        Self {
            path,
            file: Rc::new(RefCell::new(file)),
            formatted,
        }
    }
}

pub struct FileManager<'a> {
    units: HashMap<i32, Unit<'a>>,
    cwd: PathBuf,
}

impl<'a> FileManager<'a> {
    pub fn new() -> Self {
        Self {
            units: HashMap::from([
                // (5, StdinUnit {}),
                (
                    6,
                    Unit::new(
                        None,
                        WriterRecFile {
                            writer: std::io::stdout(),
                        },
                        true,
                    ),
                ),
            ]),
            cwd: PathBuf::new(),
        }
    }

    pub fn set_cwd(&mut self, path: PathBuf) {
        self.cwd = path;
    }

    fn path_from_fstr(&self, path: &[u8]) -> Result<PathBuf> {
        let str = std::str::from_utf8(path.trim_ascii_end()).map_err(|_| Error::NonUnicodePath)?;
        Ok(self.cwd.join(str))
    }

    fn io_unit(&mut self, unit: i32) -> Result<Rc<RefCell<dyn RecFile + 'a>>> {
        match self.units.get(&unit) {
            None => panic!("TODO: report missing unit"),
            Some(u) => Ok(Rc::clone(&u.file)),
        }
    }

    pub fn read_unit(&mut self, unit: Option<i32>) -> Result<Rc<RefCell<dyn RecFile + 'a>>> {
        self.io_unit(unit.unwrap_or(5))
    }

    pub fn write_unit(&mut self, unit: Option<i32>) -> Result<Rc<RefCell<dyn RecFile + 'a>>> {
        self.io_unit(unit.unwrap_or(6))
    }

    pub fn set_stdout<W: Write + 'a>(&mut self, writer: W) {
        self.units
            .insert(6, Unit::new(None, WriterRecFile { writer }, true));
    }

    pub fn inquire(&mut self, specs: InquireSpecs) -> Result<()> {
        if let Some(file) = specs.file {
            let path = self.path_from_fstr(file)?;
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

        let formatted = match specs.form.unwrap_or(b"UNFORMATTED").trim_ascii_end() {
            b"FORMATTED" => true,
            b"UNFORMATTED" => false,
            v => panic!("OPEN: invalid FORM={}", String::from_utf8_lossy(v)),
        };

        if let Some(file) = specs.file {
            let path = self.path_from_fstr(file)?;

            let f = std::fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(matches!(status, Status::New))
                .open(&path)?;

            self.units
                .insert(unit, Unit::new(Some(path), FsRecFile::new(f), formatted));
        } else {
            panic!("TODO: OPEN with no FILE")
        }

        Ok(())
    }

    pub fn close(&mut self, specs: CloseSpecs) -> Result<()> {
        let unit = specs.unit.expect("CLOSE must have UNIT");

        let delete = match specs.status.map(|s| s.trim_ascii_end()) {
            None => false, // TODO: should DELETE if SCRATCH
            Some(b"KEEP") => false,
            Some(b"DELETE") => true,
            Some(v) => panic!("CLOSE: invalid STATUS={}", String::from_utf8_lossy(v)),
        };

        match self.units.get(&unit) {
            None => panic!("TODO: report missing unit"),
            Some(u) => {
                if delete {
                    if let Some(path) = &u.path {
                        std::fs::remove_file(path)?;
                    }
                }
                self.units.remove(&unit);
            }
        }

        Ok(())
    }
}
