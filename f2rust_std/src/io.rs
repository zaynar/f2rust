mod reader;
mod writer;
pub use reader::*;
pub use writer::*;

use crate::{Error, Result, fstr};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::{cell::RefCell, rc::Rc};

pub fn capture_iostat<F: FnOnce() -> Result<()>>(f: F) -> Result<i32> {
    match f() {
        Ok(()) => Ok(0),
        Err(Error::IO(err)) => Ok(err.raw_os_error().unwrap_or(i32::MAX)),
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
// Records are arbitrary length, terminated with '\n' (if formatted),
// or preceded  *and* followed with u32 LE length (if unformatted).
//
// Reading consumes the next record.
// Writing (over)writes the next record, and truncates the file.
//
// For performance, we want to use BufReader/BufWriter. But they both want
// exclusive access to the file, so we use Options to swap between them
// (flushing internal buffers as needed).
//
// Direct IO:
// Records are fixed length, no delimiters.
// Formatted: space filled with ' '. Unformatted: space filled with '\0'.
// Read/write always specify RECN.

pub trait RecFile {
    fn read(&mut self, recnum: Option<i32>) -> Result<Vec<u8>>;
    fn write(&mut self, recnum: Option<i32>, record: &[u8]) -> Result<()>;
}

pub type RecFileRef<'a> = Rc<RefCell<dyn RecFile + 'a>>;

struct FsRecFile {
    reader: Option<BufReader<std::fs::File>>,
    writer: Option<BufWriter<std::fs::File>>,

    sequential: bool,
    formatted: bool,
    recl: i32,

    // On the first sequential write, we must truncate the file
    truncated: bool,

    // Number of records in file, if known
    numrecs: Option<u64>,
}

// RecFile wrapper for a File
impl FsRecFile {
    fn new(file: std::fs::File, sequential: bool, formatted: bool, recl: Option<i32>) -> Self {
        assert_eq!(
            recl.is_some(),
            !sequential,
            "RECL must be specified iff DIRECT"
        );

        assert!(recl.unwrap_or(1) > 0, "RECL must be positive");

        Self {
            reader: Some(BufReader::new(file)),
            writer: None,

            sequential,
            formatted,
            recl: recl.unwrap_or(0),

            truncated: false,
            numrecs: None,
        }
    }

    fn reader(&mut self) -> Result<&mut BufReader<std::fs::File>> {
        if let Some(mut w) = self.writer.take() {
            w.flush()?;
            let pos = w.stream_position()?;
            let mut r = BufReader::new(w.into_inner().map_err(|e| e.into_error())?);
            r.seek(SeekFrom::Start(pos))?;
            self.reader = Some(r);
        }

        Ok(self.reader.as_mut().unwrap())
    }

    fn writer(&mut self) -> Result<&mut BufWriter<std::fs::File>> {
        if let Some(mut r) = self.reader.take() {
            let pos = r.stream_position()?;
            let mut w = BufWriter::new(r.into_inner());
            w.seek(SeekFrom::Start(pos))?;
            self.writer = Some(w);
        }

        Ok(self.writer.as_mut().unwrap())
    }

    fn read_seq(&mut self) -> Result<Vec<u8>> {
        if self.formatted {
            let mut buf = Vec::new();
            if self.reader()?.read_until(b'\n', &mut buf)? == 0 {
                return Err(Error::EndOfFile);
            }
            buf.pop(); // remove the '\n'
            Ok(buf)
        } else {
            let r = self.reader()?;

            // Record has u32 LE len as both header and trailer. Use the header to
            // read the whole thing
            let mut len_bytes = [0u8; 4];
            match r.read_exact(&mut len_bytes) {
                Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                    return Err(Error::EndOfFile);
                }
                Err(e) => return Err(e.into()),
                Ok(()) => (),
            }
            let len = u32::from_le_bytes(len_bytes);

            let mut buf = vec![0; len as usize + 4];
            r.read_exact(&mut buf)?;

            // Remove the trailer, and verify it's consistent with len
            let tail_bytes = buf.drain(buf.len() - 4..).collect::<Vec<_>>();
            let tail_len = u32::from_le_bytes(tail_bytes.try_into().unwrap());
            if tail_len != len {
                return Err(Error::CorruptedRecord);
            }
            Ok(buf)
        }
    }

    fn write_seq(&mut self, record: &[u8]) -> Result<()> {
        if self.formatted {
            let w = self.writer()?;
            w.write_all(record)?;
            w.write_all(b"\n")?;
        } else {
            let w = self.writer()?;
            let len = (record.len() as u32).to_le_bytes();
            w.write_all(&len)?;
            w.write_all(record)?;
            w.write_all(&len)?;
        }

        // The first sequential write should truncate the file.
        // (Subsequent writes don't need to, because they're only making the file longer)
        if !self.truncated {
            // Convert BufWriter into File
            let mut w = self.writer.take().unwrap();
            w.flush()?;
            let pos = w.stream_position()?;
            let file = w.into_inner().map_err(|e| e.into_error())?;

            file.set_len(pos)?;

            // Convert File back into BufWriter
            let mut w = BufWriter::new(file);
            w.seek(SeekFrom::Start(pos))?;
            self.writer = Some(w);

            self.truncated = true;
        }

        Ok(())
    }

    fn read_direct(&mut self, recnum: i32) -> Result<Vec<u8>> {
        let recl = self.recl;

        assert!(recnum > 0, "REC must be positive");

        let r = self.reader()?;
        r.seek(SeekFrom::Start((recnum - 1) as u64 * recl as u64))?;

        let mut buf = vec![0; recl as usize];
        r.read_exact(&mut buf)?;
        Ok(buf)
    }

    fn write_direct(&mut self, recnum: i32, record: &[u8]) -> Result<()> {
        let recl = self.recl;

        assert!(
            record.len() as i32 <= recl,
            "record length {} exceeded RECL {recl}",
            record.len()
        );

        // Get the cached file size, or compute it
        let numrecs = match self.numrecs {
            Some(n) => n,
            None => {
                let w = self.writer()?;
                w.seek(SeekFrom::End(0))?;
                let file_len = w.stream_position()?;
                file_len / (recl as u64)
            }
        };

        // If we're >=1 record past the end of the file, we need to insert empty records
        if (recnum - 1) as u64 > numrecs {
            let fill = if self.formatted { b' ' } else { b'\0' };
            let dummy = vec![fill; recl as usize];

            let w = self.writer()?;
            w.seek(SeekFrom::End(0))?;
            for _ in numrecs..(recnum - 1) as u64 {
                w.write_all(&dummy)?;
            }
        }

        let w = self.writer()?;
        w.seek(SeekFrom::Start((recnum - 1) as u64 * recl as u64))?;
        w.write_all(record)?;
        if record.len() < recl as usize {
            w.write_all(&vec![0u8; recl as usize - record.len()])?;
        }

        self.numrecs = Some(numrecs.max(recnum as u64));

        Ok(())
    }
}

impl RecFile for FsRecFile {
    fn read(&mut self, recnum: Option<i32>) -> Result<Vec<u8>> {
        if let Some(recnum) = recnum {
            assert!(!self.sequential, "direct read on non-direct file");
            self.read_direct(recnum)
        } else {
            assert!(self.sequential, "sequential read on non-sequential file");
            self.read_seq()
        }
    }

    fn write(&mut self, recnum: Option<i32>, record: &[u8]) -> Result<()> {
        if let Some(recnum) = recnum {
            assert!(!self.sequential, "direct write on non-direct file");
            self.write_direct(recnum, record)
        } else {
            assert!(self.sequential, "sequential write on non-sequential file");
            self.write_seq(record)
        }
    }
}

// RecFile wrapper for an arbitrary Write type (particularly stdout)
struct WriterRecFile<W: Write> {
    writer: W,
}

impl<W: Write> RecFile for WriterRecFile<W> {
    fn read(&mut self, _recnum: Option<i32>) -> Result<Vec<u8>> {
        todo!()
    }

    fn write(&mut self, _recnum: Option<i32>, record: &[u8]) -> Result<()> {
        self.writer.write_all(record)?;
        self.writer.write_all(b"\n")?;
        Ok(())
    }
}

pub struct InternalFile<'a> {
    buf: &'a mut [u8],
    nextrec: i32,
}

impl<'a> InternalFile<'a> {
    pub fn open(buf: &'a mut [u8]) -> RecFileRef<'a> {
        Rc::new(RefCell::new(Self { buf, nextrec: 0 }))
    }
}

impl<'a> RecFile for InternalFile<'a> {
    fn read(&mut self, recnum: Option<i32>) -> Result<Vec<u8>> {
        assert!(
            recnum.is_none(),
            "internal files must be accessed sequentially"
        );
        if self.nextrec == 0 {
            self.nextrec += 1;
            Ok(self.buf.to_vec())
        } else {
            Err(Error::EndOfFile)
        }
    }

    fn write(&mut self, recnum: Option<i32>, record: &[u8]) -> Result<()> {
        assert!(
            recnum.is_none(),
            "internal files must be accessed sequentially"
        );
        if self.nextrec == 0 {
            self.nextrec += 1;
            fstr::assign(self.buf, record);
            Ok(())
        } else {
            Err(Error::EndOfFile)
        }
    }
}

struct Unit<'a> {
    path: Option<PathBuf>,
    file: RecFileRef<'a>,
}

impl<'a> Unit<'a> {
    fn new<F: RecFile + 'a>(path: Option<PathBuf>, file: F) -> Self {
        Self {
            path,
            file: Rc::new(RefCell::new(file)),
        }
    }
}

pub struct FileManager<'a> {
    units: HashMap<i32, Unit<'a>>,
    filenames: HashMap<PathBuf, i32>,
    cwd: PathBuf,
}

impl<'a> FileManager<'a> {
    pub fn new() -> Self {
        Self {
            units: HashMap::from([
                // (5, std::io::stdin ...),
                (
                    6,
                    Unit::new(
                        None,
                        WriterRecFile {
                            writer: std::io::stdout(),
                        },
                    ),
                ),
            ]),
            filenames: HashMap::new(),
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

    pub fn io_unit(&mut self, unit: i32) -> Result<RecFileRef<'a>> {
        match self.units.get(&unit) {
            None => panic!("TODO: report missing unit"),
            Some(u) => Ok(Rc::clone(&u.file)),
        }
    }

    pub fn set_stdout<W: Write + 'a>(&mut self, writer: W) {
        self.units
            .insert(6, Unit::new(None, WriterRecFile { writer }));
    }

    pub fn inquire(&mut self, specs: InquireSpecs) -> Result<()> {
        if let Some(file) = specs.file {
            let path = self.path_from_fstr(file)?;
            if path.try_exists()? {
                if let Some(v) = specs.exist {
                    *v = true;
                }

                if let Some(unit) = self.filenames.get(&path) {
                    if let Some(v) = specs.opened {
                        *v = true;
                    }

                    if let Some(v) = specs.number {
                        *v = *unit;
                    }

                    // TODO: access, form, recl, nextrec, blank
                    // (Currently codegen will bail if they are used)
                } else {
                    if let Some(v) = specs.opened {
                        *v = false;
                    }
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
                // (Currently codegen will bail if they are used)
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

            if let Some(u) = self.units.get(&unit) {
                if let Some(v) = specs.opened {
                    *v = true;
                }

                if let Some(v) = specs.number {
                    *v = unit;
                }

                if let Some(path) = &u.path {
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
                } else {
                    if let Some(v) = specs.named {
                        *v = false;
                    }
                }

                // TODO: access, sequential, direct, form, formatted, unformatted, recl, nextrec, blank
                // (Currently codegen will bail if they are used)
            } else {
                if let Some(v) = specs.opened {
                    *v = false;
                }
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
        //     specs.file.map(|f| std::str::from_utf8(f).unwrap()),
        //     specs.status.map(|f| std::str::from_utf8(f).unwrap()),
        //     specs.access.map(|f| std::str::from_utf8(f).unwrap()),
        //     specs.form.map(|f| std::str::from_utf8(f).unwrap()),
        //     specs.recl,
        // );

        if self.units.contains_key(&unit) {
            panic!("TODO: OPEN of already-open unit {unit}");
        }

        enum Status {
            Old,
            New,
            Scratch,
        }

        let status = match specs.status.unwrap_or(b"UNKNOWN").trim_ascii_end() {
            b"OLD" => Status::Old,
            b"NEW" => Status::New,
            b"SCRATCH" => Status::Scratch,
            b"UNKNOWN" => todo!(),
            v => panic!("OPEN: invalid STATUS={}", String::from_utf8_lossy(v)),
        };

        let sequential = match specs.access.unwrap_or(b"SEQUENTIAL").trim_ascii_end() {
            b"SEQUENTIAL" => true,
            b"DIRECT" => false,
            v => panic!("OPEN: invalid ACCESS={}", String::from_utf8_lossy(v)),
        };

        let formatted_def = if sequential {
            b"FORMATTED".as_slice()
        } else {
            b"UNFORMATTED".as_slice()
        };

        let formatted = match specs.form.unwrap_or(formatted_def).trim_ascii_end() {
            b"FORMATTED" => true,
            b"UNFORMATTED" => false,
            v => panic!("OPEN: invalid FORM={}", String::from_utf8_lossy(v)),
        };

        if let Some(file) = specs.file {
            if matches!(status, Status::Scratch) {
                panic!("OPEN: must not specify FILE with STATUS=SCRATCH")
            }

            let path = self.path_from_fstr(file)?;

            if self.filenames.contains_key(&path) {
                panic!("TODO: OPEN of already-open file {}", path.display());
            }

            let f = std::fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(matches!(status, Status::New))
                .open(&path)?;

            self.units.insert(
                unit,
                Unit::new(
                    Some(path.clone()),
                    FsRecFile::new(f, sequential, formatted, specs.recl),
                ),
            );

            self.filenames.insert(path, unit);
        } else {
            if !matches!(status, Status::Scratch) {
                panic!("OPEN: must specify FILE unless STATUS=SCRATCH")
            }

            let f = tempfile::tempfile_in(&self.cwd)?;

            self.units.insert(
                unit,
                Unit::new(None, FsRecFile::new(f, sequential, formatted, specs.recl)),
            );
        }

        Ok(())
    }

    pub fn close(&mut self, specs: CloseSpecs) -> Result<()> {
        let unit = specs.unit.expect("CLOSE must have UNIT");

        let delete = match specs.status.map(|s| s.trim_ascii_end()) {
            None => false,
            Some(b"KEEP") => false,
            Some(b"DELETE") => true,
            Some(v) => panic!("CLOSE: invalid STATUS={}", String::from_utf8_lossy(v)),
        };

        match self.units.get(&unit) {
            None => (), // closing a non-connected unit is permitted
            Some(u) => {
                if let Some(path) = &u.path {
                    if delete {
                        std::fs::remove_file(path)?;
                    }
                    self.filenames.remove(path);
                }
                self.units.remove(&unit);
            }
        }

        Ok(())
    }
}
