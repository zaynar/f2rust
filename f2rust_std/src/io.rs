mod reader;
mod writer;
pub use reader::*;
pub use writer::*;

use crate::{Error, Result, fstr};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, BufWriter, Cursor, Read, Seek, SeekFrom, Write};
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::{cell::RefCell, rc::Rc};

pub fn capture_iostat<F: FnOnce() -> Result<()>>(f: F) -> Result<i32> {
    match f() {
        Ok(()) => Ok(0),

        // Recoverable IO-related errors should be captured by IOSTAT
        Err(Error::EndOfFile) => Ok(-1),
        Err(Error::IO(err)) => Ok(err.raw_os_error().unwrap_or(i32::MAX)),
        Err(Error::UnitNotConnected(..)) => Ok(10000),
        Err(Error::InvalidRecordNumber(..)) => Ok(10001),
        Err(Error::NonExistentRecord(..)) => Ok(10002),
        Err(Error::FileAlreadyOpen(..)) => Ok(10003),
        Err(Error::FileAlreadyExists(..)) => Ok(10004),
        Err(Error::FileNotFound(..)) => Ok(10005),

        // All other errors should be propagated
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

impl InquireSpecs<'_> {
    fn set_exist(&mut self, exist: bool) {
        if let Some(v) = &mut self.exist {
            **v = exist;
        }
    }

    fn set_opened_number(&mut self, unit: Option<i32>) {
        match unit {
            Some(unit) => {
                if let Some(v) = &mut self.opened {
                    **v = true;
                }

                if let Some(v) = &mut self.number {
                    **v = unit;
                }

                // Not supported: access, form, recl, nextrec, blank
                // (Currently codegen will bail if they are used)
            }
            None => {
                if let Some(v) = &mut self.opened {
                    **v = false;
                }
            }
        }
    }

    fn set_name(&mut self, name: Option<&[u8]>) {
        match name {
            Some(name) => {
                if let Some(v) = &mut self.named {
                    **v = true;
                }

                if let Some(v) = &mut self.name {
                    fstr::assign(v, name);
                }
            }
            None => {
                if let Some(v) = &mut self.named {
                    **v = false;
                }
            }
        }
    }
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

enum OpenStatus {
    Old,
    New,
    Scratch,
}

impl OpenSpecs<'_> {
    fn status(&self) -> OpenStatus {
        match self.status.map(|s| s.trim_ascii_end()) {
            Some(b"OLD") => OpenStatus::Old,
            Some(b"NEW") => OpenStatus::New,
            Some(b"SCRATCH") => OpenStatus::Scratch,
            None | Some(b"UNKNOWN") => todo!(),
            Some(v) => panic!("OPEN: invalid STATUS={}", String::from_utf8_lossy(v)),
        }
    }

    fn sequential(&self) -> bool {
        match self.access.map(|s| s.trim_ascii_end()) {
            None | Some(b"SEQUENTIAL") => true,
            Some(b"DIRECT") => false,
            Some(v) => panic!("OPEN: invalid ACCESS={}", String::from_utf8_lossy(v)),
        }
    }

    fn formatted(&self) -> bool {
        match self.form.map(|s| s.trim_ascii_end()) {
            None => self.sequential(),
            Some(b"FORMATTED") => true,
            Some(b"UNFORMATTED") => false,
            Some(v) => panic!("OPEN: invalid FORM={}", String::from_utf8_lossy(v)),
        }
    }
}

#[derive(Default)]
pub struct CloseSpecs<'a> {
    pub unit: Option<i32>,
    pub status: Option<&'a [u8]>,
}

impl CloseSpecs<'_> {
    fn delete(&self) -> bool {
        match self.status.map(|s| s.trim_ascii_end()) {
            None => false,
            Some(b"KEEP") => false,
            Some(b"DELETE") => true,
            Some(v) => panic!("CLOSE: invalid STATUS={}", String::from_utf8_lossy(v)),
        }
    }
}

// BACKSPACE, ENDFILE, REWIND
#[derive(Default)]
pub struct PosSpecs {
    pub unit: Option<i32>,
}

struct RecIo {
    // To optimise seeking, this tracks the value of Seek::stream_position
    stream_pos: u64,

    sequential: bool,
    formatted: bool,
    recl: i32,
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
impl RecIo {
    fn new(sequential: bool, formatted: bool, recl: i32) -> Self {
        Self {
            stream_pos: 0,
            sequential,
            formatted,
            recl,
        }
    }

    fn read_seq<R: BufRead>(&mut self, mut reader: R) -> Result<Vec<u8>> {
        assert!(self.sequential, "sequential read on non-sequential file");

        if self.formatted {
            let mut buf = Vec::new();
            if reader.read_until(b'\n', &mut buf)? == 0 {
                return Err(Error::EndOfFile);
            }
            self.stream_pos += buf.len() as u64;
            buf.pop(); // remove the '\n'
            Ok(buf)
        } else {
            // Record has u32 LE len as both header and trailer. Use the header to
            // read the whole thing
            let mut len_bytes = [0u8; 4];
            match reader.read_exact(&mut len_bytes) {
                Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                    return Err(Error::EndOfFile);
                }
                Err(e) => return Err(e.into()),
                Ok(()) => (),
            }
            let len = u32::from_le_bytes(len_bytes);

            let mut buf = vec![0; len as usize + 4];
            reader.read_exact(&mut buf)?;

            self.stream_pos += len as u64 + 8;

            // Remove the trailer, and verify it's consistent with len
            let tail_bytes = buf.drain(buf.len() - 4..).collect::<Vec<_>>();
            let tail_len = u32::from_le_bytes(tail_bytes.try_into().unwrap());
            if tail_len != len {
                return Err(Error::CorruptedRecord);
            }
            Ok(buf)
        }
    }

    fn write_seq<W: Write>(&mut self, record: &[u8], mut writer: W) -> Result<()> {
        assert!(self.sequential, "sequential write on non-sequential file");

        if self.formatted {
            writer.write_all(record)?;
            writer.write_all(b"\n")?;
            self.stream_pos += record.len() as u64 + 1;
        } else {
            let len = (record.len() as u32).to_le_bytes();
            writer.write_all(&len)?;
            writer.write_all(record)?;
            writer.write_all(&len)?;
            self.stream_pos += record.len() as u64 + 8;
        }
        Ok(())
    }

    fn read_direct<R: Read + Seek>(&mut self, recnum: i32, mut reader: R) -> Result<Vec<u8>> {
        assert!(!self.sequential, "direct read on non-direct file");

        if recnum <= 0 {
            return Err(Error::InvalidRecordNumber(recnum));
        }

        // Use seek_relative to avoid BufReader discarding its buffer
        let rec_pos = (recnum - 1) as u64 * self.recl as u64;
        reader.seek_relative(rec_pos as i64 - self.stream_pos as i64)?;

        let mut buf = vec![0; self.recl as usize];

        // To match gfortran, as required by ZZASCII:
        // If this record is entirely after EOF, return an error.
        // If this record is interrupted by EOF, return the partial record;
        // the rest of the buffer will have undefined contents (with no indication
        // to the application of how much was actually read, which seems dangerous?)

        let mut read = 0;
        while read < buf.len() {
            let n = reader.read(&mut buf[read..])?;
            read += n;

            // Abort on EOF
            if n == 0 {
                break;
            }
        }

        self.stream_pos = rec_pos + read as u64;

        if read == 0 {
            Err(Error::NonExistentRecord(recnum))
        } else {
            Ok(buf)
        }
    }

    fn write_direct<W: Write + Seek>(
        &mut self,

        recnum: i32,
        numrecs: u64,

        record: &[u8],
        mut writer: W,
    ) -> Result<()> {
        assert!(!self.sequential, "direct write on non-direct file");

        assert!(
            record.len() as i32 <= self.recl,
            "record length {} exceeded RECL {}",
            record.len(),
            self.recl
        );

        let rec_pos = (recnum - 1) as u64 * self.recl as u64;

        // If we're >=1 record past the end of the file, we need to insert empty records
        if (recnum - 1) as u64 > numrecs {
            let fill = if self.formatted { b' ' } else { b'\0' };
            let dummy = vec![fill; self.recl as usize];

            writer.seek(SeekFrom::Start(numrecs * self.recl as u64))?;
            for _ in numrecs..(recnum - 1) as u64 {
                writer.write_all(&dummy)?;
            }
            self.stream_pos = rec_pos;
        } else {
            writer.seek_relative(rec_pos as i64 - self.stream_pos as i64)?;
        }

        writer.write_all(record)?;
        if record.len() < self.recl as usize {
            writer.write_all(&vec![0u8; self.recl as usize - record.len()])?;
        }

        self.stream_pos = rec_pos + self.recl as u64;

        Ok(())
    }
}

pub trait RecFile {
    fn read(&mut self, _recnum: Option<i32>) -> Result<Vec<u8>> {
        panic!("READ not supported on this file type");
    }

    fn write(&mut self, _recnum: Option<i32>, _record: &[u8]) -> Result<()> {
        panic!("WRITE not supported on this file type");
    }

    fn backspace(&mut self) -> Result<()> {
        panic!("BACKSPACE not supported on this file type");
    }

    fn endfile(&mut self) -> Result<()> {
        panic!("ENDFILE not supported on this file type");
    }

    fn rewind(&mut self) -> Result<()> {
        panic!("REWIND not supported on this file type");
    }
}

pub type RecFileRef<'a> = Rc<RefCell<dyn RecFile + 'a>>;

struct FsRecFile {
    reader: Option<BufReader<std::fs::File>>,
    writer: Option<BufWriter<std::fs::File>>,
    rec_io: RecIo,

    // On the first sequential write, we must truncate the file
    truncated: bool,

    // Number of records in file, if known
    numrecs: Option<u64>,
}

const BUFREADER_CAPACITY: usize = 65536;

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
            reader: Some(BufReader::with_capacity(BUFREADER_CAPACITY, file)),
            writer: None,
            rec_io: RecIo::new(sequential, formatted, recl.unwrap_or(0)),

            truncated: false,
            numrecs: None,
        }
    }

    fn prepare_reader(&mut self) -> Result<()> {
        if let Some(mut w) = self.writer.take() {
            w.flush()?;
            let mut r = BufReader::with_capacity(
                BUFREADER_CAPACITY,
                w.into_inner().map_err(|e| e.into_error())?,
            );
            r.seek(SeekFrom::Start(self.rec_io.stream_pos))?;
            self.reader = Some(r);
        }

        Ok(())
    }

    fn prepare_writer(&mut self) -> Result<()> {
        if let Some(r) = self.reader.take() {
            let mut w = BufWriter::new(r.into_inner());
            w.seek(SeekFrom::Start(self.rec_io.stream_pos))?;
            self.writer = Some(w);
        }

        Ok(())
    }

    fn read_seq(&mut self) -> Result<Vec<u8>> {
        self.prepare_reader()?;
        self.rec_io.read_seq(self.reader.as_mut().unwrap())
    }

    fn write_seq(&mut self, record: &[u8]) -> Result<()> {
        self.prepare_writer()?;
        self.rec_io
            .write_seq(record, self.writer.as_mut().unwrap())?;

        // The first sequential write should truncate the file.
        // (Subsequent writes don't need to, because they're only making the file longer)
        if !self.truncated {
            // Convert BufWriter into File
            let mut w = self.writer.take().unwrap();
            w.flush()?;
            let pos = w.stream_position()?;
            let file = w.into_inner().map_err(|e| e.into_error())?;

            // Truncate
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
        self.prepare_reader()?;
        self.rec_io
            .read_direct(recnum, self.reader.as_mut().unwrap())
    }

    fn write_direct(&mut self, recnum: i32, record: &[u8]) -> Result<()> {
        self.prepare_writer()?;

        // Get the cached file size, or compute it
        let numrecs = match self.numrecs {
            Some(n) => n,
            None => {
                let w = self.writer.as_mut().unwrap();
                w.seek(SeekFrom::End(0))?;
                self.rec_io.stream_pos = w.stream_position()?;
                self.rec_io.stream_pos / (self.rec_io.recl as u64)
            }
        };

        self.rec_io
            .write_direct(recnum, numrecs, record, self.writer.as_mut().unwrap())?;

        self.numrecs = Some(numrecs.max(recnum as u64));

        Ok(())
    }
}

impl RecFile for FsRecFile {
    fn read(&mut self, recnum: Option<i32>) -> Result<Vec<u8>> {
        if let Some(recnum) = recnum {
            self.read_direct(recnum)
        } else {
            self.read_seq()
        }
    }

    fn write(&mut self, recnum: Option<i32>, record: &[u8]) -> Result<()> {
        if let Some(recnum) = recnum {
            self.write_direct(recnum, record)
        } else {
            self.write_seq(record)
        }
    }

    fn rewind(&mut self) -> Result<()> {
        if let Some(w) = &mut self.writer {
            w.seek(SeekFrom::Start(0))?;
        }

        if let Some(r) = &mut self.reader {
            r.seek(SeekFrom::Start(0))?;
        }

        self.rec_io.stream_pos = 0;

        Ok(())
    }
}

struct VirtualRecFile {
    file: VirtualFileData,
    rec_io: RecIo,
}

// RecFile wrapper for a Vec<u8>, used for tests with a virtual filesystem
impl VirtualRecFile {
    fn new(
        file: VirtualFileData,
        sequential: bool,
        formatted: bool,
        recl: Option<i32>,
    ) -> Result<Self> {
        assert_eq!(
            recl.is_some(),
            !sequential,
            "RECL must be specified iff DIRECT"
        );

        assert!(recl.unwrap_or(1) > 0, "RECL must be positive");

        // Ensure the file starts from the start, each time it's opened
        file.borrow_mut().seek(SeekFrom::Start(0))?;

        Ok(Self {
            file,
            rec_io: RecIo::new(sequential, formatted, recl.unwrap_or(0)),
        })
    }

    fn read_seq(&mut self) -> Result<Vec<u8>> {
        let mut file = self.file.borrow_mut();
        self.rec_io.read_seq(file.deref_mut())
    }

    fn write_seq(&mut self, record: &[u8]) -> Result<()> {
        let mut file = self.file.borrow_mut();
        self.rec_io.write_seq(record, file.deref_mut())?;

        // Truncate the file after this write
        let pos = file.stream_position()?;
        file.get_mut().truncate(pos as usize);

        Ok(())
    }

    fn read_direct(&mut self, recnum: i32) -> Result<Vec<u8>> {
        let mut file = self.file.borrow_mut();
        self.rec_io.read_direct(recnum, file.deref_mut())
    }

    fn write_direct(&mut self, recnum: i32, record: &[u8]) -> Result<()> {
        let mut file = self.file.borrow_mut();
        let numrecs = file.get_ref().len() as u64 / self.rec_io.recl as u64;
        self.rec_io
            .write_direct(recnum, numrecs, record, file.deref_mut())
    }
}

impl RecFile for VirtualRecFile {
    fn read(&mut self, recnum: Option<i32>) -> Result<Vec<u8>> {
        if let Some(recnum) = recnum {
            self.read_direct(recnum)
        } else {
            self.read_seq()
        }
    }

    fn write(&mut self, recnum: Option<i32>, record: &[u8]) -> Result<()> {
        if let Some(recnum) = recnum {
            self.write_direct(recnum, record)
        } else {
            self.write_seq(record)
        }
    }

    fn rewind(&mut self) -> Result<()> {
        self.file.borrow_mut().seek(SeekFrom::Start(0))?;
        self.rec_io.stream_pos = 0;
        Ok(())
    }
}

// RecFile wrapper for stdout, using println! so that tests will capture it
struct StdoutRecFile {}

impl RecFile for StdoutRecFile {
    fn write(&mut self, _recnum: Option<i32>, record: &[u8]) -> Result<()> {
        println!("{}", String::from_utf8_lossy(record));
        Ok(())
    }
}

// RecFile wrapper for an arbitrary Write type (particularly stdout)
struct WriterRecFile<W: Write> {
    writer: W,
}

impl<W: Write> RecFile for WriterRecFile<W> {
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

    fn backspace(&mut self) -> Result<()> {
        panic!("cannot BACKSPACE internal files");
    }

    fn endfile(&mut self) -> Result<()> {
        panic!("cannot ENDFILE internal files");
    }

    fn rewind(&mut self) -> Result<()> {
        panic!("cannot REWIND internal files");
    }
}

pub trait FileManager<'a> {
    fn capture_stdout(&mut self, stdout: &'a mut Vec<u8>);

    fn unit(&mut self, unit: i32) -> Result<&RecFileRef<'a>>;

    fn inquire(&mut self, specs: InquireSpecs) -> Result<()>;
    fn open(&mut self, specs: OpenSpecs) -> Result<()>;
    fn close(&mut self, specs: CloseSpecs) -> Result<()>;

    fn backspace(&mut self, specs: PosSpecs) -> Result<()> {
        let unit = specs.unit.expect("BACKSPACE must have UNIT");
        self.unit(unit)?.borrow_mut().backspace()
    }

    fn endfile(&mut self, specs: PosSpecs) -> Result<()> {
        let unit = specs.unit.expect("ENDFILE must have UNIT");
        self.unit(unit)?.borrow_mut().endfile()
    }

    fn rewind(&mut self, specs: PosSpecs) -> Result<()> {
        let unit = specs.unit.expect("REWIND must have UNIT");
        self.unit(unit)?.borrow_mut().rewind()
    }
}

struct FsUnit<'a> {
    path: Option<PathBuf>,
    file: RecFileRef<'a>,
}

impl<'a> FsUnit<'a> {
    fn new<F: RecFile + 'a>(path: Option<PathBuf>, file: F) -> Self {
        Self {
            path,
            file: Rc::new(RefCell::new(file)),
        }
    }
}

pub struct FsFileManager<'a> {
    units: HashMap<i32, FsUnit<'a>>,
    filenames: HashMap<PathBuf, i32>,
    cwd: PathBuf,
}

impl<'a> FsFileManager<'a> {
    pub fn new(cwd: &Path) -> Self {
        Self {
            units: HashMap::from([(6, FsUnit::new(None, StdoutRecFile {}))]),
            filenames: HashMap::new(),
            cwd: cwd.to_path_buf(),
        }
    }

    fn path_from_fstr(&self, path: &[u8]) -> Result<PathBuf> {
        let str = std::str::from_utf8(path.trim_ascii_end()).map_err(|_| Error::NonUnicodePath)?;
        Ok(self.cwd.join(str))
    }
}

impl<'a> FileManager<'a> for FsFileManager<'a> {
    fn capture_stdout(&mut self, stdout: &'a mut Vec<u8>) {
        self.units
            .insert(6, FsUnit::new(None, WriterRecFile { writer: stdout }));
    }

    fn unit(&mut self, unit: i32) -> Result<&RecFileRef<'a>> {
        match self.units.get(&unit) {
            None => Err(Error::UnitNotConnected(unit)),
            Some(u) => Ok(&u.file),
        }
    }

    fn inquire(&mut self, mut specs: InquireSpecs) -> Result<()> {
        if let Some(file) = specs.file {
            let path = self.path_from_fstr(file)?;
            if path.try_exists()? {
                specs.set_exist(true);
                specs.set_opened_number(self.filenames.get(&path).copied());
                specs.set_name(Some(
                    path.as_os_str()
                        .to_str()
                        .ok_or(Error::NonUnicodePath)?
                        .as_bytes(),
                ));

                // Not supported: sequential, direct, formatted, unformatted
                // (Currently codegen will bail if they are used)
            } else {
                specs.set_exist(false);
            }
        } else if let Some(unit) = specs.unit {
            // We support unlimited units, so any valid number exists
            specs.set_exist(unit >= 0);

            if let Some(u) = self.units.get(&unit) {
                specs.set_opened_number(Some(unit));

                if let Some(path) = &u.path {
                    specs.set_name(Some(
                        path.as_os_str()
                            .to_str()
                            .ok_or(Error::NonUnicodePath)?
                            .as_bytes(),
                    ));
                } else {
                    specs.set_name(None);
                }

                // Not supported: access, sequential, direct, form,
                // formatted, unformatted, recl, nextrec, blank
                // (Currently codegen will bail if they are used)
            } else {
                specs.set_opened_number(None);
            }
        } else {
            // codegen should prevent this case
            panic!("INQUIRE must have either FILE or UNIT");
        }

        Ok(())
    }

    fn open(&mut self, specs: OpenSpecs) -> Result<()> {
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

        if let Some(file) = specs.file {
            if matches!(specs.status(), OpenStatus::Scratch) {
                panic!("OPEN: must not specify FILE with STATUS=SCRATCH")
            }

            let path = self.path_from_fstr(file)?;

            if self.filenames.contains_key(&path) {
                return Err(Error::FileAlreadyOpen(path.display().to_string()));
            }

            let f = std::fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(matches!(specs.status(), OpenStatus::New))
                .open(&path)?;

            self.units.insert(
                unit,
                FsUnit::new(
                    Some(path.clone()),
                    FsRecFile::new(f, specs.sequential(), specs.formatted(), specs.recl),
                ),
            );

            self.filenames.insert(path, unit);
        } else {
            if !matches!(specs.status(), OpenStatus::Scratch) {
                panic!("OPEN: must specify FILE unless STATUS=SCRATCH")
            }

            let f = tempfile::tempfile_in(&self.cwd)?;

            self.units.insert(
                unit,
                FsUnit::new(
                    None,
                    FsRecFile::new(f, specs.sequential(), specs.formatted(), specs.recl),
                ),
            );
        }

        Ok(())
    }

    fn close(&mut self, specs: CloseSpecs) -> Result<()> {
        let unit = specs.unit.expect("CLOSE must have UNIT");

        match self.units.get(&unit) {
            None => (), // closing a non-connected unit is permitted
            Some(u) => {
                if let Some(path) = &u.path {
                    if specs.delete() {
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

struct VirtualUnit<'a> {
    path: Option<Vec<u8>>,
    file: RecFileRef<'a>,
}

impl<'a> VirtualUnit<'a> {
    fn new<F: RecFile + 'a>(path: Option<Vec<u8>>, file: F) -> Self {
        Self {
            path,
            file: Rc::new(RefCell::new(file)),
        }
    }
}

type VirtualFileData = Rc<RefCell<Cursor<Vec<u8>>>>;

pub struct VirtualFileManager<'a> {
    units: HashMap<i32, VirtualUnit<'a>>,
    filenames: HashMap<Vec<u8>, i32>,

    files: HashMap<Vec<u8>, VirtualFileData>,
}

impl<'a> VirtualFileManager<'a> {
    pub fn new() -> Self {
        Self {
            units: HashMap::from([(6, VirtualUnit::new(None, StdoutRecFile {}))]),
            filenames: HashMap::new(),
            files: HashMap::new(),
        }
    }

    pub fn set_stdout<W: Write + 'a>(&mut self, writer: W) {
        self.units
            .insert(6, VirtualUnit::new(None, WriterRecFile { writer }));
    }

    pub fn insert(&mut self, path: &[u8], file: &[u8]) {
        self.files.insert(
            path.to_vec(),
            Rc::new(RefCell::new(Cursor::new(file.to_vec()))),
        );
    }

    fn path_from_fstr(&self, path: &[u8]) -> Vec<u8> {
        path.trim_ascii_end().to_vec()
    }
}

impl<'a> FileManager<'a> for VirtualFileManager<'a> {
    // TODO: reduce code duplication with FsFileManager

    fn capture_stdout(&mut self, stdout: &'a mut Vec<u8>) {
        self.units
            .insert(6, VirtualUnit::new(None, WriterRecFile { writer: stdout }));
    }

    fn unit(&mut self, unit: i32) -> Result<&RecFileRef<'a>> {
        match self.units.get(&unit) {
            None => Err(Error::UnitNotConnected(unit)),
            Some(u) => Ok(&u.file),
        }
    }

    fn inquire(&mut self, mut specs: InquireSpecs) -> Result<()> {
        if let Some(file) = specs.file {
            let path = self.path_from_fstr(file);
            if self.files.contains_key(&path) {
                specs.set_exist(true);
                specs.set_opened_number(self.filenames.get(&path).copied());
                specs.set_name(Some(&path));

                // Not supported: sequential, direct, formatted, unformatted
                // (Currently codegen will bail if they are used)
            } else {
                specs.set_exist(false);
            }
        } else if let Some(unit) = specs.unit {
            // We support unlimited units, so any valid number exists
            specs.set_exist(unit >= 0);

            if let Some(u) = self.units.get(&unit) {
                specs.set_opened_number(Some(unit));

                if let Some(path) = &u.path {
                    specs.set_name(Some(&path));
                } else {
                    specs.set_name(None);
                }

                // Not supported: access, sequential, direct, form,
                // formatted, unformatted, recl, nextrec, blank
                // (Currently codegen will bail if they are used)
            } else {
                specs.set_opened_number(None);
            }
        } else {
            // codegen should prevent this case
            panic!("INQUIRE must have either FILE or UNIT");
        }

        Ok(())
    }

    fn open(&mut self, specs: OpenSpecs) -> Result<()> {
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

        if let Some(file) = specs.file {
            if matches!(specs.status(), OpenStatus::Scratch) {
                panic!("OPEN: must not specify FILE with STATUS=SCRATCH")
            }

            let path = self.path_from_fstr(file);

            if self.filenames.contains_key(&path) {
                return Err(Error::FileAlreadyOpen(
                    String::from_utf8_lossy(&path).to_string(),
                ));
            }

            let data = match self.files.get(&path) {
                Some(data) => {
                    if matches!(specs.status(), OpenStatus::New) {
                        return Err(Error::FileAlreadyExists(
                            String::from_utf8_lossy(&path).to_string(),
                        ));
                    }
                    Rc::clone(data)
                }
                None => {
                    if matches!(specs.status(), OpenStatus::Old) {
                        return Err(Error::FileNotFound(
                            String::from_utf8_lossy(&path).to_string(),
                        ));
                    }
                    let data = Rc::new(RefCell::new(Cursor::new(Vec::new())));
                    self.files.insert(path.clone(), Rc::clone(&data));
                    data
                }
            };

            self.units.insert(
                unit,
                VirtualUnit::new(
                    Some(path.clone()),
                    VirtualRecFile::new(data, specs.sequential(), specs.formatted(), specs.recl)?,
                ),
            );

            self.filenames.insert(path, unit);
        } else {
            if !matches!(specs.status(), OpenStatus::Scratch) {
                panic!("OPEN: must specify FILE unless STATUS=SCRATCH")
            }

            // Scratch file, so don't insert into self.files

            let data = Rc::new(RefCell::new(Cursor::new(Vec::new())));

            self.units.insert(
                unit,
                VirtualUnit::new(
                    None,
                    VirtualRecFile::new(data, specs.sequential(), specs.formatted(), specs.recl)?,
                ),
            );
        }

        Ok(())
    }

    fn close(&mut self, specs: CloseSpecs) -> Result<()> {
        let unit = specs.unit.expect("CLOSE must have UNIT");

        match self.units.get(&unit) {
            None => (), // closing a non-connected unit is permitted
            Some(u) => {
                if let Some(path) = &u.path {
                    if specs.delete() {
                        self.files.remove(path);
                    }
                    self.filenames.remove(path);
                }
                self.units.remove(&unit);
            }
        }

        Ok(())
    }
}
