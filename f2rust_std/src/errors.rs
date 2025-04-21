use crate::format::FormatError;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("process terminated with code {0}")]
    Terminated(i32),
    #[error("format description error: {0}")]
    Format(#[from] FormatError),
    #[error("IO error")]
    IO(#[from] std::io::Error),
    #[error("non-Unicode path")]
    NonUnicodePath,
    #[error("edit descriptor not allowed in READ")]
    InvalidDescOnInput,
    #[error("READ reached end of file")]
    EndOfFile,
    #[error("file record corrupted (head/tail length mismatch)")]
    CorruptedRecord,
    #[error("IO unit {0} not connected")]
    UnitNotConnected(i32),
    #[error("invalid record number: {0}")]
    InvalidRecordNumber(i32),
    #[error("READ non-existent record number: {0}")]
    NonExistentRecord(i32),
    #[error("OPEN of filename that is already open: {0}")]
    FileAlreadyOpen(String),
    #[error("OPEN STATUS=NEW of filename that already exists: {0}")]
    FileAlreadyExists(String),
    #[error("OPEN STATUS=OLD of filename that does not exist: {0}")]
    FileNotFound(String),
    // NOTE: New IO errors need to be added to capture_iostat
}

pub type Result<T> = std::result::Result<T, Error>;
