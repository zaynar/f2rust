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
    #[error("invalid record number: {0}")]
    InvalidRecordNumber(i32),
    #[error("OPEN of filename that is already open: {0:?}")]
    FileAlreadyOpen(std::path::PathBuf),
}

pub type Result<T> = std::result::Result<T, Error>;
