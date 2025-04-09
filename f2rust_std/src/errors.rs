use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Error {
    Terminated(i32),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Terminated(code) => write!(f, "process terminated with code {code}"),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
