//! Compilation error module. Contains the Result and Error types for the compile module.

use std::io;
use std::fmt;

/// Represents the different types of errors which can be encountered
/// when compiling.
#[derive(Debug)]
pub enum Error {
    Generic(String),
    IO(::std::io::Error),
}

/// Compilation result. Returned from each compilation stage.
pub type Result<T> = ::std::result::Result<T, Error>;

impl From<String> for Error {
    /// Convert untyped errors to generic compilation errors.
    fn from(s: String) -> Error {
        Error::Generic(s)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Generic(ref err) => write!(f, "{}", err),
            Error::IO(ref ioerr) => write!(f, "IO Error: {}", ioerr),
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IO(e)
    }
}
