//! Compilation error module. Contains the Result and Error types for the compile module.

use std::io;

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

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IO(e)
    }
}