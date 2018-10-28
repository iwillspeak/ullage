//! Compilation error module. Contains the Result and Error types for
//! the compile module.

use std::io;

/// Represents the different types of errors which can be encountered
/// when compiling.
#[derive(Fail, Debug)]
pub enum CompError {
    /// Generic Error String
    #[fail(display = "compilation error: {}", _0)]
    Generic(String),

    /// Wrapped IO Error
    #[fail(display = "IO error: {}", _0)]
    IO(#[cause] ::std::io::Error),
}

/// Compilation result. Returned from each compilation stage.
pub type CompResult<T> = ::std::result::Result<T, CompError>;

impl From<String> for CompError {
    /// Convert untyped errors to generic compilation errors.
    fn from(s: String) -> Self {
        CompError::Generic(s)
    }
}

impl From<io::Error> for CompError {
    fn from(e: io::Error) -> Self {
        CompError::IO(e)
    }
}
