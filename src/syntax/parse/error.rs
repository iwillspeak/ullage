//! Parse error module. Contains the Result and Error types for the
//! `parse` module.

use std::{error, fmt};

/// Parser result type. Returned from parsing functions when
/// success can't be guaranteed.
pub type Result<T> = ::std::result::Result<T, Error>;

/// Parser error type. This distinguishes between the different
/// kinds of errors that the `Parser` can encounter.
#[derive(Debug, PartialEq)]
pub enum Error {
    /// Unexpected token.
    Unexpected,

    /// Incomplete data
    Incomplete,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", (self as &error::Error).description())
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Unexpected => "Unexpected token",
            Error::Incomplete => "Incomplete expression",
        }
    }
}
