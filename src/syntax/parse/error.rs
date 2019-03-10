//! Parse error module. Contains the Result and Error types for the
//! `parse` module.

use failure::Fail;
use std::fmt;

/// Parser result type
///
/// Returned from parsing functions when success can't be guaranteed.
pub type ParseResult<T> = ::std::result::Result<T, ParseError>;

/// List of diagnostics
#[derive(Debug, PartialEq)]
pub struct DiagnosticsList(Vec<String>);

impl From<Vec<String>> for DiagnosticsList {
    fn from(errors: Vec<String>) -> Self {
        DiagnosticsList(errors)
    }
}

impl fmt::Display for DiagnosticsList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for error in self.0.iter() {
            writeln!(f, "error: {}", error)?;
        }
        Ok(())
    }
}

/// Parser error type
///
/// This distinguishes between the different
/// kinds of errors that the `Parser` can encounter.
///
/// TODO: Trying to phase this type out. Ideally parses won't need any
/// error type.
#[derive(Fail, Debug, PartialEq)]
pub enum ParseError {
    /// Unexpected token.
    #[fail(display = "unexpected token: {}", _0)]
    Unexpected(String),

    /// Incomplete data
    #[fail(display = "incomplete expression")]
    Incomplete,

    /// Diagnostics were collected
    #[fail(display = "one or more errors:\n{}", _0)]
    Diagnostics(DiagnosticsList),
}
