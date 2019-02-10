//! Parse error module. Contains the Result and Error types for the
//! `parse` module.

use std::fmt;
use failure::Fail;

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
            write!(f, "error: {}", error)?;
        }
        Ok(())
    }
}

/// Parser error type
///
/// This distinguishes between the different
/// kinds of errors that the `Parser` can encounter.
///
/// TODO: Both variants of this type should have more data
/// attached. It would be nice to know _what_ token was unexpected or
/// what the incomplete expression could have continued with (for
/// error recovery). It probably makes sense to roll this in with
/// adding position information to the parser tokens and errors
/// though. Not sure if we want to have information in these error
/// return types. Instead we could just return a list of diagnostics.
#[derive(Fail, Debug, PartialEq)]
pub enum ParseError {
    /// Unexpected token.
    #[fail(display = "unexpected token: {:?}", _0)]
    Unexpected(String),

    /// Incomplete data
    #[fail(display = "incomplete expression")]
    Incomplete,

    /// Diagnostics were collected
    ///
    /// TODO: Is this the best way to model parse failure due to
    /// diagnostics being collected?
    #[fail(display = "one or more errors:\n{}", _0)]
    Diagnostics(DiagnosticsList),
}
