//! Parse error module. Contains the Result and Error types for the
//! `parse` module.

/// Parser result type
///
/// Returned from parsing functions when success can't be guaranteed.
pub type ParseResult<T> = ::std::result::Result<T, ParseError>;

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
    #[fail(display = "unexpected token")]
    Unexpected,

    /// Incomplete data
    #[fail(display = "incomplete expression")]
    Incomplete,
}
