//! Contains the types responsible for parsing a buffer into an
//! expression tree.

pub mod error;

mod parser;
mod tokeniser;

#[cfg(test)]
mod checkparse_tests;

pub use self::error::{ParseError, ParseResult};
pub use self::parser::Parser;

use super::text::SourceText;
use super::tree::expression::Expression;

/// Parse an Expression Tree from the Source
///
/// Runs the tokeniser and parser over the given input stirng. If the
/// parse was successful then a sequence expression containing all the
/// expressions in the source is returned. If any error is encountered
/// then that is surfaced instead.
pub fn parse_tree(source: &SourceText) -> ParseResult<Expression> {
    let mut p = Parser::new(source);
    Ok(Expression::sequence(p.expressions()?))
}

/// Parse a Single Expression
///
/// Runs the tokeniser and parser of the given input string, returning
/// the first expression parsed.
pub fn parse_single(source: &SourceText) -> ParseResult<Expression> {
    let mut p = Parser::new(source);
    p.expression()
}
