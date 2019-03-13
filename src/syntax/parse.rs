//! Contains the types responsible for parsing a buffer into an
//! expression tree.

pub mod error;

mod parser;
mod tokeniser;

#[cfg(test)]
mod checkparse_tests;

pub use self::error::{ParseError, ParseResult};
pub use self::parser::Parser;
