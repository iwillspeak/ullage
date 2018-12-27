//! Trivia Tokens
//!
//! Trivia tokens represent parts of a parse which are not important
//! to the semantic analysis of the program.
//!
//! The idea of trivia tokens is to allow a parsed syntax tree to be
//! re-written and then serialised to allow code tranformations
//! without loss of things like comments and indentation.

use super::super::text::Span;

/// Trivia Token
///
/// Trivia tokens appear in the leading or trailing trivia of main
/// tokens. They should be attached to the 'closest' token in the
/// token stream, for some value of closest.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct TriviaToken {
    kind: TriviaTokenKind,
    span: Span,
}

/// Trivia Token Kind
///
/// The data held by a `TriviaToken`
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TriviaTokenKind {
    /// A whitespace token
    Whitespace,
    /// Unrecognised characters
    Junk,
    /// A single line comment
    Comment,
    /// A newline character
    Newline,
}
