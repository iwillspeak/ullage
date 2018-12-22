//! Syntax Token Structure
//!
//! This module contains the token strucutre produced by the
//! lexter.

/// Literal Value
///
/// Represents any constant / literal value in the syntax tree.
#[derive(Debug, PartialEq)]
pub enum Literal {
    /// A literal string
    RawString(String),

    /// A numeric literal
    Number(i64),
}

/// This structure represents a single token from the input source
/// buffer.
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    /// Represents a string of alphabetic characters. This could be a
    /// language keyword or a variable or type identifier.
    Word(&'a str),

    /// Whitespace trivia
    Whitespace(&'a str),

    /// Constant numerical value.
    Literal(Literal),

    /// The `=` character
    Equals,

    /// The `==` operator
    DoubleEquals,

    /// The `!` character
    Bang,

    /// The `!=` operator
    BangEquals,

    /// The `+` character
    Plus,

    /// The `-` character
    Minus,

    /// The `*` character
    Star,

    /// The `/` character
    Slash,

    /// The `(` character
    OpenBracket,

    /// The `)` character
    CloseBracket,

    /// The `[` character
    OpenSqBracket,

    /// The `]` character
    CloseSqBracket,

    /// The `,` character
    Comma,

    /// The `:` character
    Colon,

    /// The `<` character
    LessThan,

    /// The `>` character
    MoreThan,

    /// An unrecognised token
    Unknown(char),
}
