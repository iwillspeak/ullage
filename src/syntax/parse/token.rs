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

/// Lexer Token
///
/// This structure represents a single token from the input source
/// buffer. Tokens are produced by the `Tokeniser`.
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

impl<'a> Token<'a> {
    /// Left binding power. This controls the precedence of
    /// the symbol when being parsed as an infix operator.
    ///
    /// Returns the associativity, or binding power, for the given
    /// token. This is used when deciding if to parse the `led()`
    /// of this token.
    pub fn lbp(&self) -> u32 {
        match *self {
            Token::Equals => 10,

            // ternary if
            Token::Word("if") | Token::Word("unless") => 20,

            // boolean conditional operators
            Token::DoubleEquals | Token::BangEquals | Token::LessThan | Token::MoreThan => 40,

            // Arithmetic operators
            Token::Plus | Token::Minus => 50,

            Token::Star | Token::Slash => 60,

            // Index/Call operators
            Token::OpenBracket | Token::OpenSqBracket => 80,

            _ => 0,
        }
    }
}
