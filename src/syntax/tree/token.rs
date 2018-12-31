//! Syntax Token
//!
//! A lexeme in the token stream. Tokens are produced by the
//! `Tokeniser` when parsing a source text.

use super::super::text::{Ident, Span};
use super::TriviaToken;

/// A Syntax Token
///
/// Syntax tokens are produced by the lexer and contain metadata about
/// their position in the source text.
#[derive(Debug, PartialEq)]
pub struct Token {
    /// The `TokenKind` for this token. Public to allow matching over
    /// different token kinds.
    pub kind: TokenKind,
    span: Span,
    leading: Vec<TriviaToken>,
    trailing: Vec<TriviaToken>,
}

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

/// Token Kind
///
/// The data held by a token. This is usually just the token type. For
/// some tokens, like `Word` we also store their value. For all token
/// kinds the underlying source can be retrieved from the `Token`'s
/// span.
#[derive(Debug, PartialEq)]
pub enum TokenKind {
    /// A string of alpahbetic characters. This could be a langauge
    /// keyword or a variable or type identifier.
    Word(Ident),

    /// Literal Value. Represents either a string or numeric literal
    /// in the source text. Booleans are not represented by literals
    /// and instead are just `Word` tokens.
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

    /// The end of the token stream. This is retuend indefinitely once
    /// the lexer reaches the end of the source text.
    End,
}

impl Token {
    /// Create a token from a position and kind
    pub fn with_span(span: Span, kind: TokenKind) -> Self {
        Token {
            span,
            leading: Vec::new(),
            trailing: Vec::new(),
            kind,
        }
    }

    /// Left binding power. This controls the precedence of
    /// the symbol when being parsed as an infix operator.
    ///
    /// Returns the associativity, or binding power, for the given
    /// token. This is used when deciding if to parse the `led()`
    /// of this token.
    pub fn lbp(&self) -> u32 {
        match self.kind {
            TokenKind::Equals => 10,

            // ternary if
            TokenKind::Word(Ident::If) | TokenKind::Word(Ident::Unless) => 20,

            // boolean conditional operators
            TokenKind::DoubleEquals
            | TokenKind::BangEquals
            | TokenKind::LessThan
            | TokenKind::MoreThan => 40,

            // Arithmetic operators
            TokenKind::Plus | TokenKind::Minus => 50,

            TokenKind::Star | TokenKind::Slash => 60,

            // Index/Call operators
            TokenKind::OpenBracket | TokenKind::OpenSqBracket => 80,

            _ => 0,
        }
    }
}