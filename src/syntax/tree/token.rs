//! Syntax Token
//!
//! A lexeme in the token stream. Tokens are produced by the
//! `Tokeniser` when parsing a source text.

use super::super::text::Span;
use super::TriviaToken;

/// A Syntax Token
///
/// Syntax tokens are produced by the lexer and contain metadata about
/// their position in the source text.
pub struct Token {
    kind: TokenKind,
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
pub enum TokenKind {
    /// A string of alpahbetic characters. This could be a langauge
    /// keyword or a variable or type identifier.
    Word(String),     // TODO: This could do with being an interned
    // stirng.

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
