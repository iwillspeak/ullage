//! Syntax Token
//!
//! A lexeme in the token stream. Tokens are produced by the
//! `Tokeniser` when parsing a source text.

use super::super::text::{Ident, Span, DUMMY_SPAN};
use super::TriviaToken;
use std::fmt;

/// A Syntax Token
///
/// Syntax tokens are produced by the lexer and contain metadata about
/// their position in the source text.
#[derive(Debug)]
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
#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq, Clone)]
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

    /// The `<=` operator
    LessThanEqual,

    /// The `>` character
    MoreThan,

    /// The `>=` operator
    MoreThanEqual,

    /// The end of the token stream. This is retuend indefinitely once
    /// the lexer reaches the end of the source text.
    End,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::RawString(s) => write!(f, "'{}'", s),
            Literal::Number(n) => write!(f, "{}", n),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Word(id) => match id {
                    Ident::Unknown(_) => "identifier",
                    _ => "keyword",
                },
                TokenKind::Literal(lit) => return write!(f, "literal value {}", lit),
                TokenKind::Equals => "'='",
                TokenKind::DoubleEquals => "'=='",
                TokenKind::Bang => "'!'",
                TokenKind::BangEquals => "'!='",
                TokenKind::Plus => "'+'",
                TokenKind::Minus => "'-'",
                TokenKind::Star => "'*'",
                TokenKind::Slash => "'/'",
                TokenKind::OpenBracket => "'('",
                TokenKind::CloseBracket => "')'",
                TokenKind::OpenSqBracket => "'['",
                TokenKind::CloseSqBracket => "']'",
                TokenKind::Comma => "','",
                TokenKind::Colon => "':'",
                TokenKind::LessThan => "'<'",
                TokenKind::LessThanEqual => "'<='",
                TokenKind::MoreThan => "'>'",
                TokenKind::MoreThanEqual => "'>='",
                TokenKind::End => "end of file",
            }
        )
    }
}

impl Token {
    /// Create a Token from a Kind
    pub fn new(kind: TokenKind) -> Self {
        Token::with_span(DUMMY_SPAN, kind)
    }

    /// Create a token from a position and kind
    pub fn with_span(span: Span, kind: TokenKind) -> Self {
        Token {
            span,
            leading: Vec::new(),
            trailing: Vec::new(),
            kind,
        }
    }

    /// Get the Span of a Token
    pub fn span(&self) -> Span {
        self.span
    }

    /// Inspect the leading trivia
    pub fn leading(&self) -> &[TriviaToken] {
        &self.leading[..]
    }

    /// Inspect the trailing trivia
    pub fn trailing(&self) -> &[TriviaToken] {
        &self.trailing[..]
    }

    /// Set leading trivia
    pub fn with_leading_trivia(self, leading: Vec<TriviaToken>) -> Self {
        Token { leading, ..self }
    }

    /// Set trailing trivia
    pub fn with_trailing_trivia(self, trailing: Vec<TriviaToken>) -> Self {
        Token { trailing, ..self }
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
            | TokenKind::LessThanEqual
            | TokenKind::MoreThan
            | TokenKind::MoreThanEqual => 40,

            // Arithmetic operators
            TokenKind::Plus | TokenKind::Minus => 50,

            TokenKind::Star | TokenKind::Slash => 60,

            // Index/Call operators
            TokenKind::OpenBracket | TokenKind::OpenSqBracket => 80,

            _ => Self::MIN_LBP,
        }
    }

    /// The minimum binding power of any token. This is used by the
    /// parser to parse root level expressions.
    ///
    /// For any token `MIN_LBP` is less than or equal to the token's
    /// binding power.
    pub const MIN_LBP: u32 = 0;

    /// The maximum binding power of any token. This is used by the
    /// parser to parse right-associative expressions.
    ///
    /// For any token `MAX_LBP` is strictly greatre than the token's
    /// binding power.
    pub const MAX_LBP: u32 = 100;
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.kind == other.kind
            && (self.span == DUMMY_SPAN || other.span == DUMMY_SPAN || self.span == other.span)
    }
}

#[cfg(test)]
mod test {

    use super::super::super::text::Pos;
    use super::super::TriviaTokenKind;
    use super::*;

    #[test]
    fn test_token_equality_ignores_dummy_span() {
        assert_eq!(
            Token::new(TokenKind::OpenBracket),
            Token::new(TokenKind::OpenBracket)
        );
        assert_eq!(
            Token::new(TokenKind::CloseBracket),
            Token::with_span(DUMMY_SPAN, TokenKind::CloseBracket)
        );
        assert_eq!(
            Token::new(TokenKind::Comma),
            Token::with_span(Span::new(Pos::from(0), Pos::from(1)), TokenKind::Comma)
        );
        assert_ne!(
            Token::with_span(Span::new(Pos::from(1), Pos::from(2)), TokenKind::LessThan),
            Token::with_span(Span::new(Pos::from(0), Pos::from(1)), TokenKind::LessThan)
        );
    }

    #[test]
    fn test_token_equality_ignores_trivia() {
        assert_eq!(
            Token::new(TokenKind::Bang),
            Token::new(TokenKind::Bang).with_leading_trivia(vec![TriviaToken::with_span(
                Span::new(Pos::from(0), Pos::from(1)),
                TriviaTokenKind::Newline
            )])
        );
        assert_eq!(
            Token::new(TokenKind::Equals).with_trailing_trivia(vec![TriviaToken::with_span(
                Span::new(Pos::from(0), Pos::from(1)),
                TriviaTokenKind::Whitespace
            )]),
            Token::new(TokenKind::Equals)
        );
    }
}
