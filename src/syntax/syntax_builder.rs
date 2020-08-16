//! Syntax Tree Factory
//!
//! Ergonomic methods to build new expression trees
//!
//! Used for building expression trees by hand in code rather than the
//! parser.

use super::text::Ident;
use super::*;

/// Build a raw string literal
///
/// Takes a string and builds a string literal expression from it. The
/// token is stubbed and will contian a dummy span.
pub fn raw_string<S: Into<String>>(value: S) -> Expression {
    let value = value.into();
    Expression::constant_string(
        Token::new(TokenKind::Literal(Literal::RawString(value.clone()))),
        value,
    )
}

/// Constant Boolean Expression
///
/// Takes a given bool value and builds a stubbed-out token for
/// it. The token will contain a dummy span.
pub fn const_bool(value: bool) -> Expression {
    Expression::constant_bool(word(if value { Ident::True } else { Ident::False }), value)
}

/// Constant numeric value
///
/// Takes a given numeric value and builds a stubbed-out token for
/// it. The token will contain a dummy span.
pub fn const_num(value: i64) -> Expression {
    Expression::constant_num(
        Token::new(TokenKind::Literal(Literal::Number(value))),
        value,
    )
}

/// Identifier Expression. Reads a value from a variable or provides a
/// reference to a function or other named item.
pub fn ident_expr(value: Ident) -> Expression {
    Expression::identifier(word(value), value)
}

/// Word token from identifier. Wraps the identifier in a token with
/// missing position information
pub fn word(id: Ident) -> Token {
    Token::new(TokenKind::Word(id))
}
