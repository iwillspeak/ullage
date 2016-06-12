//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.
//!
//! This crate provides a simple abstract syntax tree, and a parser
//! implementation which recognises a simple lanugage using
//! Pratt-style operator precedence parsing.

mod parse;

/// This structure represents a single token from the input source
/// buffer.
#[derive(Debug,PartialEq)]
pub enum Token {
    /// Represents a string of alphabetic characters. This could be a
    /// language keyword or a variable or type identifier.
    Word(String),

    /// Whitespace trivia
    Whitespace(String),

    /// Constant numerical value.
    Literal(i64),

    /// The `=` character
    Equals,

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
}

/// Represents an AST prefix operator.
#[derive(Debug,PartialEq)]
pub enum PrefixOp {
    Negate,
}

/// Represents an AST infix operator
#[derive(Debug,PartialEq)]
pub enum InfixOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

impl InfixOp {
    fn for_token(tok: &Token) -> Option<Self> {
        use InfixOp::*;
        use Token::*;
        match *tok {
            Equals => Some(Assign),
            Star => Some(Mul),
            Slash => Some(Div),
            Plus => Some(Add),
            Minus => Some(Sub),
            _ => None,
        }
    }
}

/// Represents an AST expression.
#[derive(Debug,PartialEq)]
pub enum Expression {
    Identifier(String),
    Literal(i64),
    Prefix(PrefixOp, Box<Expression>),
    Infix(Box<Expression>, InfixOp, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[cfg(not(test))]
fn main() {
    use std::io;
    use std::io::prelude::*;

    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            match Expression::parse_str(&line) {
                Ok(parsed) => println!("OK > {:?}", parsed),
                Err(err) => println!("Error: {:?}", err),
            };
        };
    }
}
