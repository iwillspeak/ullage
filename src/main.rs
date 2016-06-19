//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.
//!
//! This crate provides a simple abstract syntax tree, and a parser
//! implementation which recognises a simple lanugage using
//! Pratt-style operator precedence parsing.

mod parse;

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
