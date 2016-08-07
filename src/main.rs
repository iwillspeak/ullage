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
    Function(Box<Expression>, Vec<Expression>, Vec<Expression>)
}

#[cfg(not(test))]
fn main() {
    use std::io;
    use std::io::prelude::*;
    let mut buffered = String::new();

    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            buffered.push_str(&line);
            match Expression::parse_str(&buffered) {
                Ok(parsed) => {
                    println!("OK > {:?}", parsed);
                    buffered.clear();
                },
                Err(err) => println!("Error: {:?} ({})", err, buffered)
            };
        };
    }
}
