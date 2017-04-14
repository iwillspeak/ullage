//! Syntax parsing and expression tree
//!
//! This crate provides a simple abstract syntax tree, and a parser
//! implementation which recognises a simple lanugage using
//! Pratt-style operator precedence parsing.

pub mod parse;
pub mod ast;
