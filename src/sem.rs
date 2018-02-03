//! Semantic Analysis and Translation
//!
//! This module is responsible for translating the syntactic
//! representation of a program, as produced by the parser, into a
//! semantically rich model ready to be lowered for execution.

use syntax;

/// A Semantically Decorated Expression
///
/// This struct represents the expression tree after semantic
/// analysis. This is no longer guaranteed to be a a lieral
/// representation of the code as it was written.
pub struct Expression {
    /// TODO: Properly implement this.
    pub expr: syntax::Expression,
}

/// Transform Expression
///
/// Convert a syntax expression into a symantic one.
pub fn transform_expression(expr: syntax::Expression) -> Expression {
    Expression { expr: expr }
}

#[cfg(test)]
pub mod test {}
