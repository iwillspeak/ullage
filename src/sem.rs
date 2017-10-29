//! Semantic Analysis and Translation
//!
//! THis module is responsible for translating the syntactic
//! representation of a program, as produced by the parser, into a
//! semantically rich model ready to be lowered for execution.

use syntax;

pub struct Expression {
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
