//! Semantic Analysis and Translation
//!
//! THis module is responsible for translating the syntactic
//! representation of a program, as produced by the parser, into a
//! semantically rich model ready to be lowered for execution.

use syntax;

pub struct Expression {
    pub expr: syntax::Expression,
}

/// Transform Expressions
///
/// Convert a group of expressions into
pub fn transform_expressions(exprs: Vec<syntax::Expression>) -> Vec<Expression> {
    exprs.into_iter()
        .map(transform_expression)
        .collect()
}

pub fn transform_expression(expr: syntax::Expression) -> Expression {
    Expression {
        expr: expr,
    }
}

#[cfg(test)]
pub mod test {

    
}
