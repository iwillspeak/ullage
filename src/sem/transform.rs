//! Semantic Transforms
//!
//! This module contains the logic for converting a syntax expression
//! tree into a semantic one. The main entry point for this module is
//! the [`transform_expression`] function.
//!
//! [`transform_expression`]: ./function.transform_expression.html

use syntax;

use super::tree::*;

/// Transform Expression
///
/// Convert a syntax expression into a symantic one.
pub fn transform_expression(expr: syntax::Expression) -> Expression {
    Expression::new(ExpressionKind::Fixme(expr), None)
}
