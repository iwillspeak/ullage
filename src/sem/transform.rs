//! Semantic Transforms
//!
//! This module contains the logic for converting a syntax expression
//! tree into a semantic one. The main entry point for this module is
//! the [`transform_expression`] function.
//!
//! [`transform_expression`]: ./function.transform_expression.html

use syntax::{Constant,Expression as SyntaxExpr};

use super::types::{Typ, BuiltinType};
use super::tree::*;

/// Transform Expression
///
/// Convert a syntax expression into a symantic one.
pub fn transform_expression(expr: SyntaxExpr) -> Expression {
    match expr {
        SyntaxExpr::Literal(c) => {
            let typ = Typ::Builtin(match &c {
                &Constant::Bool(_) => BuiltinType::Bool,
                &Constant::Number(_) => BuiltinType::Number,
                &Constant::String(_) => BuiltinType::String,
            });
            Expression::new(
                ExpressionKind::Literal(c),
                Some(typ))
        }
        expr => Expression::new(ExpressionKind::Fixme(expr), None)
    }
}
