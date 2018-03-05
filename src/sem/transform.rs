//! Semantic Transforms
//!
//! This module contains the logic for converting a syntax expression
//! tree into a semantic one. The main entry point for this module is
//! the [`transform_expression`] function.
//!
//! [`transform_expression`]: ./function.transform_expression.html

use syntax::{Constant, Expression as SyntaxExpr};
use syntax::operators::InfixOp;

use super::super::compile::{Error, Result};
use super::types::{BuiltinType, Typ};
use super::tree::*;

/// Transform Expression
///
/// Convert a syntax expression into a symantic one.
pub fn transform_expression(expr: SyntaxExpr) -> Result<Expression> {
    match expr {
        SyntaxExpr::Identifier(i) => {
            // FIXME: need to keep track of types when transforming
            // expressions so that this can be looked up properly.
            let typ = None;
            Ok(Expression::new(ExpressionKind::Identifier(i), typ))
        }
        SyntaxExpr::Literal(c) => {
            let typ = Typ::Builtin(match &c {
                &Constant::Bool(_) => BuiltinType::Bool,
                &Constant::Number(_) => BuiltinType::Number,
                &Constant::String(_) => BuiltinType::String,
            });
            Ok(Expression::new(ExpressionKind::Literal(c), Some(typ)))
        }
        SyntaxExpr::Sequence(seq) => {
            let transformed = seq.into_iter()
                .map(transform_expression)
                .collect::<Result<Vec<_>>>()?;
            let typ = transformed.last().and_then(|e| e.typ.clone());
            Ok(Expression::new(ExpressionKind::Sequence(transformed), typ))
        }
        SyntaxExpr::Prefix(op, expr) => {
            let transformed = transform_expression(*expr)?;
            let typ = transformed.typ.clone();
            Ok(Expression::new(
                ExpressionKind::Prefix(op, Box::new(transformed)),
                typ,
            ))
        }
        SyntaxExpr::Infix(lhs, op, rhs) => {
            let rhs = transform_expression(*rhs)?;
            match op {
                InfixOp::Assign => {
                    if let SyntaxExpr::Identifier(id) = *lhs {
                        // TODO: look up the type of the identifier
                        Ok(Expression::new(
                            ExpressionKind::Assignment(id, Box::new(rhs)),
                            None,
                        ))
                    } else {
                        Err(Error::Generic(String::from(
                            "left hand side of an assignment must be an identifier",
                        )))
                    }
                }
                _ => {
                    let lhs = transform_expression(*lhs)?;
                    // TODO: Promote the types somehow?
                    let subexpr_typ = lhs.typ.clone().or(rhs.typ.clone());
                    let typ = match op {
                        InfixOp::Eq | InfixOp::NotEq | InfixOp::Gt | InfixOp::Lt => {
                            Some(Typ::Builtin(BuiltinType::Bool))
                        }
                        _ => subexpr_typ,
                    };
                    Ok(Expression::new(
                        ExpressionKind::Infix(Box::new(lhs), op, Box::new(rhs)),
                        typ,
                    ))
                }
            }
        }
        SyntaxExpr::Index(expr, index) => {
            let expr = transform_expression(*expr)?;
            let index = transform_expression(*index)?;
            // FIXME: Get the type from the thing being indexed into.
            Ok(Expression::new(
                ExpressionKind::Index(Box::new(expr), Box::new(index)),
                None,
            ))
        }
        SyntaxExpr::IfThenElse(iff, then, els) => {
            let iff = transform_expression(*iff)?;
            let then = transform_expression(*then)?;
            let els = transform_expression(*els)?;
            // FIXME: Check that the type of the then and else
            // branches match up.
            let typ = then.typ.clone();
            Ok(Expression::new(
                ExpressionKind::IfThenElse(Box::new(iff), Box::new(then), Box::new(els)),
                typ,
            ))
        }
        SyntaxExpr::Loop(condition, body) => {
            let condition = transform_expression(*condition)?;
            let body = transform_expression(*body)?;
            Ok(Expression::new(
                ExpressionKind::Loop(Box::new(condition), Box::new(body)),
                Some(Typ::Unit),
            ))
        }
        SyntaxExpr::Print(inner) => {
            let transformed = transform_expression(*inner)?;
            let typ = transformed.typ.clone();
            Ok(Expression::new(
                ExpressionKind::Print(Box::new(transformed)),
                typ,
            ))
        }
        SyntaxExpr::Declaration(tid, is_mut, initialiser) => {
            let initialiser = transform_expression(*initialiser)?;
            let typ = initialiser.typ.clone();
            // FIXME: check the type matches the variable declaration
            Ok(Expression::new(
                ExpressionKind::Declaration(tid.id, is_mut, Box::new(initialiser)),
                typ,
            ))
        }
        expr => Ok(Expression::new(ExpressionKind::Fixme(expr), None)),
    }
}
