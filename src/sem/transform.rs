//! Semantic Transforms
//!
//! This module contains the logic for converting a syntax expression
//! tree into a semantic one. The main entry point for this module is
//! the [`transform_expression`] function.
//!
//! [`transform_expression`]: ./function.transform_expression.html

use crate::syntax::operators::InfixOp;
use crate::syntax::{Constant, Expression as SyntaxExpr};

use super::super::compile::{CompError, CompResult};
use super::operators::find_builtin_op;
use super::sem_ctx::SemCtx;
use super::tree::*;
use super::types::{BuiltinType, Typ};

/// Transform Expression
///
/// Convert a syntax expression into a symantic one.
pub fn transform_expression(ctx: &mut SemCtx, expr: SyntaxExpr) -> CompResult<Expression> {
    match expr {
        SyntaxExpr::Identifier(i) => {
            let typ = ctx.find_local(&i);
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
            let transformed = seq
                .into_iter()
                .map(|e| transform_expression(ctx, e))
                .collect::<CompResult<Vec<_>>>()?;
            let typ = transformed.last().and_then(|e| e.typ);
            Ok(Expression::new(ExpressionKind::Sequence(transformed), typ))
        }
        SyntaxExpr::Prefix(op, expr) => {
            let transformed = transform_expression(ctx, *expr)?;
            let typ = transformed.typ;
            Ok(Expression::new(
                ExpressionKind::Prefix(op, Box::new(transformed)),
                typ,
            ))
        }
        SyntaxExpr::Infix(lhs, op, rhs) => {
            let rhs = transform_expression(ctx, *rhs)?;
            match op {
                InfixOp::Assign => {
                    if let SyntaxExpr::Identifier(id) = *lhs {
                        let id_typ = ctx.find_local(&id).or(rhs.typ);
                        Ok(Expression::new(
                            ExpressionKind::Assignment(id, Box::new(rhs)),
                            id_typ,
                        ))
                    } else {
                        Err(CompError::from(String::from(
                            "left hand side of an assignment must be an identifier",
                        )))
                    }
                }
                _ => {
                    let lhs = transform_expression(ctx, *lhs)?;

                    let lhs_typ = relax_ty(lhs.typ);
                    let rhs_typ = relax_ty(rhs.typ);
                    let found = find_builtin_op(op, lhs_typ, rhs_typ);

                    if let Some(operator) = found {
                        Ok(Expression::new(
                            ExpressionKind::Infix(Box::new(lhs), op, Box::new(rhs)),
                            Some(operator.result_typ),
                        ))
                    } else {
                        Err(CompError::from(format!(
                            "Use of operator `{:?}` with invalid arguments",
                            op
                        )))
                    }
                }
            }
        }
        SyntaxExpr::Index(expr, index) => {
            let expr = transform_expression(ctx, *expr)?;
            let index = transform_expression(ctx, *index)?;
            // FIXME: Get the type from the thing being indexed into.
            Ok(Expression::new(
                ExpressionKind::Index(Box::new(expr), Box::new(index)),
                None,
            ))
        }
        SyntaxExpr::IfThenElse(iff, then, els) => {
            let iff = transform_expression(ctx, *iff)?;
            let then = transform_expression(ctx, *then)?;
            let els = transform_expression(ctx, *els)?;
            // FIXME: Check that the type of the then and else
            // branches match up.
            let typ = then.typ;
            Ok(Expression::new(
                ExpressionKind::IfThenElse(Box::new(iff), Box::new(then), Box::new(els)),
                typ,
            ))
        }
        SyntaxExpr::Loop(condition, body) => {
            let condition = transform_expression(ctx, *condition)?;
            let body = transform_expression(ctx, *body)?;
            Ok(Expression::new(
                ExpressionKind::Loop(Box::new(condition), Box::new(body)),
                Some(Typ::Unit),
            ))
        }
        SyntaxExpr::Print(inner) => {
            let transformed = transform_expression(ctx, *inner)?;
            let typ = transformed.typ;
            Ok(Expression::new(
                ExpressionKind::Print(Box::new(transformed)),
                typ,
            ))
        }
        SyntaxExpr::Function(ident, ret_ty, params, body) => {
            ctx.push_scope();

            let params = params
                .into_iter()
                .map(|p| {
                    // FIXME: this will squash type declaration
                    // errors in params. All params _must_ have a
                    // type.
                    let typ = p.typ.and_then(|t| ctx.sem_ty(t));
                    ctx.add_local(p.id.clone(), typ.unwrap());
                    VarDecl {
                        ident: p.id,
                        ty: typ,
                    }
                })
                .collect();

            let fn_decl = FnDecl {
                ident,
                ret_ty: ensure_ty(ctx.sem_ty(ret_ty))?,
                params: params,
                body: Box::new(transform_expression(ctx, *body)?),
            };

            ctx.pop_scope();
            // TODO: When we start storing funciton types in the
            // context then there will need to be two passes for
            // transformation.
            let typ = None;
            Ok(Expression::new(ExpressionKind::Function(fn_decl), typ))
        }
        SyntaxExpr::Declaration(tid, is_mut, initialiser) => {
            let initialiser = transform_expression(ctx, *initialiser)?;
            // TODO: This is a mess. Need to make ensuring type match
            // between delcaration and expression the responsibility
            // of something else. Better than not checking it at all
            // though I suppose.
            let typ = match tid.typ {
                Some(ty_ref) => {
                    let declared_ty = ensure_ty(ctx.sem_ty(ty_ref))?;
                    if Some(declared_ty) != initialiser.typ {
                        return Err(CompError::from(format!(
                            "Initialiser doesn't match declaration type for {}",
                            tid.id
                        )));
                    }
                    Some(declared_ty)
                }
                None => initialiser.typ,
            };
            ctx.add_local(tid.id.clone(), typ.unwrap_or(Typ::Unknown));
            Ok(Expression::new(
                ExpressionKind::Declaration(
                    VarDecl {
                        ident: tid.id,
                        ty: typ,
                    },
                    is_mut,
                    Box::new(initialiser),
                ),
                typ,
            ))
        }
        SyntaxExpr::Call(callee, args) => {
            let callee = transform_expression(ctx, *callee)?;
            let args = args
                .into_iter()
                .map(|a| transform_expression(ctx, a))
                .collect::<CompResult<Vec<_>>>()?;

            // FIXME: Look up the type of the function
            let typ = None;
            Ok(Expression::new(
                ExpressionKind::Call(Box::new(callee), args),
                typ,
            ))
        }
    }
}

/// Relax Type
///
/// Returns the type if it is present, or assumes `Number`
/// otherwise. This is a hack to deal with partially-typed trees and
/// should be removed.
fn relax_ty(maybe_typ: Option<Typ>) -> Typ {
    maybe_typ.unwrap_or(Typ::Builtin(BuiltinType::Number))
}

/// Ensure Type
///
/// Checks that the given type lookup succeeded. Probably should be
/// part of the `SemCtx`.
fn ensure_ty(maybe_ty: Option<Typ>) -> CompResult<Typ> {
    // TODO: Improve error reporting here.
    maybe_ty.ok_or(CompError::from("Reference to undefined type".to_string()))
}
