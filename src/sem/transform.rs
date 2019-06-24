//! Semantic Transforms
//!
//! This module contains the logic for converting a syntax expression
//! tree into a semantic one. The main entry point for this module is
//! the [`transform_expression`] function.
//!
//! [`transform_expression`]: ./function.transform_expression.html

use crate::diag::Diagnostic;
use crate::syntax::text::Ident;
use crate::syntax::tree::TokenKind;
use crate::syntax::{Constant, Expression as SyntaxExpr, InfixOp, PrefixOp, SyntaxNode, VarStyle};

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
            let typ = ctx.find_local(i.ident);
            let id_str = ctx.source().interned_value(i.ident);
            Ok(Expression::new(ExpressionKind::Identifier(id_str), typ))
        }
        SyntaxExpr::Literal(c) => {
            let c = c.value;
            let typ = Typ::Builtin(match c {
                Constant::Bool(_) => BuiltinType::Bool,
                Constant::Number(_) => BuiltinType::Number,
                Constant::String(_) => BuiltinType::String,
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
        SyntaxExpr::Prefix(pref) => {
            let expr = pref.inner;
            let transformed = transform_expression(ctx, *expr)?;
            let typ = transformed.typ;
            Ok(Expression::new(
                ExpressionKind::Prefix(pref.op, Box::new(transformed)),
                typ,
            ))
        }
        SyntaxExpr::Infix(infix) => {
            let rhs = transform_expression(ctx, *infix.right)?;
            match infix.op {
                InfixOp::Assign => {
                    if let SyntaxExpr::Identifier(id) = *infix.left {
                        let id_typ = ctx.find_local(id.ident).or(rhs.typ);
                        let id = ctx.source().interned_value(id.ident);
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
                    let lhs = transform_expression(ctx, *infix.left)?;

                    let lhs_typ = relax_ty(lhs.typ);
                    let rhs_typ = relax_ty(rhs.typ);
                    let found = find_builtin_op(infix.op, lhs_typ, rhs_typ);

                    if let Some(operator) = found {
                        Ok(Expression::new(
                            ExpressionKind::Infix(Box::new(lhs), infix.op, Box::new(rhs)),
                            Some(operator.result_typ),
                        ))
                    } else {
                        Err(CompError::from(format!(
                            "Use of operator `{:?}` with invalid arguments",
                            infix.op
                        )))
                    }
                }
            }
        }
        SyntaxExpr::Index(index) => {
            let expr = transform_expression(ctx, *index.indexee)?;
            let index = transform_expression(ctx, *index.index)?;
            // FIXME: Get the type from the thing being indexed into.
            Ok(Expression::new(
                ExpressionKind::Index(Box::new(expr), Box::new(index)),
                None,
            ))
        }
        SyntaxExpr::IfThenElse(expr) => {
            let cond = transform_expression(ctx, *expr.cond)?;
            let if_true = transform_expression(ctx, *expr.if_true)?;
            let if_false = transform_expression(ctx, *expr.if_false)?;
            // FIXME: Check that the `then` and `else` types match
            let typ = if_true.typ;
            Ok(Expression::new(
                ExpressionKind::IfThenElse(Box::new(cond), Box::new(if_true), Box::new(if_false)),
                typ,
            ))
        }
        SyntaxExpr::Loop(loop_expr) => {
            let mut condition = transform_expression(ctx, *loop_expr.condition)?;
            if loop_expr.kw_token.kind == TokenKind::Word(Ident::Until) {
                let typ = condition.typ;
                condition = Expression::new(
                    ExpressionKind::Prefix(PrefixOp::Not, Box::new(condition)),
                    typ,
                );
            }
            let body = transform_expression(ctx, *loop_expr.body.contents)?;
            Ok(Expression::new(
                ExpressionKind::Loop(Box::new(condition), Box::new(body)),
                Some(Typ::Unit),
            ))
        }
        SyntaxExpr::Print(print) => {
            let transformed = transform_expression(ctx, *print.inner)?;
            let typ = transformed.typ;
            Ok(Expression::new(
                ExpressionKind::Print(Box::new(transformed)),
                typ,
            ))
        }
        SyntaxExpr::Function(func) => {
            ctx.push_scope();

            let params = func
                .params
                .into_iter()
                .map(|p| {
                    let p = p.as_inner();
                    let typ = match p.typ.as_ref() {
                        Some(anno) => match ctx.sem_ty(&anno.type_ref) {
                            Some(ty) => ty,
                            None => {
                                ctx.emit(Diagnostic::new(
                                    "reference to undefined parameter type",
                                    anno.type_ref.span(),
                                ));
                                Typ::Unknown
                            }
                        },
                        None => {
                            ctx.emit(Diagnostic::new("parameter missing type", p.id_tok.span()));
                            Typ::Unknown
                        }
                    };
                    ctx.add_local(p.id, typ);
                    VarDecl {
                        ident: ctx.source().interned_value(p.id),
                        ty: Some(typ),
                    }
                })
                .collect();

            let fn_decl = FnDecl {
                ident: ctx.source().interned_value(func.identifier),
                ret_ty: ensure_ty(ctx.sem_ty(&func.return_type.type_ref))?,
                params,
                body: Box::new(transform_expression(ctx, *func.body.contents)?),
            };

            ctx.pop_scope();
            // TODO: When we start storing funciton types in the
            // context then there will need to be two passes for
            // transformation.
            let typ = None;
            Ok(Expression::new(ExpressionKind::Function(fn_decl), typ))
        }
        SyntaxExpr::Declaration(decl) => {
            let initialiser = transform_expression(ctx, *decl.initialiser)?;
            // TODO: This is a mess. Need to make ensuring type match
            // between delcaration and expression the responsibility
            // of something else. Better than not checking it at all
            // though I suppose.
            let typ = match decl.id.typ {
                Some(ty) => {
                    let declared_ty = ensure_ty(ctx.sem_ty(&ty.type_ref))?;
                    if Some(declared_ty) != initialiser.typ {
                        return Err(CompError::from(format!(
                            "Initialiser doesn't match declaration type for '{}'",
                            ctx.source().interned_value(decl.id.id)
                        )));
                    }
                    Some(declared_ty)
                }
                None => initialiser.typ,
            };
            let is_mut = decl.style == VarStyle::Mutable;
            ctx.add_local(decl.id.id, typ.unwrap_or(Typ::Unknown));
            Ok(Expression::new(
                ExpressionKind::Declaration(
                    VarDecl {
                        ident: ctx.source().interned_value(decl.id.id),
                        ty: typ,
                    },
                    is_mut,
                    Box::new(initialiser),
                ),
                typ,
            ))
        }
        SyntaxExpr::Call(call) => {
            let callee = transform_expression(ctx, *call.callee)?;
            let args = call
                .arguments
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
        SyntaxExpr::Grouping(group) => transform_expression(ctx, *group.inner),
    }
}

/// Relax Type
///
/// Returns the type if it is present, or assumes `Number`
/// otherwise. This is a hack to deal with partially-typed trees and
/// should be removed.
///
/// HAXX: This should be removed once proper binding takes
/// place. Failure to resolve the type of an inner expression should
/// result in an error expression so we never need to deal with
/// partially typed trees.
fn relax_ty(maybe_typ: Option<Typ>) -> Typ {
    maybe_typ.unwrap_or(Typ::Builtin(BuiltinType::Number))
}

/// Ensure Type
///
/// Checks that the given type lookup succeeded. Probably should be
/// part of the `SemCtx`.
fn ensure_ty(maybe_ty: Option<Typ>) -> CompResult<Typ> {
    // TODO: Improve error reporting here.
    maybe_ty.ok_or_else(|| CompError::from("Reference to undefined type".to_string()))
}
