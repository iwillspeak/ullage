//! Expression Lowering
//!
//! This module is responsible for taking Expressions and lowering
//! them to LLVM.

use syntax::Constant;
use sem::{BuiltinType, Expression, ExpressionKind, Typ};
use syntax::operators::{InfixOp, PrefixOp};
use low_loader::prelude::*;

use super::lower_context::LowerContext;
use super::error::*;

use std::collections::HashMap;

type Local = (bool, LLVMValueRef);

impl From<InfixOp> for Predicate {
    fn from(op: InfixOp) -> Self {
        match op {
            InfixOp::Eq => Predicate::Eq,
            InfixOp::NotEq => Predicate::Neq,
            InfixOp::Lt => Predicate::Lt,
            InfixOp::Gt => Predicate::Gt,
            _ => panic!("Infix op {:?} is not a predicate", op),
        }
    }
}

/// Lower an Expression as the Main Method
///
/// Takes a given tree of expressions and adds a new `main` function
/// to the LLVM Context. When called `main` will compute the value of
/// the expression and return `0`.
pub fn lower_as_main(ctx: &mut LowerContext, expr: Expression) -> Result<Function> {
    let int_type = ctx.llvm_ctx.int_type(64);
    let mut fun = ctx.llvm_ctx
        .add_function(ctx.module, "main", int_type, &mut []);
    let bb = ctx.llvm_ctx.add_block(&mut fun, "entry");

    let mut builder = ctx.llvm_ctx.add_builder();
    builder.position_at_end(bb);

    lower_expression(ctx, &mut fun, &mut builder, expr)?;

    builder.build_ret(ctx.llvm_ctx.const_int(0));

    Ok(fun)
}

/// Lower an Expression to LLVM
///
/// Takes the given expression and lowers it to LLVM IR. This is just
/// a thin wrapper around the internal expresison visitor which is
/// responsible for actually lowering the expression to an
/// LLVMValue. This function discards the resulting value and is
/// intended to be used at a higher level when the result o the
/// expression is not required for anything.
///
/// # Arguments
///
///  * `expr` - The `Expression` to lower.
///
/// # Returns
///
/// A `Result` indicating if the expression was lowered successfully.
pub fn lower_expression<'a>(
    ctx: &mut LowerContext,
    fun: &mut Function,
    builder: &mut Builder,
    expr: Expression,
) -> Result<()> {
    let mut vars = HashMap::new();

    add_decls(ctx, &expr);

    lower_internal(ctx, fun, builder, &mut vars, expr)?;
    Ok(())
}

/// Add the Declarations form the Expression to the Context
///
/// Loops through the tree of expressions and adds LLVM function
/// declarations for each defined function. This ensures mutal
/// recursion is possible.
fn add_decls(ctx: &mut LowerContext, expr: &Expression) {
    match expr.kind {
        ExpressionKind::Sequence(ref exprs) => for expr in exprs.iter() {
            add_decls(ctx, expr);
        },
        ExpressionKind::Function(ref fn_decl) => {
            let ret = ctx.llvm_type(&fn_decl.ret_ty).unwrap();
            let mut params = fn_decl
                .params
                .iter()
                .map(|p| {
                    ctx.llvm_type(
                        &(p.ty
                            .clone() // FIXME: UGH
                            .unwrap_or(Typ::Builtin(BuiltinType::Number))),
                    ).unwrap()
                })
                .collect::<Vec<_>>();
            ctx.llvm_ctx
                .add_function(ctx.module, &fn_decl.ident, ret, &mut params[..]);
        }
        _ => (),
    }
}

/// Internal Lowering of `Expression`s
///
/// Converts an `Expression` to LLVM IR
pub fn lower_internal(
    ctx: &mut LowerContext,
    fun: &mut Function,
    builder: &mut Builder,
    vars: &mut HashMap<String, Local>,
    expr: Expression,
) -> Result<LLVMValueRef> {
    match expr.kind {
        ExpressionKind::Identifier(id) => match vars.get(&id) {
            Some(&(is_mut, val)) => Ok(if is_mut { builder.build_load(val) } else { val }),
            None => Err(Error::from(format!("Reference to undefined '{}'", id))),
        },
        ExpressionKind::Literal(constant) => match constant {
            Constant::Number(n) => Ok(ctx.llvm_ctx.const_int(n)),
            Constant::Bool(b) => Ok(ctx.llvm_ctx.const_bool(b)),
            Constant::String(_) => unimplemented!(),
        },
        ExpressionKind::Prefix(op, inner) => {
            let val = lower_internal(ctx, fun, builder, vars, *inner)?;
            Ok(match op {
                PrefixOp::Negate => builder.build_neg(val),
                PrefixOp::Not => builder.build_not(val),
            })
        }
        ExpressionKind::Infix(lhs, op, rhs) => {
            let lhs_val = lower_internal(ctx, fun, builder, vars, *lhs)?;
            let rhs_val = lower_internal(ctx, fun, builder, vars, *rhs)?;
            let val = match op {
                InfixOp::Add => builder.build_add(lhs_val, rhs_val),
                InfixOp::Sub => builder.build_sub(lhs_val, rhs_val),
                InfixOp::Mul => builder.build_mul(lhs_val, rhs_val),
                InfixOp::Div => builder.build_sdiv(lhs_val, rhs_val),

                InfixOp::Eq | InfixOp::NotEq | InfixOp::Lt | InfixOp::Gt => {
                    builder.build_icmp(Predicate::from(op), lhs_val, rhs_val)
                }

                InfixOp::Assign => unreachable!(),
            };
            Ok(val)
        }
        ExpressionKind::Assignment(id, expression) => {
            let val = lower_internal(ctx, fun, builder, vars, *expression)?;
            match vars.get(&id) {
                Some(&(true, var)) => {
                    builder.build_store(val, var);
                    Ok(val)
                }
                _ => Err(Error::from(format!("can't assign to {}", id))),
            }
        }
        ExpressionKind::Call(callee, args) => {
            if let ExpressionKind::Identifier(name) = callee.kind {
                match ctx.module.find_function(&name) {
                    Some(function) => {
                        let mut args = args.into_iter()
                            .map(|arg| lower_internal(ctx, fun, builder, vars, arg))
                            .collect::<Result<Vec<_>>>()?;
                        let call_res = builder.build_call(&function, &mut args);
                        Ok(call_res)
                    }
                    None => Err(Error::from(format!("Can't find function '{}", name))),
                }
            } else {
                unimplemented!()
            }
        }
        ExpressionKind::Index(_expr, _index) => unimplemented!(),
        ExpressionKind::IfThenElse(iff, then, els) => {
            let cond = lower_internal(ctx, fun, builder, vars, *iff)?;

            let typ = ctx.llvm_ctx.int_type(64);
            let ret = builder.build_alloca(typ, "if");

            let thenblock = ctx.llvm_ctx.add_block(fun, "thenblock");
            let elsblock = ctx.llvm_ctx.add_block(fun, "elseblock");
            let joinblock = ctx.llvm_ctx.add_block(fun, "joinblock");

            builder.build_cond_br(cond, thenblock, elsblock);

            builder.position_at_end(thenblock);
            let then = lower_internal(ctx, fun, builder, vars, *then)?;
            builder.build_store(then, ret);
            builder.build_br(joinblock);

            builder.position_at_end(elsblock);
            let els = lower_internal(ctx, fun, builder, vars, *els)?;
            builder.build_store(els, ret);
            builder.build_br(joinblock);

            builder.position_at_end(joinblock);
            Ok(builder.build_load(ret))
        }
        ExpressionKind::Function(fn_decl) => {
            let mut fun = ctx.module.find_function(&fn_decl.ident).expect(&format!(
                "missing function declaration '{}'",
                &fn_decl.ident
            ));
            let bb = ctx.llvm_ctx.add_block(&mut fun, "body");
            let mut builder = ctx.llvm_ctx.add_builder();
            builder.position_at_end(bb);

            let mut vars = fn_decl
                .params
                .into_iter()
                .enumerate()
                .map(|(i, p)| (p.ident, (false, fun.get_param(i as u32))))
                .collect::<HashMap<String, Local>>();

            let body = lower_internal(ctx, &mut fun, &mut builder, &mut vars, *fn_decl.body)?;
            builder.build_ret(body);
            fun.verify_or_panic();
            Ok(unsafe { fun.as_raw() })
        }
        ExpressionKind::Loop(cond, body) => {
            let condblock = ctx.llvm_ctx.add_block(fun, "condblock");
            let bodyblock = ctx.llvm_ctx.add_block(fun, "whilebody");
            let joinblock = ctx.llvm_ctx.add_block(fun, "joinblock");

            builder.build_br(condblock);
            builder.position_at_end(condblock);

            let cond = lower_internal(ctx, fun, builder, vars, *cond)?;
            builder.build_cond_br(cond, bodyblock, joinblock);

            builder.position_at_end(bodyblock);
            lower_internal(ctx, fun, builder, vars, *body)?;
            builder.build_br(condblock);

            builder.position_at_end(joinblock);

            Ok(cond)
        }
        ExpressionKind::Sequence(seq) => seq.into_iter()
            .map(|e| lower_internal(ctx, fun, builder, vars, e))
            .last()
            .unwrap(),
        ExpressionKind::Print(inner) => {
            let val = lower_internal(ctx, fun, builder, vars, *inner)?;
            let (to_format, format) = expr.typ
                .and_then(|t| fmt_from_type(t, ctx, fun, builder, val))
                .unwrap_or_else(|| fmt_from_llvm(ctx, fun, builder, val));
            fmt(ctx, builder, to_format, format);
            Ok(val)
        }
        ExpressionKind::Declaration(decl, is_mut, initialiser) => {
            let initialiser = lower_internal(ctx, fun, builder, vars, *initialiser)?;
            let value = if is_mut {
                // FIXME: look the type up properly
                let typ = ctx.llvm_ctx.int_type(64);
                let stackloc = builder.build_alloca(typ, &decl.ident);
                builder.build_store(initialiser, stackloc);
                stackloc
            } else {
                initialiser
            };
            vars.insert(decl.ident, (is_mut, value));
            Ok(initialiser)
        }
    }
}

/// Format with Printf
///
/// Constructs a call to the `printf` function using the given format
/// name string to write `to_format` to the standard output. This
/// method is intended to be used as part of the `print`
/// operator/expression. For a list of the supported format string
/// names check out `add_core_decls`.
fn fmt(ctx: &mut LowerContext, builder: &mut Builder, to_format: LLVMValueRef, format_name: &str) {
    let format = ctx.module.find_global(format_name).unwrap();
    let format_ptr = builder.build_gep(
        format,
        &mut [ctx.llvm_ctx.const_int(0), ctx.llvm_ctx.const_int(0)],
    );
    let mut args = vec![format_ptr, to_format];
    let printf = ctx.module.find_function("printf").unwrap();
    builder.build_call(&printf, &mut args);
}

/// Get Format String from Expression Type
fn fmt_from_type(
    typ: Typ,
    ctx: &mut LowerContext,
    fun: &mut Function,
    builder: &mut Builder,
    val: LLVMValueRef,
) -> Option<(LLVMValueRef, &'static str)> {
    match typ {
        Typ::Builtin(BuiltinType::Bool) => {
            let formatted = fmt_convert_bool(ctx, fun, builder, val);
            Some((formatted, "printf_cstr_format"))
        }
        Typ::Builtin(BuiltinType::Number) => Some((val, "printf_num_format")),
        _ => None,
    }
}

/// Format Bool to String
///
/// Prepares a bool value to be formatted. This compiles down to a
/// ternary with two fixed strings `true` and `false`.
fn fmt_convert_bool(
    ctx: &mut LowerContext,
    fun: &mut Function,
    builder: &mut Builder,
    val: LLVMValueRef,
) -> LLVMValueRef {
    let cstr_type = ctx.llvm_ctx.cstr_type();
    let temp = builder.build_alloca(cstr_type, "bool_formatted");
    let true_bb = ctx.llvm_ctx.add_block(fun, "true");
    let false_bb = ctx.llvm_ctx.add_block(fun, "false");
    let join_bb = ctx.llvm_ctx.add_block(fun, "join");

    builder.build_cond_br(val, true_bb, false_bb);

    builder.position_at_end(true_bb);
    let true_s = ctx.module
        .find_global("print_true")
        .expect("could't find `print_true`");
    let true_s = builder.build_bitcast(true_s, cstr_type, "true");
    builder.build_store(true_s, temp);
    builder.build_br(join_bb);

    builder.position_at_end(false_bb);
    let false_s = ctx.module
        .find_global("print_false")
        .expect("couldn't find `print_false`");
    let false_s = builder.build_bitcast(false_s, cstr_type, "false");
    builder.build_store(false_s, temp);
    builder.build_br(join_bb);

    builder.position_at_end(join_bb);

    builder.build_load(temp)
}

/// FIXME: Remove this function
fn fmt_from_llvm(
    ctx: &mut LowerContext,
    fun: &mut Function,
    builder: &mut Builder,
    val: LLVMValueRef,
) -> (LLVMValueRef, &'static str) {
    match Type::from(ctx.llvm_ctx.get_type(val)) {
        Type::Int(1) => {
            let formatted = fmt_convert_bool(ctx, fun, builder, val);
            (formatted, "printf_cstr_format")
        }
        Type::Int(_) => (val, "printf_num_format"),
        _ => unimplemented!(),
    }
}
