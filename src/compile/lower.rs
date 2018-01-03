//! Expression Lowering
//!
//! This module is responsible for taking Expressions and lowering
//! them to LLVM.

use syntax::{self, Constant};
use sem::Expression;
use syntax::operators::{PrefixOp, InfixOp};
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

pub fn lower_as_main(ctx: &mut LowerContext, expr: Expression) -> Result<Function> {
    // TODO: HAXX. Should pass on the `LowerContext` to `lower_expression`.
    let ref mut module = ctx.module;
    let ref mut ctx = ctx.llvm_ctx;

    let int_type = ctx.int_type(64);
    let mut fun = ctx.add_function(module, "main", int_type, &mut []);
    let bb = ctx.add_block(&mut fun, "entry");

    let mut builder = ctx.add_builder();
    builder.position_at_end(bb);

    lower_expression(ctx, module, &mut fun, &mut builder, expr)?;

    builder.build_ret(ctx.const_int(0));

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
    ctx: &mut Context,
    module: &mut Module,
    fun: &mut Function,
    builder: &mut Builder,
    expr: Expression,
) -> Result<()> {
    let mut vars = HashMap::new();

    add_decls(ctx, module, &expr.expr);

    lower_internal(ctx, module, fun, builder, &mut vars, expr.expr)?;
    Ok(())
}

fn add_decls(ctx: &mut Context, module: &mut Module, expr: &syntax::Expression) {

    match expr {
        &syntax::Expression::Sequence(ref exprs) => {
            for expr in exprs.iter() {
                add_decls(ctx, module, expr)
            }
        }
        &syntax::Expression::Function(ref name, ref ret, ref params, ref _body) => {
            let ret = ctx.named_type(ret.simple_name());
            let mut params = params
                .iter()
                .map(|p| ctx.named_type(p.typ.as_ref().unwrap().simple_name()))
                .collect::<Vec<_>>();
            ctx.add_function(module, &name, ret, &mut params[..]);
        }
        _ => {}
    }
}

/// Internal Lowering of `Expression`s
///
/// Converts an `Expression` to LLVM IR
pub fn lower_internal<'a>(
    ctx: &mut Context,
    module: &mut Module,
    fun: &mut Function,
    builder: &mut Builder,
    vars: &mut HashMap<String, Local>,
    expr: syntax::Expression,
) -> Result<LLVMValueRef> {
    match expr {
        syntax::Expression::Identifier(id) => {
            match vars.get(&id) {
                Some(&(is_mut, val)) => Ok(if is_mut { builder.build_load(val) } else { val }),
                None => Err(Error::from(format!("Reference to undefined '{}'", id))),
            }
        }
        syntax::Expression::Literal(lit) => {
            match lit {
                Constant::Number(n) => Ok(ctx.const_int(n)),
                Constant::Bool(b) => Ok(ctx.const_bool(b)),
                Constant::String(_) => unimplemented!(),
            }
        }
        syntax::Expression::Prefix(op, expr) => {
            let val = lower_internal(ctx, module, fun, builder, vars, *expr)?;
            let val = match op {
                PrefixOp::Negate => builder.build_neg(val),
                PrefixOp::Not => builder.build_not(val),
            };
            Ok(val)
        }
        syntax::Expression::Infix(lhs, op, rhs) => {
            let rhs_val = lower_internal(ctx, module, fun, builder, vars, *rhs)?;

            // TODO: maybe assignment should be a different node in the AST?
            if op == InfixOp::Assign {
                if let syntax::Expression::Identifier(id) = *lhs {
                    match vars.get(&id) {
                        Some(&(true, var)) => {
                            builder.build_store(rhs_val, var);
                            Ok(rhs_val)
                        }
                        _ => Err(Error::from(format!("can't assign to {}", id))),
                    }
                } else {
                    Err(Error::Generic(String::from(
                        "left hand side of an assignment must be an identifier",
                    )))
                }
            } else {
                let lhs_val = lower_internal(ctx, module, fun, builder, vars, *lhs)?;
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
        }
        syntax::Expression::Print(inner) => {
            let val = lower_internal(ctx, module, fun, builder, vars, *inner)?;
            let (to_format, format_name) = match Type::from(ctx.get_type(val)) {
                Type::Int(1) => {
                    let cstr_type = ctx.cstr_type();
                    let temp = builder.build_alloca(cstr_type, "bool_formatted");
                    let true_bb = ctx.add_block(fun, "true");
                    let false_bb = ctx.add_block(fun, "false");
                    let join_bb = ctx.add_block(fun, "join");

                    builder.build_cond_br(val, true_bb, false_bb);

                    builder.position_at_end(true_bb);
                    let true_s = module.find_global("print_true").expect(
                        "could't find `print_true`",
                    );
                    let true_s = builder.build_bitcast(true_s, cstr_type, "true");
                    builder.build_store(true_s, temp);
                    builder.build_br(join_bb);

                    builder.position_at_end(false_bb);
                    let false_s = module.find_global("print_false").expect(
                        "couldn't find `print_false`",
                    );
                    let false_s = builder.build_bitcast(false_s, cstr_type, "false");
                    builder.build_store(false_s, temp);
                    builder.build_br(join_bb);

                    builder.position_at_end(join_bb);

                    let formatted = builder.build_load(temp);
                    (formatted, "printf_cstr_format")
                }
                Type::Int(_) => (val, "printf_num_format"),
                _ => unimplemented!(),
            };
            let format = module.find_global(format_name).unwrap();
            let format_ptr = builder.build_gep(format, &mut [ctx.const_int(0), ctx.const_int(0)]);
            let mut args = vec![format_ptr, to_format];
            let printf = module.find_function("printf").unwrap();
            builder.build_call(&printf, &mut args);
            Ok(val)
        }
        syntax::Expression::Declaration(decl, is_mut, expr) => {
            let initialiser = lower_internal(ctx, module, fun, builder, vars, *expr)?;
            let value = if is_mut {
                // FIXME: look the type up properly
                let typ = ctx.int_type(64);
                let stackloc = builder.build_alloca(typ, &decl.id);
                builder.build_store(initialiser, stackloc);
                stackloc
            } else {
                initialiser
            };
            vars.insert(decl.id, (is_mut, value));
            Ok(initialiser)
        }
        syntax::Expression::IfThenElse(cond, then, elze) => {
            let cond = lower_internal(ctx, module, fun, builder, vars, *cond)?;

            let typ = ctx.int_type(64);
            let ret = builder.build_alloca(typ, "if");

            let thenblock = ctx.add_block(fun, "thenblock");
            let elzeblock = ctx.add_block(fun, "elseblock");
            let joinblock = ctx.add_block(fun, "joinblock");

            builder.build_cond_br(cond, thenblock, elzeblock);

            builder.position_at_end(thenblock);
            let then = lower_internal(ctx, module, fun, builder, vars, *then)?;
            builder.build_store(then, ret);
            builder.build_br(joinblock);

            builder.position_at_end(elzeblock);
            let elze = lower_internal(ctx, module, fun, builder, vars, *elze)?;
            builder.build_store(elze, ret);
            builder.build_br(joinblock);

            builder.position_at_end(joinblock);
            Ok(builder.build_load(ret))
        }
        syntax::Expression::Loop(cond, body) => {
            let condblock = ctx.add_block(fun, "condblock");
            let bodyblock = ctx.add_block(fun, "whilebody");
            let joinblock = ctx.add_block(fun, "joinblock");

            builder.build_br(condblock);
            builder.position_at_end(condblock);

            let cond = lower_internal(ctx, module, fun, builder, vars, *cond)?;
            builder.build_cond_br(cond, bodyblock, joinblock);

            builder.position_at_end(bodyblock);
            lower_internal(ctx, module, fun, builder, vars, *body)?;
            builder.build_br(condblock);

            builder.position_at_end(joinblock);

            Ok(cond)
        }
        syntax::Expression::Sequence(exprs) => {
            exprs
                .into_iter()
                .map(|expr| lower_internal(ctx, module, fun, builder, vars, expr))
                .last()
                .unwrap()
        }
        syntax::Expression::Function(name, _typ, params, body) => {

            let mut fun = module.find_function(&name).expect(&format!(
                "missing function declaration '{}'",
                name
            ));
            let bb = ctx.add_block(&mut fun, "body");
            let mut builder = ctx.add_builder();
            builder.position_at_end(bb);

            let mut vars = params
                .into_iter()
                .enumerate()
                .map(|(i, p)| (p.id, (false, fun.get_param(i as u32))))
                .collect::<HashMap<String, Local>>();

            let body = lower_internal(ctx, module, &mut fun, &mut builder, &mut vars, *body)?;
            builder.build_ret(body);
            fun.verify_or_panic();
            Ok(unsafe { fun.as_raw() })
        }
        syntax::Expression::Call(callee, args) => {
            if let syntax::Expression::Identifier(name) = *callee {
                match module.find_function(&name) {
                    Some(function) => {
                        let mut args = args.into_iter()
                            .map(|arg| lower_internal(ctx, module, fun, builder, vars, arg))
                            .collect::<::std::result::Result<Vec<_>, Error>>()?;
                        let call_res = builder.build_call(&function, &mut args);
                        Ok(call_res)
                    }
                    None => Err(Error::from(format!("Can't find function '{}", name))),
                }
            } else {
                unimplemented!();
            }
        }
        syntax::Expression::Index(_lhs, _index) => unimplemented!(),
    }
}
