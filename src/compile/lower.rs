//! Expression Lowering
//!
//! This module is responsible for taking Expressions and lowering
//! them to LLVM.

use syntax::{Expression, Constant};
use syntax::ast::operators::{PrefixOp, InfixOp};
use low_loader::prelude::*;

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
pub fn lower_expressions<'a>(ctx: &mut Context,
                             module: &mut Module,
                             fun: &mut Function,
                             builder: &mut Builder,
                             expressions: Vec<Expression>)
                             -> Result<()> {
    let mut vars = HashMap::new();
    for expr in expressions {
        try!(lower_internal(ctx, module, fun, builder, &mut vars, expr));
    }
    Ok(())
}

/// Internal Lowering of `Expression`s
///
/// Converts an `Expression` to LLVM IR
pub fn lower_internal<'a>(ctx: &mut Context,
                          module: &mut Module,
                          fun: &mut Function,
                          builder: &mut Builder,
                          vars: &mut HashMap<String, Local>,
                          expr: Expression)
                          -> Result<LLVMValueRef> {
    match expr {
        Expression::Identifier(id) => {
            match vars.get(&id) {
                Some(&(is_mut, val)) => {
                    Ok(if is_mut { builder.build_load(val) } else { val })
                }
                None => Err(Error::from(format!("Reference to undefined '{}'", id))),
            }
        }
        Expression::Literal(lit) => {
            match lit {
                Constant::Number(n) => Ok(ctx.const_int(n)),
                Constant::Bool(b) => Ok(ctx.const_bool(b)),
                Constant::String(_) => unimplemented!(),
            }
        }
        Expression::Prefix(op, expr) => {
            let val = try!(lower_internal(ctx, module, fun, builder, vars, *expr));
            let val = match op {
                PrefixOp::Negate => builder.build_neg(val),
                PrefixOp::Not => builder.build_not(val),
            };
            Ok(val)
        }
        Expression::Infix(lhs, op, rhs) => {
            let rhs_val = try!(lower_internal(ctx, module, fun, builder, vars, *rhs));

            // TODO: maybe assignment should be a different node in the AST?
            if op == InfixOp::Assign {
                if let Expression::Identifier(id) = *lhs {
                    match vars.get(&id) {
                        Some(&(true, var)) => {
                            builder.build_store(rhs_val, var);
                            Ok(rhs_val)
                        }
                        _ => Err(Error::from(format!("can't assign to {}", id))),
                    }
                } else {
                    Err(Error::Generic(String::from("left hand side of an assignment must be an \
                                                     identifier")))
                }
            } else {
                let lhs_val = try!(lower_internal(ctx, module, fun, builder, vars, *lhs));
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
        Expression::Print(inner) => {
            let val = try!(lower_internal(ctx, module, fun, builder, vars, *inner));
            let fun = module.find_function("printf").unwrap();
            let format = module.find_global("printf_num_format").unwrap();
            let format_ptr = builder.build_gep(format, &mut [ctx.const_int(0), ctx.const_int(0)]);
            let mut args = vec![format_ptr, val];
            builder.build_call(&fun, &mut args);
            Ok(val)
        }
        Expression::Declaration(decl, is_mut, expr) => {
            let initialiser = try!(lower_internal(ctx, module, fun, builder, vars, *expr));
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
        Expression::IfThenElse(cond, then, elze) => {
            let cond = try!(lower_internal(ctx, module, fun, builder, vars, *cond));

            let typ = ctx.int_type(64);
            let ret = builder.build_alloca(typ, "if");

            let thenblock = ctx.add_block(fun, "thenblock");
            let elzeblock = ctx.add_block(fun, "elseblock");
            let joinblock = ctx.add_block(fun, "joinblock");

            builder.build_cond_br(cond, thenblock, elzeblock);

            builder.position_at_end(thenblock);
            let then = try!(lower_internal(ctx, module, fun, builder, vars, *then));
            builder.build_store(then, ret);
            builder.build_br(joinblock);

            builder.position_at_end(elzeblock);
            let elze = try!(lower_internal(ctx, module, fun, builder, vars, *elze));
            builder.build_store(elze, ret);
            builder.build_br(joinblock);

            builder.position_at_end(joinblock);
            Ok(builder.build_load(ret))
        }
        Expression::Loop(cond, body) => {
            let condblock = ctx.add_block(fun, "condblock");
            let bodyblock = ctx.add_block(fun, "whilebody");
            let joinblock = ctx.add_block(fun, "joinblock");

            builder.build_br(condblock);
            builder.position_at_end(condblock);

            let cond = try!(lower_internal(ctx, module, fun, builder, vars, *cond));
            builder.build_cond_br(cond, bodyblock, joinblock);

            builder.position_at_end(bodyblock);
            try!(lower_internal(ctx, module, fun, builder, vars, *body));
            builder.build_br(condblock);

            builder.position_at_end(joinblock);

            Ok(cond)
        }
        Expression::Sequence(exprs) => {
            exprs.into_iter()
                .map(|expr| lower_internal(ctx, module, fun, builder, vars, expr))
                .last()
                .unwrap()
        }
        Expression::Function(name, _typ, params, body) => {

            let mut sig = params.iter()
                .map(|_| ctx.int_type(64))
                .collect::<Vec<_>>();

            let mut fun = ctx.add_function(module, &name, &mut sig);
            let bb = ctx.add_block(&mut fun, "body");
            let mut builder = ctx.add_builder();
            builder.position_at_end(bb);

            let mut vars = params.into_iter()
                .enumerate()
                .map(|(i, p)| (p.id, (false, fun.get_param(i as u32))))
                .collect::<HashMap<String, Local>>();

            let body = try!(lower_internal(ctx, module, &mut fun, &mut builder, &mut vars, *body));
            builder.build_ret(body);
            fun.verify_or_panic();
            Ok(unsafe { fun.as_raw() })
        }
        Expression::Call(callee, args) => {
            if let Expression::Identifier(name) = *callee {
                match module.find_function(&name) {
                    Some(function) => {
                        let mut args = try!(args.into_iter()
                            .map(|arg| lower_internal(ctx, module, fun, builder, vars, arg))
                            .collect::<::std::result::Result<Vec<_>, Error>>());
                        let call_res = builder.build_call(&function, &mut args);
                        Ok(call_res)
                    }
                    None => Err(Error::from(format!("Can't find function '{}", name))),
                }
            } else {
                unimplemented!();
            }
        }
        Expression::Index(_lhs, _index) => unimplemented!(),
    }
}
