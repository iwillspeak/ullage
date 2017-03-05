//! Expression Lowering
//!
//! This module is responsible for taking Expressions and lowering
//! them to LLVM.

use syntax::{Expression, Constant};
use syntax::operators::{PrefixOp, InfixOp};
use low_loader::prelude::*;

use super::error::*;

use std::collections::HashMap;

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
/// # Retunrs
///
/// A `Result` indicating if the expression was lowered successfully.
pub fn lower_expressions<'a>(ctx: &mut Context,
                             module: &mut Module,
                             fun: &mut Function,
                             builder: &mut BuildContext<'a>,
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
                          builder: &mut BuildContext<'a>,
                          vars: &mut HashMap<String, LLVMValueRef>,
                          expr: Expression)
                          -> Result<LLVMValueRef> {
    match expr {
        Expression::Identifier(id) => {
            // TODO: Make this simpler by wrapping LLVMValueRef in our own type that's copy.
            match vars.get(&id) {
                Some(val) => Ok(val.clone()),
                None => Err(Error::from(format!("Reference to undefined '{}'", id))),
            }
        }
        Expression::Literal(Constant::Number(n)) => Ok(ctx.const_int(n)),
        Expression::Prefix(op, expr) => {
            let val = try!(lower_internal(ctx, module, fun, builder, vars, *expr));
            let val = match op {
                PrefixOp::Negate => builder.build_neg(val),
                PrefixOp::Not => unimplemented!(),
            };
            Ok(val)
        }
        Expression::Infix(lhs, op, rhs) => {
            let lhs_val = try!(lower_internal(ctx, module, fun, builder, vars, *lhs));
            let rhs_val = try!(lower_internal(ctx, module, fun, builder, vars, *rhs));
            let val = match op {
                InfixOp::Add => builder.build_add(lhs_val, rhs_val),
                InfixOp::Sub => builder.build_sub(lhs_val, rhs_val),
                InfixOp::Mul => builder.build_mul(lhs_val, rhs_val),
                InfixOp::Div => builder.build_sdiv(lhs_val, rhs_val),
                _ => unimplemented!(),
            };
            Ok(val)
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
        Expression::Declaration(decl, expr) => {
            let initialiser = try!(lower_internal(ctx, module, fun, builder, vars, *expr));
            vars.insert(decl.id, initialiser);
            Ok(initialiser)
        }
        _ => unimplemented!(),
    }
}
