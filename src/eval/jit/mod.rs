//! Jit Evaluation Module
//!
//! This module uses LLVM to implement the `Evaluator` trait.

extern crate llvm_sys;

pub mod low_loader;

mod value;

use std::ffi::{CStr, CString};
use std::collections::HashMap;
use std::ptr;

use syntax::*;
use syntax::visit::*;
use syntax::operators::*;
use super::{Evaluator, Value};

use self::low_loader::{Context, Module};

use self::llvm_sys::prelude::*;
use self::llvm_sys::{analysis, core};

/// Jit Evaluator
///
/// LLVM Jit Implemntation of the `Evaulator` Tratit.
///
/// This strut holds global state which should live for the lifetime
/// of the entire program.
pub struct JitEvaluator {
    sym_tab: HashMap<String, LLVMValueRef>,
    ctx: Context,
    current_function: Option<LLVMValueRef>,
}

impl Evaluator for JitEvaluator {
    fn eval(&mut self, expr: Expression) -> Value {
        let module = self.compile(expr);
        self.eval_module(module).unwrap()
    }
}

impl visit::Visitor for JitEvaluator {
    type Output = LLVMValueRef;

    fn on_identifier(&mut self, id: String) -> Self::Output {
        let var = self.sym_tab.get(&id).expect("Reference to ID not set!").clone();
        let loaded = CStr::from_bytes_with_nul(b"loaded\0").unwrap();
        unsafe { core::LLVMBuildLoad(self.ctx.as_builder_ptr(), var, loaded.as_ptr()) }
    }

    fn on_literal(&mut self, lit: Constant) -> Self::Output {
        match lit {
            Constant::Number(i) => self.ctx.const_int(i).into(),
            _ => unimplemented!(),
        }
    }

    fn on_prefix(&mut self, op: PrefixOp, expr: Expression) -> Self::Output {
        let value = self.visit(expr);
        match op {
            PrefixOp::Negate => unsafe {
                let temp_name = CStr::from_bytes_with_nul(b"neg\0").unwrap();
                core::LLVMBuildNeg(self.ctx.as_builder_ptr(), value, temp_name.as_ptr())
            },
            _ => unimplemented!(),
        }
    }

    fn on_infix(&mut self, lhs: Expression, op: InfixOp, rhs: Expression) -> Self::Output {
        use syntax::operators::InfixOp::*;

        match op {
            // arithmetic operators
            Add => {
                let rhs_val = self.visit(rhs);
                let lhs_val = self.visit(lhs);
                let name = CStr::from_bytes_with_nul(b"add\0").unwrap();
                unsafe {
                    core::LLVMBuildAdd(self.ctx.as_builder_ptr(), lhs_val, rhs_val, name.as_ptr())
                }
            }
            Sub => {
                let rhs_val = self.visit(rhs);
                let lhs_val = self.visit(lhs);
                let name = CStr::from_bytes_with_nul(b"sub\0").unwrap();
                unsafe {
                    core::LLVMBuildSub(self.ctx.as_builder_ptr(), lhs_val, rhs_val, name.as_ptr())
                }
            }
            Mul => {
                let rhs_val = self.visit(rhs);
                let lhs_val = self.visit(lhs);
                let name = CStr::from_bytes_with_nul(b"mul\0").unwrap();
                unsafe {
                    core::LLVMBuildMul(self.ctx.as_builder_ptr(), lhs_val, rhs_val, name.as_ptr())
                }
            }
            Div => {
                let rhs_val = self.visit(rhs);
                let lhs_val = self.visit(lhs);
                let name = CStr::from_bytes_with_nul(b"div\0").unwrap();
                unsafe {
                    core::LLVMBuildSDiv(self.ctx.as_builder_ptr(), lhs_val, rhs_val, name.as_ptr())
                }
            }

            Assign => {
                let rhs_value = self.visit(rhs);
                if let Expression::Identifier(id) = lhs {
                    match self.sym_tab.get(&id) {
                        Some(value) => unsafe {
                            core::LLVMBuildStore(self.ctx.as_builder_ptr(), rhs_value, *value);
                        },
                        None => panic!("Assig to variable that wasn't defined!"),
                    }
                } else {
                    panic!("Assign to something which isn't an ID");
                }
                rhs_value
            }

            // Comparison operators
            Eq | NotEq | Lt | Gt => unimplemented!(),
        }
    }

    fn on_call(&mut self, _calee: Expression, _args: Vec<Expression>) -> Self::Output {
        unimplemented!();
    }

    fn on_index(&mut self, _target: Expression, _index: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_if(&mut self, cond: Expression, then: Expression, els: Expression) -> Self::Output {
        // First things first, let's compute the value of the condition
        let cond_value = self.visit(cond);

        // Create some basic blocks to hold the conditionally executed bits
        let true_block = self.add_basic_block("on_true");
        let false_block = self.add_basic_block("on_false");
        let merge_block = self.add_basic_block("merge");

        let int64 = unsafe { core::LLVMInt64TypeInContext(self.ctx.as_context_ptr()) };

        // A temp variable on the stack to hold the 'result' of the
        // if. This will in almost all cases be trivially optimised by
        // LLVM into a phi-node.
        let result_name = CStr::from_bytes_with_nul(b"res\0").unwrap();
        let result = unsafe {
            core::LLVMBuildAlloca(self.ctx.as_builder_ptr(), int64, result_name.as_ptr())
        };

        let cond_name = CStr::from_bytes_with_nul(b"cond\0").unwrap();
        unsafe {
            // Compare the condition value to false (so we end up with
            // a bool) and then branch based on it's value.
            let false_int = core::LLVMConstInt(int64, 0 as u64, 1);
            let cond_value = core::LLVMBuildICmp(self.ctx.as_builder_ptr(),
                                                 llvm_sys::LLVMIntPredicate::LLVMIntNE,
                                                 cond_value,
                                                 false_int,
                                                 cond_name.as_ptr());
            core::LLVMBuildCondBr(self.ctx.as_builder_ptr(),
                                  cond_value,
                                  true_block,
                                  false_block);

            // Move on to building the true block
            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), true_block);
            let then_value = self.visit(then);
            core::LLVMBuildStore(self.ctx.as_builder_ptr(), then_value, result);
            core::LLVMBuildBr(self.ctx.as_builder_ptr(), merge_block);

            // Move on to the else boock
            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), false_block);
            let els_value = self.visit(els);
            core::LLVMBuildStore(self.ctx.as_builder_ptr(), els_value, result);
            core::LLVMBuildBr(self.ctx.as_builder_ptr(), merge_block);

            // Then join the result together from both branches
            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), merge_block);
            core::LLVMBuildLoad(self.ctx.as_builder_ptr(), result, result_name.as_ptr())
        }
    }

    fn on_function(&mut self,
                   _id: String,
                   _ty: TypeRef,
                   _args: Vec<TypedId>,
                   _body: Expression)
                   -> Self::Output {
        unimplemented!();
    }

    fn on_loop(&mut self, cond: Expression, body: Expression) -> Self::Output {
        // Create some basic blocks to hold the conditionally executed bits
        let cond_block = self.add_basic_block("condition");
        let body_block = self.add_basic_block("loop_body");
        let merge_block = self.add_basic_block("merge");

        let cond_name = CStr::from_bytes_with_nul(b"cond\0").unwrap();
        unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.ctx.as_context_ptr());
            core::LLVMBuildBr(self.ctx.as_builder_ptr(), cond_block);

            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), cond_block);
            let cond_value = self.visit(cond);

            let false_int = core::LLVMConstInt(int64, 0 as u64, 1);
            let cond_value = core::LLVMBuildICmp(self.ctx.as_builder_ptr(),
                                                 llvm_sys::LLVMIntPredicate::LLVMIntNE,
                                                 cond_value,
                                                 false_int,
                                                 cond_name.as_ptr());
            core::LLVMBuildCondBr(self.ctx.as_builder_ptr(),
                                  cond_value,
                                  body_block,
                                  merge_block);


            // Move on to building the body
            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), body_block);
            self.visit(body);

            // back to the top of the loop
            core::LLVMBuildBr(self.ctx.as_builder_ptr(), cond_block);

            // After the loop
            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), merge_block);
            false_int
        }
    }

    fn on_variable(&mut self, var: TypedId) -> Self::Output {
        let int64 = unsafe { core::LLVMInt64TypeInContext(self.ctx.as_context_ptr()) };
        let var_name = CString::new(var.id.clone()).unwrap();
        let var_slot =
            unsafe { core::LLVMBuildAlloca(self.ctx.as_builder_ptr(), int64, var_name.as_ptr()) };
        self.sym_tab.insert(var.id, var_slot.clone());
        var_slot
    }

    fn on_sequence(&mut self, mut exprs: Vec<Expression>) -> Self::Output {
        exprs.drain(..).map(|exp| self.visit(exp)).last().unwrap_or_else(|| {
            unsafe {
                let int64 = core::LLVMInt64TypeInContext(self.ctx.as_context_ptr());
                core::LLVMConstInt(int64, 0 as u64, 1)
            }
        })
    }
}

impl JitEvaluator {
    /// Create a new Jit
    ///
    /// Initialises a new LLVM Jit evaluator with the default
    /// settings.
    pub fn new() -> JitEvaluator {
        JitEvaluator {
            sym_tab: HashMap::new(),
            ctx: Context::new(),
            current_function: None,
        }
    }

    /// Evaluate Module
    ///
    /// Given an LLVM module run it and return a representation of the
    /// result as a `Value`.
    fn eval_module(&mut self, module: Module) -> Result<Value, String> {
        let engine = module.into_execution_engine().unwrap();
        Ok(Value::Num(try!(engine.run_function(" "))))
    }

    /// Compile an Expression
    ///
    /// Takes a given expression and compiles it into an LLVM Module
    fn compile(&mut self, expr: Expression) -> Module {

        // Create a module to compile the expression into
        let module = self.ctx.add_module("temp");

        // Create a function to be used to evaluate our expression
        let function_type = unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.ctx.as_context_ptr());
            core::LLVMFunctionType(int64, ptr::null_mut(), 0, 0)
        };

        let function_name = CStr::from_bytes_with_nul(b" \0").unwrap();
        let function = unsafe {
            core::LLVMAddFunction(module.as_ptr(), function_name.as_ptr(), function_type)
        };
        self.current_function = Some(function);

        let main_block = self.add_basic_block("entry");
        unsafe {
            core::LLVMPositionBuilderAtEnd(self.ctx.as_builder_ptr(), main_block);
        }

        // compile down the expression
        let expression = self.visit(expr);

        let function_verified = unsafe {
            core::LLVMBuildRet(self.ctx.as_builder_ptr(), expression);
            analysis::LLVMVerifyFunction(
                function, analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction)
        };

        if function_verified != 0 {
            let mut message = ptr::null_mut();
            unsafe {
                if analysis::LLVMVerifyModule(
                    module.as_ptr(),
                    analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction,
                    &mut message) != 0 {
                    let err = CStr::from_ptr(message);
                    println!("ERROR: {}", err.to_string_lossy());
                    core::LLVMDisposeMessage(message);
                }
            }
            panic!("Function did not verify!");
        }

        module
    }

    /// Add a Basic Block
    fn add_basic_block(&mut self, name: &str) -> LLVMBasicBlockRef {
        // Create a basic block and add it to the function
        let block_name = CString::new(name).unwrap();
        let function = self.current_function.expect("not compiling a function!");
        unsafe {
            core::LLVMAppendBasicBlockInContext(self.ctx.as_context_ptr(),
                                                function,
                                                block_name.as_ptr())
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use super::super::*;

    #[test]
    fn create_jit() {
        let _ = JitEvaluator::new();
    }

    #[test]
    fn evaulate_simple_expression() {
        let mut e = JitEvaluator::new();
        {
            let expr = Expression::constant_num(1337);
            assert_eq!(Value::Num(1337), e.eval(expr));
        }
        {
            let expr = Expression::constant_num(2000);
            assert_eq!(Value::Num(2000), e.eval(expr));
        }
    }

    #[test]
    fn evaulate_prefix_op_applies_operation() {
        let mut e = JitEvaluator::new();
        {
            let expr = Expression::prefix(PrefixOp::Negate, Expression::constant_num(9000));
            assert_eq!(Value::Num(-9000), e.eval(expr));
        }
    }

    #[test]
    fn evaluate_infix_op_applies_operation() {
        let mut e = JitEvaluator::new();
        {
            let expr = Expression::infix(Expression::constant_num(1300),
                                         InfixOp::Add,
                                         Expression::constant_num(37));
            assert_eq!(Value::Num(1337), e.eval(expr));
        }
        {
            let expr = Expression::infix(Expression::constant_num(0),
                                         InfixOp::Sub,
                                         Expression::constant_num(999));
            assert_eq!(Value::Num(-999), e.eval(expr));
        }
    }

    #[test]
    fn evaluate_if() {
        let mut e = JitEvaluator::new();
        {
            let expr = Expression::if_then_else(Expression::constant_num(1),
                                                Expression::constant_num(2),
                                                Expression::constant_num(3));
            assert_eq!(Value::Num(2), e.eval(expr));
        }
        {
            let expr =
                Expression::if_then_else(Expression::if_then_else(Expression::constant_num(1),
                                                                  Expression::constant_num(0),
                                                                  Expression::constant_num(1)),
                                         Expression::constant_num(2),
                                         Expression::constant_num(3));
            assert_eq!(Value::Num(3), e.eval(expr));
        }
    }
}
