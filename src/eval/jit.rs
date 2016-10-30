//! Jit Evaluation Module
//!
//! This module uses LLVM to implement the `Evaluator` trait.

extern crate llvm_sys;

use std::ffi::{CStr, CString};
use std::collections::HashMap;
use std::ptr;
use std::mem;

use syntax::*;
use syntax::operators::*;
use super::{Evaluator, Value};

use self::llvm_sys::prelude::*;
use self::llvm_sys::{analysis, core, target, execution_engine};

/// Jit Evaluator
///
/// LLVM Jit Implemntation of the `Evaulator` Tratit.
pub struct JitEvaluator {
    sym_tab: HashMap<String, LLVMValueRef>,
    context: *mut llvm_sys::LLVMContext,
    builder: *mut llvm_sys::LLVMBuilder,
    current_function: Option<LLVMValueRef>,
}

impl Evaluator for JitEvaluator {
    fn eval(&mut self, expr: Expression) -> Value {
        let module = self.compile(expr);
        self.eval_module(module)
    }
}

impl Drop for JitEvaluator {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
            core::LLVMContextDispose(self.context);
        }
    }
}

impl visit::Visitor for JitEvaluator {
    type Output = LLVMValueRef;

    fn on_identifier(&mut self, id: String) -> Self::Output {
        let var = self.sym_tab.get(&id).expect("Reference to ID not set!").clone();
        let loaded = CStr::from_bytes_with_nul(b"loaded\0").unwrap();
        unsafe { core::LLVMBuildLoad(self.builder, var, loaded.as_ptr()) }
    }

    fn on_literal(&mut self, lit: Constant) -> Self::Output {
        match lit {
            Constant::Number(i) => unsafe {
                let int64 = core::LLVMInt64TypeInContext(self.context);
                core::LLVMConstInt(int64, i as u64, 1)
            },
            _ => unimplemented!(),
        }
    }

    fn on_prefix(&mut self, op: PrefixOp, expr: Expression) -> Self::Output {
        let value = expr.visit(self);
        match op {
            PrefixOp::Negate => unsafe {
                let temp_name = CStr::from_bytes_with_nul(b"neg\0").unwrap();
                core::LLVMBuildNeg(self.builder, value, temp_name.as_ptr())
            },
            _ => unimplemented!(),
        }
    }

    fn on_infix(&mut self, lhs: Expression, op: InfixOp, rhs: Expression) -> Self::Output {
        use syntax::operators::InfixOp::*;

        match op {
            // arithmetic operators
            Add => {
                let rhs_val = rhs.visit(self);
                let lhs_val = lhs.visit(self);
                let name = CStr::from_bytes_with_nul(b"add\0").unwrap();
                unsafe { core::LLVMBuildAdd(self.builder, lhs_val, rhs_val, name.as_ptr()) }
            }
            Sub => {
                let rhs_val = rhs.visit(self);
                let lhs_val = lhs.visit(self);
                let name = CStr::from_bytes_with_nul(b"sub\0").unwrap();
                unsafe { core::LLVMBuildSub(self.builder, lhs_val, rhs_val, name.as_ptr()) }
            }
            Mul => {
                let rhs_val = rhs.visit(self);
                let lhs_val = lhs.visit(self);
                let name = CStr::from_bytes_with_nul(b"mul\0").unwrap();
                unsafe { core::LLVMBuildMul(self.builder, lhs_val, rhs_val, name.as_ptr()) }
            }
            Div => {
                let rhs_val = rhs.visit(self);
                let lhs_val = lhs.visit(self);
                let name = CStr::from_bytes_with_nul(b"div\0").unwrap();
                unsafe { core::LLVMBuildSDiv(self.builder, lhs_val, rhs_val, name.as_ptr()) }
            }

            Assign => {
                let rhs_value = rhs.visit(self);
                if let Expression::Identifier(id) = lhs {
                    match self.sym_tab.get(&id) {
                        Some(value) => unsafe {
                            core::LLVMBuildStore(self.builder, rhs_value, *value);
                        },
                        None => panic!("Assig to variable that wasn't defined!")
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
        let cond_value = cond.visit(self);

        // Create some basic blocks to hold the conditionally executed bits
        let true_block = self.add_basic_block("on_true");
        let false_block = self.add_basic_block("on_false");
        let merge_block = self.add_basic_block("merge");

        let int64 = unsafe { core::LLVMInt64TypeInContext(self.context) };

        // A temp variable on the stack to hold the 'result' of the
        // if. This will in almost all cases be trivially optimised by
        // LLVM into a phi-node.
        let result_name = CStr::from_bytes_with_nul(b"res\0").unwrap();
        let result = unsafe { core::LLVMBuildAlloca(self.builder, int64, result_name.as_ptr()) };

        let cond_name = CStr::from_bytes_with_nul(b"cond\0").unwrap();
        unsafe {
            // Compare the condition value to false (so we end up with
            // a bool) and then branch based on it's value.
            let false_int = core::LLVMConstInt(int64, 0 as u64, 1);
            let cond_value = core::LLVMBuildICmp(self.builder,
                                                 llvm_sys::LLVMIntPredicate::LLVMIntNE,
                                                 cond_value,
                                                 false_int,
                                                 cond_name.as_ptr());
            core::LLVMBuildCondBr(self.builder, cond_value, true_block, false_block);

            // Move on to building the true block
            core::LLVMPositionBuilderAtEnd(self.builder, true_block);
            let then_value = then.visit(self);
            core::LLVMBuildStore(self.builder, then_value, result);
            core::LLVMBuildBr(self.builder, merge_block);

            // Move on to the else boock
            core::LLVMPositionBuilderAtEnd(self.builder, false_block);
            let els_value = els.visit(self);
            core::LLVMBuildStore(self.builder, els_value, result);
            core::LLVMBuildBr(self.builder, merge_block);

            // Then join the result together from both branches
            core::LLVMPositionBuilderAtEnd(self.builder, merge_block);
            core::LLVMBuildLoad(self.builder, result, result_name.as_ptr())
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
            let int64 = core::LLVMInt64TypeInContext(self.context);
            core::LLVMBuildBr(self.builder, cond_block);

            core::LLVMPositionBuilderAtEnd(self.builder, cond_block);
            let cond_value = cond.visit(self);

            let false_int = core::LLVMConstInt(int64, 0 as u64, 1);
            let cond_value = core::LLVMBuildICmp(self.builder,
                                                 llvm_sys::LLVMIntPredicate::LLVMIntNE,
                                                 cond_value,
                                                 false_int,
                                                 cond_name.as_ptr());
            core::LLVMBuildCondBr(self.builder, cond_value, body_block, merge_block);


            // Move on to building the body
            core::LLVMPositionBuilderAtEnd(self.builder, body_block);
            body.visit(self);

            // back to the top of the loop
            core::LLVMBuildBr(self.builder, cond_block);

            // After the loop
            core::LLVMPositionBuilderAtEnd(self.builder, merge_block);
            false_int
        }
    }

    fn on_variable(&mut self, var: TypedId) -> Self::Output {
        let int64 = unsafe { core::LLVMInt64TypeInContext(self.context) };
        let var_name = CString::new(var.id.clone()).unwrap();
        let var_slot = unsafe { core::LLVMBuildAlloca(self.builder, int64, var_name.as_ptr()) };
        self.sym_tab.insert(var.id, var_slot.clone());
        var_slot
    }

    fn on_sequence(&mut self, mut exprs: Vec<Expression>) -> Self::Output {
        exprs.drain(..).map(|exp| exp.visit(self)).last().unwrap_or_else(|| {
            unsafe {
                let int64 = core::LLVMInt64TypeInContext(self.context);
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
        Self::ensure_initialised();
        let context = unsafe { core::LLVMContextCreate() };
        let builder = unsafe { core::LLVMCreateBuilderInContext(context) };
        JitEvaluator {
            sym_tab: HashMap::new(),
            context: context,
            builder: builder,
            current_function: None,
        }
    }

    fn ensure_initialised() {
        use std::sync::{Once, ONCE_INIT};

        static INIT: Once = ONCE_INIT;

        INIT.call_once(|| {
            // Initialise LLVM
            unsafe {
                execution_engine::LLVMLinkInMCJIT();
                if target::LLVM_InitializeNativeTarget() != 0 {
                    panic!("Could not initialise target");
                }
                if target::LLVM_InitializeNativeAsmPrinter() != 0 {
                    panic!("Could not initialise ASM Printer");
                }
            }
        });
    }

    /// Evaluate Module
    ///
    /// Given an LLVM module run it and return a representation of the
    /// result as a `Value`
    fn eval_module(&mut self, module: LLVMModuleRef) -> Value {

        // Create an execution engine to run the function
        let mut engine = unsafe { mem::uninitialized() };
        let mut out = unsafe { mem::zeroed() };
        unsafe {
            execution_engine::LLVMCreateExecutionEngineForModule(&mut engine, module, &mut out)
        };

        // Call the function
        let function_name = CStr::from_bytes_with_nul(b" \0").unwrap();
        let result = unsafe {
            let mut function = mem::zeroed();

            if execution_engine::LLVMFindFunction(engine,
                                                  function_name.as_ptr(),
                                                  &mut function as *mut LLVMValueRef) !=
               0 {
                panic!("Could not find function")
            }
            execution_engine::LLVMRunFunction(engine, function, 0, ptr::null_mut())
        };

        // dispose of the engine now we are done with it
        unsafe {
            execution_engine::LLVMDisposeExecutionEngine(engine);
        }

        Value::Num(unsafe { execution_engine::LLVMGenericValueToInt(result, 1) as i64 })
    }

    /// Compile an Expression
    ///
    /// Takes a given expression and compiles it into an LLVM Module
    fn compile(&mut self, expr: Expression) -> LLVMModuleRef {

        // Create a module to compile the expression into
        let mod_name = CStr::from_bytes_with_nul(b"temp\0").unwrap();
        let module =
            unsafe { core::LLVMModuleCreateWithNameInContext(mod_name.as_ptr(), self.context) };

        // Create a function to be used to evaluate our expression
        let function_type = unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.context);
            core::LLVMFunctionType(int64, ptr::null_mut(), 0, 0)
        };

        let function_name = CStr::from_bytes_with_nul(b" \0").unwrap();
        let function =  unsafe { core::LLVMAddFunction(module, function_name.as_ptr(), function_type) };
        self.current_function = Some(function);

        let main_block = self.add_basic_block("entry");
        unsafe {
            core::LLVMPositionBuilderAtEnd(self.builder, main_block);
        }

        // compile down the expression
        let expression = expr.visit(self);

        let function_verified = unsafe {
            core::LLVMBuildRet(self.builder, expression);
            analysis::LLVMVerifyFunction(function, analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction)
        };

        if function_verified != 0 {
            let mut message = ptr::null_mut();
            unsafe {
                if analysis::LLVMVerifyModule(module, analysis::LLVMVerifierFailureAction::LLVMReturnStatusAction, &mut message) != 0 {
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
        unsafe { core::LLVMAppendBasicBlockInContext(self.context, function, block_name.as_ptr()) }
    }
}

#[cfg(test)]
mod test {

    use syntax::*;
    use syntax::operators::*;

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
