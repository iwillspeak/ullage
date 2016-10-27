//! Jit Evaluation Module
//!
//! This module uses LLVM to implement the `Evaluator` trait.

extern crate llvm_sys;

use std::ffi::CStr;
use std::ptr;
use std::mem;

use syntax::*;
use syntax::operators::*;
use super::{Evaluator, Value};

use self::llvm_sys::prelude::*;
use self::llvm_sys::{core, target, execution_engine};

/// Jit Evaluator
///
/// LLVM Jit Implemntation of the `Evaulator` Tratit.
pub struct JitEvaluator {
    context: *mut llvm_sys::LLVMContext,
    builder: *mut llvm_sys::LLVMBuilder,
}

impl Evaluator for JitEvaluator {
    fn eval(&mut self, expr: Expression) -> Value {
        let module = self.compile(expr);
        self.eval_module(module)
    }
}

impl Drop for JitEvaluator {
    fn drop(&mut self) {
        println!("GOING OUT OF SCOPE");
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
            core::LLVMContextDispose(self.context);
        }
    }
}

impl visit::Visitor for JitEvaluator {
    type Output = LLVMValueRef;

    fn on_identifier(&mut self, _id: String) -> Self::Output {
        unimplemented!();
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

    fn on_infix(&mut self, _lhs: Expression, _op: InfixOp, _rhs: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_call(&mut self, _calee: Expression, _args: Vec<Expression>) -> Self::Output {
        unimplemented!();
    }

    fn on_index(&mut self, _target: Expression, _index: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_if(&mut self, _cond: Expression, _then: Expression, _els: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_function(&mut self,
                   _id: String,
                   _ty: TypeRef,
                   _args: Vec<TypedId>,
                   _body: Expression)
                   -> Self::Output {
        unimplemented!();
    }

    fn on_loop(&mut self, _cond: Expression, _body: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_variable(&mut self, _var: TypedId) -> Self::Output {
        unimplemented!();
    }

    fn on_sequence(&mut self, mut _exprs: Vec<Expression>) -> Self::Output {
        unimplemented!();
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
            context: context,
            builder: builder,
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
        let function =
            unsafe { core::LLVMAddFunction(module, function_name.as_ptr(), function_type) };

        // Create a basic block and add it to the function
        let block_name = CStr::from_bytes_with_nul(b"entry\0").unwrap();
        let entry_block = unsafe {
            core::LLVMAppendBasicBlockInContext(self.context, function, block_name.as_ptr())
        };

        unsafe {
            core::LLVMPositionBuilderAtEnd(self.builder, entry_block);
        }

        // compile down the expression
        let expression = expr.visit(self);

        unsafe {
            core::LLVMBuildRet(self.builder, expression);
        }

        module
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
}
