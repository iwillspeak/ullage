//! Jit Evaluation Module
//!
//! This module uses LLVM to implement the `Evaluator` trait.

extern crate llvm_sys;

use std::ffi::CStr;
use std::mem;

use syntax::Expression;
use super::{Evaluator, Value};

use self::llvm_sys::prelude::*;
use self::llvm_sys::{core, target, execution_engine};

/// Jit Evaluator
///
/// LLVM Jit Implemntation of the `Evaulator` Tratit.
pub struct JitEvaluator {
    context: *mut llvm_sys::LLVMContext
}

impl Evaluator for JitEvaluator {

    

    fn eval(&mut self, expr: Expression) -> Value {

        let module = self.compile(expr);

        unsafe { core::LLVMDumpModule(module) }

        let result = self.eval_module(module);

        // TODO: Remove the module from the execution engine so it
        // doesn't get disposed when the engine is disposed...
        // unsafe { core::LLVMDisposeModule(module) }

        result
    }
}

impl JitEvaluator {

    /// Create a new Jit
    ///
    /// Initialises a new LLVM Jit evaluator with the default
    /// settings.
    pub fn new() -> JitEvaluator {
        // initialise some things
        unsafe {
            execution_engine::LLVMLinkInMCJIT();
            if target::LLVM_InitializeNativeTarget() != 0 {
                panic!("Could not initialise target");
            }
            if target::LLVM_InitializeNativeAsmPrinter() != 0 {
                panic!("Could not initialise ASM Printer");
            }
        }
        JitEvaluator {
            context: unsafe { core::LLVMContextCreate() }
        }
    }

    /// Evaluate Module
    ///
    /// Given an LLVM module run it and return a representation of the
    /// result as a `Value`
    fn eval_module(&mut self, module: LLVMModuleRef) -> Value {

        let int64 = unsafe { core::LLVMInt64Type() };

        // Create an execution engine to run the function
        let mut engine = unsafe { mem::uninitialized() };
        let mut out = unsafe { mem::zeroed() };
        unsafe {
            execution_engine::LLVMCreateExecutionEngineForModule(&mut engine, module, &mut out)
        };

        // set up the arguments for the function
        let mut args = unsafe {
            [execution_engine::LLVMCreateGenericValueOfInt(int64, 1700, 0),
             execution_engine::LLVMCreateGenericValueOfInt(int64, 1700, 0)] };

        // Call the function
        let function_name = CStr::from_bytes_with_nul(b"test\0").unwrap();
        let result = unsafe {
            let function = core::LLVMGetNamedFunction(module, function_name.as_ptr());
            execution_engine::LLVMRunFunction(
                engine, function, args.len() as u32, args.as_mut_ptr())
        };

        // dispose of the engine now we are done with it
        unsafe {
            execution_engine::LLVMDisposeExecutionEngine(engine);
        }
        
        Value::Num(unsafe { execution_engine::LLVMGenericValueToInt(result, 1)  as i64})
    }

    /// Compile an Expression
    ///
    /// Takes a given expression and compiles it into an LLVM Module
    fn compile(&mut self, _expr: Expression) -> LLVMModuleRef {

        // Create a module to compile the expression into
        let mod_name = CStr::from_bytes_with_nul(b"temp\0").unwrap();
        let module = unsafe { core::LLVMModuleCreateWithName(mod_name.as_ptr()) };

        // Create a function type and add a function of that type to the module
        let int64 = unsafe { core::LLVMInt64Type() };
        let mut param_types = [int64, int64];

        let function_type = unsafe {
            core::LLVMFunctionType(
                int64,
                param_types.as_mut_ptr(),
                param_types.len() as u32, 0) };

        let function_name = CStr::from_bytes_with_nul(b"test\0").unwrap();
        let function = unsafe {
            core::LLVMAddFunction(module, function_name.as_ptr(), function_type) };

        // Create a basic block and add it to the function
        let block_name = CStr::from_bytes_with_nul(b"entry\0").unwrap();
        let entry_block = unsafe {
            core::LLVMAppendBasicBlock(function, block_name.as_ptr()) };

        // Fill in the body of the function. This is just placeholder for now
        unsafe {
            let builder = core::LLVMCreateBuilder();
            core::LLVMPositionBuilderAtEnd(builder, entry_block);

            let fozzle = core::LLVMConstInt(int64, 1337, 1);
            core::LLVMBuildRet(builder, fozzle);

            core::LLVMDisposeBuilder(builder);
        }

        module
    }
}

impl Drop for JitEvaluator {
    fn drop(&mut self) {
        unsafe { core::LLVMContextDispose(self.context) }
    }
}
