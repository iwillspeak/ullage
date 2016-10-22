//! Jit Evaluation Module
//!
//! This module uses LLVM to implement the `Evaluator` trait.

extern crate llvm_sys;

use std::ffi::CStr;
use std::ptr;

use syntax::Expression;
use super::{Evaluator, Value};

use self::llvm_sys::{core, target};

/// Jit Evaluator
///
/// LLVM Jit Implemntation of the `Evaulator` Tratit.
pub struct JitEvaluator {
    context: *mut llvm_sys::LLVMContext
}

impl Evaluator for JitEvaluator {
    fn eval(&mut self, _: Expression) -> Value {

        // Create a module to compile the expression into
        let mod_name = CStr::from_bytes_with_nul(b"temp\0").unwrap();
        let module = unsafe { core::LLVMModuleCreateWithName(mod_name.as_ptr()) };

        unsafe { core::LLVMDisposeModule(module) }

        Value::Num(1337)
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
            target::LLVM_InitializeNativeAsmPrinter();
            target::LLVM_InitializeNativeTarget();
        };
        JitEvaluator {
            context: unsafe { core::LLVMContextCreate() }
        }
    }
}

impl Drop for JitEvaluator {
    fn drop(&mut self) {
        unsafe { core::LLVMContextDispose(self.context) }
    }
}
