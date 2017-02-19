//! LLVM Function Wrapper
//!
//! A wrapper around an LLVM function.

use std::ffi::CString;
use super::llvm_sys::prelude::*;
use super::llvm_sys::{analysis, core};
use super::prelude::*;

/// Function
///
/// A single function in a given module.
#[derive(Debug,PartialEq)]
pub struct Function<'a> {
    raw: LLVMValueRef,
    ctx: &'a Context,
}

impl<'a> Function<'a> {
    /// Wrap an Existing Funciton
    ///
    /// Takes ownership of the given function and provides more
    /// stronlgy typed access to it.
    pub unsafe fn from_raw(raw: LLVMValueRef, ctx: &'a Context) -> Self {
        Function {
            raw: raw,
            ctx: ctx
        }
    }

    /// Add a Basic Block to this Function
    ///
    /// Creates a basic block and add it to the function.
    pub fn add_block(&mut self, name: &str) -> LLVMBasicBlockRef {
        let block_name = CString::new(name).unwrap();
        unsafe {
            core::LLVMAppendBasicBlockInContext(self.ctx.as_raw(),
                                                self.raw,
                                                block_name.as_ptr())
        }        
    }
    
    /// Verify the Function
    ///
    /// Makes LLVM check the funciton is valid. If the function is not
    /// valid we will panic to signal the error. This is intended for
    /// debugging the compiler's output.
    pub fn verify_or_panic(&self) {
        use self::analysis::*;
        let verified = unsafe {
            LLVMVerifyFunction(self.raw, LLVMVerifierFailureAction::LLVMPrintMessageAction)
        };

        if verified == 1 {
            panic!("Function failed verification!")
        }
    }
    
}
