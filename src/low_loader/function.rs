//! LLVM Function Wrapper
//!
//! A wrapper around an LLVM function.

use super::llvm_sys::analysis;
use super::llvm_sys::core;
use super::llvm_sys::prelude::*;

/// Function
///
/// A single function in a given module.
#[derive(Debug, PartialEq)]
pub struct Function {
    raw: LLVMValueRef,
}

impl Function {
    /// Wrap an Existing Funciton
    ///
    /// Takes ownership of the given function and provides more
    /// stronlgy typed access to it.
    pub unsafe fn from_raw(raw: LLVMValueRef) -> Self {
        Function { raw: raw }
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

    /// Get a Function's Parameter
    ///
    /// Returns a value that can be used to access the `n`th function
    /// argument.
    pub fn get_param(&self, n: u32) -> LLVMValueRef {
        unsafe { core::LLVMGetParam(self.as_raw(), n) }
    }

    /// Raw Borrow
    ///
    /// # Safety
    ///
    /// This method returns a raw pointer to the underlying
    /// LLVMValue. It's up to you to make sure it doesn't outlive the
    /// `Function`, and to make sure you don't break any of LLVMs
    /// thread safety requirements.
    pub unsafe fn as_raw(&self) -> LLVMValueRef {
        self.raw
    }
}
