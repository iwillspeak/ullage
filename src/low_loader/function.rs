//! LLVM Function Wrapper
//!
//! A wrapper around an LLVM function.

use super::llvm_sys::analysis;
use super::llvm_sys::core;
use super::llvm_sys::prelude::*;
use super::llvm_sys::LLVMCallConv;

/// Function
///
/// A single function in a given module.
#[derive(Debug, PartialEq)]
pub struct Function {
    raw: LLVMValueRef,
}

/// Calling Contentions
///
/// This is a subset of the LLVM calling contentions.
pub enum CallConvention {
    /// THe `fastcall` calling contention
    Fastcall,
    /// The C Calling Contention
    CDecl,
}

impl From<CallConvention> for libc::c_uint {
    fn from(call_convention: CallConvention) -> Self {
        let llvm_conv = match call_convention {
            CallConvention::Fastcall => LLVMCallConv::LLVMFastCallConv,
            CallConvention::CDecl => LLVMCallConv::LLVMCCallConv,
        };
        llvm_conv as libc::c_uint
    }
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
            unsafe {
                core::LLVMDumpValue(self.raw);
            }
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

    /// Set the Function's Calling Convention
    ///
    /// Updates the calling convention for the function
    /// delcaration. We use fastcall for our calling convention and
    /// cdecl for c interop.
    pub fn set_calling_convention(&mut self, call_convention: CallConvention) {
        unsafe {
            core::LLVMSetFunctionCallConv(self.raw, call_convention.into());
        }
    }
}
