//! LLVM IR Builder Wrapper
//!
//! Contains a Rust wrapper for dealing with LLVM Intermediate
//! Representation Builders. These objects are responsible for
//! creating instructions and addind them to basic blocks. Essentially
//! they make up the ponity end of the whole thing. Useful!

use super::llvm_sys::prelude::*;
use super::llvm_sys::core;

use std::ffi::CStr;
use std::os::raw::c_uint;

use super::function::Function;

/// IR Builder
///
/// Creating yo instructions and manipulating yo basic blocks.
#[derive(Debug,PartialEq)]
pub struct Builder {
    raw: LLVMBuilderRef,
}

/// Build Context
///
/// A build context represents an IR builder attached to a block.
pub struct BuildContext<'a> {
    builder: &'a mut Builder,
}

impl Builder {
    /// Create a Builder from a Raw Pointer
    ///
    /// Takes ownership of the given builder pointer. The builder can
    /// then be manipulated through the returned object and will be
    /// disposed of when this object leaves scope.
    pub fn from_raw(raw: LLVMBuilderRef) -> Self {
        Builder { raw: raw }
    }

    /// Build at the End of a Block
    ///
    /// Takes the builder, points it at the end of the basic block and
    /// returns a build context that can be used to insert
    /// instructions.
    pub fn build_at_end<'a>(&'a mut self, block: LLVMBasicBlockRef) -> BuildContext<'a> {
        unsafe {
            core::LLVMPositionBuilderAtEnd(self.raw, block);
        }
        BuildContext { builder: self }
    }
}

impl<'a> BuildContext<'a> {
    /// Add a Ret Instrution
    ///
    /// Returns control from the current function
    /// immediately. Consumes this build context as t the current
    /// basic block can't have any more instructions added after a
    /// terminator instruciton.
    pub fn build_ret(self, value: LLVMValueRef) {
        unsafe {
            core::LLVMBuildRet(self.builder.raw, value);
        }
    }

    /// Build a Call Instruction
    ///
    /// Emits a call to the given function.
    pub fn build_call(&mut self, function: &Function, args: &mut [LLVMValueRef]) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"printed\0");
            core::LLVMBuildCall(self.builder.raw,
                                function.as_raw(),
                                args.as_mut_ptr(),
                                args.len() as c_uint,
                                name.as_ptr())
        }
    }

    /// Build a GEP
    ///
    /// GEP, or GetElementPointer, retrieves a pointer to an element in an item.
    pub fn build_gep(&mut self, value: LLVMValueRef, indices: &mut [LLVMValueRef]) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"gep\0");
            core::LLVMBuildGEP(self.builder.raw,
                               value,
                               indices.as_mut_ptr(),
                               indices.len() as c_uint,
                               name.as_ptr())
        }
    }
}

impl Drop for Builder {
    /// Disponse this Builder
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.raw);
        }
    }
}
