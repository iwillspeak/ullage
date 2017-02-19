//! LLVM IR Builder Wrapper
//!
//! Contains a Rust wrapper for dealing with LLVM Intermediate
//! Representation Builders. These objects are responsible for
//! creating instructions and addind them to basic blocks. Essentially
//! they make up the ponity end of the whole thing. Useful!

use super::llvm_sys::prelude::*;
use super::llvm_sys::core;

/// IR Builder
///
/// Creating yo instructions and manipulating yo basic blocks.
#[derive(Debug,PartialEq)]
pub struct Builder {
    raw: LLVMBuilderRef,
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
}

impl Drop for Builder {
    /// Disponse this Builder
    fn drop(&mut self) {
        unsafe { core::LLVMDisposeBuilder(self.raw); }
    }
}
