//! LLVM Module Wrapper
//!
//! Contains types and wrappers for dealing with LLVM Modules.

use std::{mem, ptr};
use std::ffi::CString;

use super::llvm_sys::prelude::*;
use super::llvm_sys::execution_engine;

use super::engine::ExecutionEngine;

/// Module
///
/// A module repsents a single code unit. It maps down to a library or
/// executable when compiled by LLVM. This type provides a safe
/// abstraction around the raw `LLVMModule` type.
#[derive(Debug,PartialEq)]
pub struct Module(LLVMModuleRef);

impl Module {
    /// Module from Raw
    ///
    /// Creates a new module from a raw module reference. This takes
    /// ownership of the given module. When the returned Module
    /// instance goes out of scope the module will be disposed.
    ///
    /// *Note*: You shouldn't need to use this directly, instead modules
    /// can be created with `Context::add_module`.
    pub fn from_raw(mod_ref: LLVMModuleRef) -> Self {
        Module(mod_ref)
    }

    /// Get the Raw Module Handle
    ///
    /// Returns the internal module handle.
    ///
    /// TODO: Get rid of the need for this.
    pub fn as_ptr(&self) -> LLVMModuleRef {
        let &Module(mod_ref) = self;
        mod_ref
    }

    /// Convert into Execution Engine
    ///
    /// Takes the given module, and compiles it into a JIT execution
    /// engine.
    pub fn into_execution_engine(self) -> Result<ExecutionEngine, String> {
        let mut engine = unsafe { mem::uninitialized() };
        let mut out = ptr::null_mut();
        let ok = unsafe {
            // consume the module to prevent it from being dropped later on
            execution_engine::LLVMCreateExecutionEngineForModule(&mut engine,
                                                                 self.into(),
                                                                 &mut out) == 0
        };
        if ok {
            Ok(ExecutionEngine::from_raw(engine))
        } else {
            Err(unsafe { CString::from_raw(out) }.into_string().unwrap())
        }
    }
}

// Allow converting modules to the underlying LLVM type for now, this
// aids conversion of the JIT to use these wrapper types.
impl From<Module> for LLVMModuleRef {
    /// Convert from Module
    ///
    /// Consume the wrapped module and return it's interal module
    /// reference. This transfers the ownership of the module back to
    /// the caller preventing the it from being automaticaly freed.
    fn from(m: Module) -> LLVMModuleRef {
        let Module(mod_ref) = m;
        ::std::mem::forget(m);
        mod_ref
    }
}
