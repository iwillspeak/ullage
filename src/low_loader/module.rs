//! LLVM Module Wrapper
//!
//! Contains types and wrappers for dealing with LLVM Modules.

use super::llvm_sys::prelude::*;
use super::llvm_sys::core;

/// Module
///
/// A module repsents a single code unit. It maps down to a library or
/// executable when compiled by LLVM. This type provides a safe
/// abstraction around the raw `LLVMModule` type.
#[derive(Debug,PartialEq)]
pub struct Module {
    raw: LLVMModuleRef
}

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
        Module { raw: mod_ref }
    }

    /// Dump the Module
    ///
    /// Writes a representation of the module to standard output. This
    /// is intended to be used as an aid to debugging.
    pub fn dump(&self) {
        unsafe {
            core::LLVMDumpModule(self.raw)
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeModule(self.raw)
        }
    }
}

impl From<Module> for LLVMModuleRef {
    /// Convert from Module
    ///
    /// Consume the wrapped module and return it's interal module
    /// reference. This transfers the ownership of the module back to
    /// the caller preventing the it from being automaticaly freed.
    fn from(m: Module) -> LLVMModuleRef {
        let mod_ref = m.raw;
        ::std::mem::forget(m);
        mod_ref
    }
}
