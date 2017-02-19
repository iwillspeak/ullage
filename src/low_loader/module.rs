//! LLVM Module Wrapper
//!
//! Contains types and wrappers for dealing with LLVM Modules.

use std::ptr;
use std::ffi::CString;
use super::llvm_sys::prelude::*;
use super::llvm_sys::core;
use super::prelude::*;

/// Module
///
/// A module repsents a single code unit. It maps down to a library or
/// executable when compiled by LLVM. This type provides a safe
/// abstraction around the raw `LLVMModule` type.
#[derive(Debug,PartialEq)]
pub struct Module<'a> {
    raw: LLVMModuleRef,
    ctx: &'a Context
}

impl<'a> Module<'a> {
    /// Module from Raw
    ///
    /// Creates a new module from a raw module reference. This takes
    /// ownership of the given module. When the returned Module
    /// instance goes out of scope the module will be disposed.
    ///
    /// *Note*: You shouldn't need to use this directly, instead modules
    /// can be created with `Context::add_module`.
    pub fn from_raw_parts(mod_ref: LLVMModuleRef, ctx: &'a Context) -> Self {
        Module {
            raw: mod_ref,
            ctx: ctx
        }
    }

    /// Add a Function to the Module
    ///
    /// Creates a new function in the module.
    pub fn add_function(&mut self, name: &str) -> Function {
        // Create a function to be used to evaluate our expression
        let function_type = unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.ctx.as_raw());
            core::LLVMFunctionType(int64, ptr::null_mut(), 0, 0)
        };

        let function_name = CString::new(name).unwrap();
        unsafe {
            Function::from_raw(
                core::LLVMAddFunction(self.raw, function_name.as_ptr(), function_type),
                self.ctx
            )   
        }
    }

    /// Dump the Module
    ///
    /// Writes a representation of the module to standard output. This
    /// is intended to be used as an aid to debugging.
    pub fn dump(&self) {
        unsafe { core::LLVMDumpModule(self.raw) }
    }
}

impl<'a> Drop for Module<'a> {
    fn drop(&mut self) {
        unsafe { core::LLVMDisposeModule(self.raw) }
    }
}

impl<'a> From<Module<'a>> for LLVMModuleRef {
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
