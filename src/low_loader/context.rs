//! LLVM Context Wrapper
//!
//! Contains a Rust wrapper for dealing with LLVM Context objects.

use std::ffi::CString;
use super::llvm_sys::prelude::*;
use super::llvm_sys::{target, core};
use super::prelude::*;

/// Ensure Initialised
///
/// Makes sure that the LLVM library has been initialised to support
/// the features we want to use. This function can safely be called
/// any number of times but will only initialise LLVM once.
///
/// # Panics
///
/// If any of the LLVM subsystems can't be successfully initialised
/// then this function will panic.
fn ensure_initialised() {
    use std::sync::{Once, ONCE_INIT};

    static INIT: Once = ONCE_INIT;

    INIT.call_once(|| {
        unsafe {
            // // Initialise the machine code JIT compiler. This is what
            // // the `Context` uses to evaluate expressions.
            // execution_engine::LLVMLinkInMCJIT();

            // Make sure that the 'native' target is set up and ready
            // to go. The 'native' target is the one on which LLVM is
            // currently running.
            if target::LLVM_InitializeNativeTarget() != 0 {
                panic!("Could not initialise target");
            }
             if target::LLVM_InitializeNativeAsmPrinter() != 0 {
                panic!("Could not initialise ASM Printer");
            }
        }
    });
}

/// Context
///
/// A context groups together all the LLVM objects used when
/// compiling.
///
/// The LLVM context holds the global state for compilation. This
/// includes types and modules. LLVM context objects aren't
/// guaranteed to be thread safe, and shouldn't be shared between
/// threasds. We'll enforce this by taking `&mut self` when
/// meddling with the context.
#[derive(Debug,PartialEq)]
pub struct Context(LLVMContextRef);

impl Context {
    /// Create Context
    ///
    /// You'll probably only need one of these per 'program' you want
    /// to evaluate. Modules, types and execution from one context
    /// can't be used with another context.
    ///
    /// # Returns
    ///
    /// Returns a new compilation context instance.
    ///
    /// # Panics
    ///
    /// If the underlying LLVM library can't be initialised then this
    /// function will panic.
    pub fn new() -> Self {
        ensure_initialised();
        Context(unsafe { core::LLVMContextCreate() })
    }


    /// Add Module
    ///
    /// Creates a new LLVM module in this context.
    pub fn add_module(&mut self, name: &str) -> Module {
        let mod_name = CString::new(name).unwrap();
        Module::from_raw(unsafe {
            core::LLVMModuleCreateWithNameInContext(mod_name.as_ptr(), self.as_raw())
        })
    }

    /// Raw Borrow
    fn as_raw(&mut self) -> LLVMContextRef {
        let &mut Context(raw_ctx) = self;
        raw_ctx
    }

    // TODO: Builder support!
    // pub fn add_builder(&mut self) -> Builder {
    //     Builder::from(unsafe { core::LLVMCreateBuilderInContext(ctx) })
    // }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            core::LLVMContextDispose(self.as_raw());
        }
    }
}
