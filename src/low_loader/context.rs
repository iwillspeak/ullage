//! LLVM Context Wrapper
//!
//! Contains a Rust wrapper for dealing with LLVM Context objects.

use std::ptr;
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

    /// Add a Function to the Module
    ///
    /// Creates a new function in the module. The funciton has a fixed
    /// return type of int64 at the moment and takes no
    /// parameters. This is just for testing though and more
    /// configuration will have to be added when we come to compile
    /// functions of our own rather than just functions to represent a
    /// group of top-level expressions.
    pub fn add_function(&mut self, module: &mut Module, name: &str) -> Function {
        // Create a function to be used to evaluate our expression
        let function_type = unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.as_raw());
            core::LLVMFunctionType(int64, ptr::null_mut(), 0, 0)
        };

        let function_name = CString::new(name).unwrap();

        // Function::from_raw is `unsafe` because it doesn't verify that the value you
        // give it is an LLVM Function. I think we can be sure this one is though :-p
        unsafe {
            Function::from_raw(core::LLVMAddFunction(module.as_raw(),
                                                     function_name.as_ptr(),
                                                     function_type))
        }
    }

    /// Add a Basic Block to a given Function
    ///
    /// Creates a basic block and add it to the function.
    pub fn add_block(&mut self, fun: &mut Function, name: &str) -> LLVMBasicBlockRef {
        let block_name = CString::new(name).unwrap();
        unsafe {
            core::LLVMAppendBasicBlockInContext(self.as_raw(), fun.as_raw(), block_name.as_ptr())
        }
    }

    /// Create an IR Builder
    ///
    /// Creates and initalises a new IR Builder in this `Context`.
    pub fn add_builder(&mut self) -> Builder {
        Builder::from_raw(unsafe { core::LLVMCreateBuilderInContext(self.as_raw()) })
    }

    /// Create A Constant Value
    ///
    /// The returned value is a constant 64 bit integer with the given
    /// value.
    pub fn const_int(&self, i: i64) -> LLVMValueRef {
        unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.as_raw());
            core::LLVMConstInt(int64, i as u64, 1)
        }
    }

    /// Raw Borrow
    ///
    /// # Safety
    ///
    /// This method returns a raw pointer to the underlying
    /// LLVMContext. It's up to you to make sure it doesn't outlive
    /// the `Context`, and to make sure you don't break any of LLVMs
    /// thread safety requirements.
    pub unsafe fn as_raw(&self) -> LLVMContextRef {
        let &Context(raw_ctx) = self;
        raw_ctx
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            core::LLVMContextDispose(self.as_raw());
        }
    }
}
