//! LLVM Context Wrapper
//!
//! Contains a Rust wrapper for dealing with LLVM Context objects.

use std::ffi::{CStr, CString};
use std::os::raw::c_uint;
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
    pub fn add_function(&mut self,
                        module: &mut Module,
                        name: &str,
                        ret_type: LLVMTypeRef,
                        params: &mut [LLVMTypeRef])
                        -> Function {
        // Create a function to be used to evaluate our expression
        let function_type = unsafe {
            let param_count = params.len();
            let params = params.as_mut_ptr();
            core::LLVMFunctionType(ret_type, params, param_count as c_uint, 0)
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

    /// Add a Printf Declaration to the Module
    ///
    /// Creates a new function in the given module which maps to the
    /// `printf` function. This will be used by the `print` operator
    /// to write output.
    pub fn add_printf_decl(&mut self, module: &mut Module) {
        unsafe {
            let int32 = core::LLVMInt32TypeInContext(self.as_raw());
            let mut args = vec![core::LLVMPointerType(core::LLVMInt8TypeInContext(self.as_raw()),
                                                      0)];
            let printf_type = core::LLVMFunctionType(int32, args.as_mut_ptr(), 1, 1);
            let function_name = CStr::from_bytes_with_nul_unchecked(b"printf\0");
            core::LLVMAddFunction(module.as_raw(), function_name.as_ptr(), printf_type);
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

    /// Create a Constant Character Value
    pub fn const_char(&self, i: u8) -> LLVMValueRef {
        unsafe {
            let int8 = core::LLVMInt8TypeInContext(self.as_raw());
            core::LLVMConstInt(int8, i as u64, 0)
        }
    }

    /// Create a Constant Bool
    ///
    /// The returned value is a constant 1-bit integer with the given
    /// boolean value mapped to `true` => `1`, `false` => `0`.
    pub fn const_bool(&self, b: bool) -> LLVMValueRef {
        let mapped = if b { 1 } else { 0 };
        unsafe {
            let int1 = core::LLVMInt1TypeInContext(self.as_raw());
            core::LLVMConstInt(int1, mapped, 0)
        }
    }

    /// Create a Constant String Value
    ///
    /// The returned value is a constant i8 array with characters from
    /// the given string stored as UTF8.
    pub fn const_str(&self, s: &str) -> LLVMValueRef {
        let mut bytes: Vec<_> = s.bytes()
            .map(|b| self.const_char(b))
            .collect();
        bytes.push(self.const_char(0));
        unsafe {
            let int8 = core::LLVMInt8TypeInContext(self.as_raw());
            core::LLVMConstArray(int8, bytes.as_mut_ptr(), bytes.len() as c_uint)
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

    /// Get Type in this Context
    pub fn int_type(&self, width: usize) -> LLVMTypeRef {
        unsafe { core::LLVMIntTypeInContext(self.as_raw(), width as c_uint) }
    }

    pub fn bool_type(&self) -> LLVMTypeRef {
        self.int_type(1)
    }

    pub fn cstr_type(&self) -> LLVMTypeRef {
        unsafe {
            let int8 = core::LLVMInt8TypeInContext(self.as_raw());
            core::LLVMPointerType(int8, 0)
        }
    }

    /// Get the type kind from a given type.
    pub fn get_type(&self, value: LLVMValueRef) -> LLVMTypeRef {
        unsafe { core::LLVMTypeOf(value) }
    }

    pub fn named_type(&self, type_name: &str) -> LLVMTypeRef {
        match type_name {
            "Bool" => self.bool_type(),
            "Number" => self.int_type(64),
            _ => unimplemented!(),
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            core::LLVMContextDispose(self.as_raw());
        }
    }
}
