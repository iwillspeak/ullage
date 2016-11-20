//! Low Loader
//!
//! Low-level bindings to LLVM for building JIT compilers.

extern crate llvm_sys;

use self::llvm_sys::prelude::*;
use self::llvm_sys::{core, target, execution_engine};

/// Wrapped Value Reference
#[derive(Debug,PartialEq)]
pub struct Value(LLVMValueRef);

impl From<Value> for LLVMValueRef {
    /// From Value
    ///
    /// Convert a wrapped value into a raw LLVM value reference.
    fn from(v: Value) -> Self {
        let Value(inner) = v;
        inner
    }
}

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
            // Initialise the machine code JIT compiler. This is what
            // the `Context` uses to evaluate expressions.
            execution_engine::LLVMLinkInMCJIT();

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

/// Compilation Context
///
/// Represents all of the global state which is held between
/// individual function compilations and evaluations. This object is
/// responsible for managing the underlying LLVM contex, builder and
/// evaluator.
pub struct Context {
    /// Raw LLVM Context
    ///
    /// The LLVM context holds the global state for compilation. This
    /// includes types and modules. LLVM context objects aren't
    /// guaranteed to be thread safe, and shouldn't be shared between
    /// threasds. We'll enforce this by taking `&mut self` when
    /// meddling with the context.
    context: *mut llvm_sys::LLVMContext,

    /// Builder
    ///
    /// The LLVM IR builder object is responsible for writing
    /// instructions into basic blocks. We just want one of these for
    /// now.
    builder: *mut llvm_sys::LLVMBuilder,
}

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
        let ctx = unsafe { core::LLVMContextCreate() };
        Context {
            context: ctx,
            builder: unsafe { core::LLVMCreateBuilderInContext(ctx) },
        }
    }

    /// Constant Int
    ///
    /// Creates a constant integer value in the given context. This
    /// value can then be used during compilation as part of a
    /// constant expression.
    ///
    /// # Arguments
    ///
    ///  * `val` - The 64 bit integer value to convert into an LLVM
    ///  value.
    pub fn const_int(&self, i: i64) -> Value {
        Value(unsafe {
            let int64 = core::LLVMInt64TypeInContext(self.context);
            core::LLVMConstInt(int64, i as u64, 1)
        })
    }

    /// TODO: remove this!!
    pub fn as_context_ptr(&mut self) -> *mut llvm_sys::LLVMContext {
        self.context
    }

    /// TODO: remove this too!
    pub fn as_builder_ptr(&mut self) -> *mut llvm_sys::LLVMBuilder {
        self.builder
    }
}

/// Drop impl for Context
///
/// The context object holds raw LLVM resources and therefore must
/// dispose of them.
impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            core::LLVMDisposeBuilder(self.builder);
            core::LLVMContextDispose(self.context);
        }
    }
}

#[cfg(test)]
mod test {

    use std::ptr;

    use super::*;

    #[test]
    fn create_jit_context_succeeds() {
        let _ = Context::new();
    }

    #[test]
    fn context_as_raw_builder_ptr_is_not_null() {
        let mut ctx = Context::new();
        let raw = ctx.as_builder_ptr();
        assert_ne!(ptr::null_mut(), raw);
    }

    #[test]
    fn context_as_raw_context_ptr_is_not_null() {
        let mut ctx = Context::new();
        let raw = ctx.as_context_ptr();
        assert_ne!(ptr::null_mut(), raw);
    }

    #[test]
    fn context_create_constant_int() {
        let ctx = Context::new();
        let _ = ctx.const_int(1234);
    }
}
