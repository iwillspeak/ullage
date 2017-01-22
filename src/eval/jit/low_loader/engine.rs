//! LLVM Execution Engine Wrappers
//!
//! Contains the types and functions for abstracting over LLVM Execution Engines

use std::{mem, ptr};
use std::ffi::CString;

use super::llvm_sys::execution_engine::*;
use super::llvm_sys::prelude::*;

/// Execution Engine Reference
///
/// Holds a raw LLVMExecutionEgnineRef, and provides safe access to
/// some of it's functionality.
pub struct ExecutionEngine(LLVMExecutionEngineRef);

impl ExecutionEngine {
    /// From Raw
    ///
    /// Create a new `ExecutionEngine` by wrapping the given raw
    /// reference.
    ///
    /// # Arguments
    ///
    ///  * `engine` - The raw LLVMExecutionEngineRef to wrap.
    ///
    /// # Safety
    ///
    /// This method takes ownership of the given Execution Engine. The
    /// execution engine will be destroyed when the returned object's
    /// lifetime ends.
    pub fn from_raw(engine: LLVMExecutionEngineRef) -> Self {
        ExecutionEngine(engine)
    }

    /// As Pointer
    ///
    /// Retireves the internal pointer reference to the unsafe
    /// execution engine.
    pub fn as_ptr(&self) -> LLVMExecutionEngineRef {
        let &ExecutionEngine(engine) = self;
        engine
    }

    /// Run Function
    ///
    /// Looks up a given funciton in the execution engine and calls
    /// it. The funciton shouldn't take any arguments and should
    /// return a 64 bit integer.
    pub fn run_function(&self, name: &str) -> Result<i64, String> {
        let name = CString::new(name).unwrap();
        unsafe {
            let mut function = mem::zeroed();

            if LLVMFindFunction(self.as_ptr(),
                                name.as_ptr(),
                                &mut function as *mut LLVMValueRef) != 0 {
                return Err(String::from("Could not find function"));
            }
            let ret = LLVMRunFunction(self.as_ptr(), function, 0, ptr::null_mut());
            Ok(LLVMGenericValueToInt(ret, 1) as i64)
        }
    }
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        let engine = self.as_ptr();
        unsafe {
            LLVMDisposeExecutionEngine(engine);
        }
    }
}
