//! LLVM IR Builder Wrapper
//!
//! Contains a Rust wrapper for dealing with LLVM Intermediate
//! Representation Builders. These objects are responsible for
//! creating instructions and adding them to basic blocks. Essentially
//! they make up the ponity end of the whole thing. Useful!

use super::llvm_sys::core;
use super::llvm_sys::prelude::*;
use super::llvm_sys::*;

use std::ffi::{CStr, CString};
use std::os::raw::c_uint;

use super::function::Function;

/// IR Builder
///
/// Creating yo instructions and manipulating yo basic blocks.
#[derive(Debug, PartialEq)]
pub struct Builder {
    raw: LLVMBuilderRef,
}

/// Comparison Predicate Type
///
/// Choice of comparison operators. These will be mapped through to
/// `LLVMIntPreidcate` or `LLVMRealPredicate`s depending on the types
/// being used.
#[derive(Copy, Clone)]
pub enum Predicate {
    /// Equality predicate
    Eq,
    /// Inequality predicate
    Neq,
    /// Arithmetic less than comparison
    Lt,
    /// Arithmetic less than or equal comparison
    LtEq,
    /// Arithmetic greter than comparision
    Gt,
    /// Arithmetic greater than or equal comparison
    GtEq,
}

impl Builder {
    /// Create a Builder from a Raw Pointer
    ///
    /// Takes ownership of the given builder pointer. The builder can
    /// then be manipulated through the returned object and will be
    /// disposed of when this object leaves scope.
    pub fn from_raw(raw: LLVMBuilderRef) -> Self {
        Builder { raw }
    }

    /// Build at the End of a Block
    ///
    /// Takes the builder, points it at the end of the basic block.
    pub fn position_at_end(&mut self, block: LLVMBasicBlockRef) {
        unsafe {
            core::LLVMPositionBuilderAtEnd(self.raw, block);
        }
    }

    /// Add a Ret Instrution
    ///
    /// Returns control from the current function
    /// immediately. Consumes this build context as t the current
    /// basic block can't have any more instructions added after a
    /// terminator instruciton.
    pub fn build_ret(self, value: LLVMValueRef) {
        unsafe {
            core::LLVMBuildRet(self.raw, value);
        }
    }

    /// Build a Call to a `void` Function
    ///
    /// The built value produces nothing so no value is returned.
    pub fn build_void_call(&mut self, function: &Function, args: &mut [LLVMValueRef]) {
        self.build_named_call(function, args, Some("voidcall"));
    }

    /// Build a Call Instruction
    ///
    /// Emits a call to the given function.
    pub fn build_call(&mut self, function: &Function, args: &mut [LLVMValueRef]) -> LLVMValueRef {
        self.build_named_call(function, args, Some("call"))
    }

    /// Build a Call with a Name
    ///
    /// Emits a call instruction. The name provided to the call
    /// controls the resulting temporary value. If no name is provided
    /// then the result isn't bound to any temporary.
    pub fn build_named_call(
        &mut self,
        function: &Function,
        args: &mut [LLVMValueRef],
        name: Option<&str>,
    ) -> LLVMValueRef {
        let name = name.map(|n| CString::new(n).unwrap());
        unsafe {
            let call = core::LLVMBuildCall(
                self.raw,
                function.as_raw(),
                args.as_mut_ptr(),
                args.len() as c_uint,
                name.as_ref().map_or(std::ptr::null(), |n| n.as_ptr()),
            );
            core::LLVMSetInstructionCallConv(call, function.call_conv().into());
            call
        }
    }

    /// Build a GEP
    ///
    /// GEP, or GetElementPointer, retrieves a pointer to an element in an item.
    pub fn build_gep(&mut self, value: LLVMValueRef, indices: &mut [LLVMValueRef]) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"gep\0");
            core::LLVMBuildGEP(
                self.raw,
                value,
                indices.as_mut_ptr(),
                indices.len() as c_uint,
                name.as_ptr(),
            )
        }
    }

    /// Build a Structure GEP
    ///
    /// Loads an element from a given structure pointer.
    pub fn build_struct_gep(&mut self, struct_val: LLVMValueRef, index: u32) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"sgep\0");
            core::LLVMBuildStructGEP(self.raw, struct_val, index as c_uint, name.as_ptr())
        }
    }

    /// Extract a Value from an Aggregate
    ///
    /// Reads a value from a structure at the given struct offset.
    pub fn build_extract_value(&mut self, struct_val: LLVMValueRef, index: u32) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"extracted\0");
            core::LLVMBuildExtractValue(self.raw, struct_val, index as c_uint, name.as_ptr())
        }
    }

    /// Build an Integer Negation
    pub fn build_neg(&mut self, value: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"negated\0");
            core::LLVMBuildNeg(self.raw, value, name.as_ptr())
        }
    }

    /// Build an Integer Add
    pub fn build_add(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"addtmp\0");
            core::LLVMBuildAdd(self.raw, lhs, rhs, name.as_ptr())
        }
    }

    /// Build an Integer Subtraction
    pub fn build_sub(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"subtmp\0");
            core::LLVMBuildSub(self.raw, lhs, rhs, name.as_ptr())
        }
    }

    /// Build an Integer Multiplication
    pub fn build_mul(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"multmp\0");
            core::LLVMBuildMul(self.raw, lhs, rhs, name.as_ptr())
        }
    }

    /// Build a Signed Integer Division
    pub fn build_sdiv(&mut self, lhs: LLVMValueRef, rhs: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"divtmp\0");
            core::LLVMBuildSDiv(self.raw, lhs, rhs, name.as_ptr())
        }
    }

    /// Build an Integer Comparision
    pub fn build_icmp(
        &mut self,
        op: Predicate,
        lhs: LLVMValueRef,
        rhs: LLVMValueRef,
    ) -> LLVMValueRef {
        let op = match op {
            Predicate::Eq => LLVMIntPredicate::LLVMIntEQ,
            Predicate::Neq => LLVMIntPredicate::LLVMIntNE,
            Predicate::Lt => LLVMIntPredicate::LLVMIntSLT,
            Predicate::Gt => LLVMIntPredicate::LLVMIntSGT,
            Predicate::LtEq => LLVMIntPredicate::LLVMIntSLE,
            Predicate::GtEq => LLVMIntPredicate::LLVMIntSGE,
        };
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"cmptemp\0");
            core::LLVMBuildICmp(self.raw, op, lhs, rhs, name.as_ptr())
        }
    }

    /// Build an Allocate Instruction
    ///
    /// Creates a new value allocated for the remainder of the current
    /// stack frame.
    pub fn build_alloca(&mut self, typ: LLVMTypeRef, name: &str) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        unsafe { core::LLVMBuildAlloca(self.raw, typ, name.as_ptr()) }
    }

    /// Build a `malloc` Instruction
    ///
    /// Creates a new value allocated on the heap. Retusna pointer to
    /// the new value.
    pub fn build_malloc(
        &mut self,
        typ: LLVMTypeRef,
        count: Option<LLVMValueRef>,
        name: &str,
    ) -> LLVMValueRef {
        let name = CString::new(name).unwrap();
        match count {
            Some(count) => unsafe {
                core::LLVMBuildArrayMalloc(self.raw, typ, count, name.as_ptr())
            },
            None => unsafe { core::LLVMBuildMalloc(self.raw, typ, name.as_ptr()) },
        }
    }

    /// Create a Conditional Branch
    ///
    /// If the condition is true then execution continues in the first
    /// block, otherwise execution will move to the second block.
    pub fn build_cond_br(
        &mut self,
        cond: LLVMValueRef,
        iftrue: LLVMBasicBlockRef,
        iffalse: LLVMBasicBlockRef,
    ) {
        unsafe {
            core::LLVMBuildCondBr(self.raw, cond, iftrue, iffalse);
        }
    }

    /// Create an Unconditional Branch
    pub fn build_br(&mut self, block: LLVMBasicBlockRef) {
        unsafe {
            core::LLVMBuildBr(self.raw, block);
        }
    }

    /// Load from Variable
    pub fn build_load(&mut self, var: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"loaded\0");
            core::LLVMBuildLoad(self.raw, var, name.as_ptr())
        }
    }

    /// Store to Variable
    pub fn build_store(&mut self, val: LLVMValueRef, var: LLVMValueRef) -> LLVMValueRef {
        unsafe { core::LLVMBuildStore(self.raw, val, var) }
    }

    /// Built a Not
    pub fn build_not(&mut self, val: LLVMValueRef) -> LLVMValueRef {
        unsafe {
            let name = CStr::from_bytes_with_nul_unchecked(b"not\0");
            core::LLVMBuildNot(self.raw, val, name.as_ptr())
        }
    }

    /// Bitcast
    ///
    /// Re-interpret the input value to be of the given type. This
    /// just transforms how the underlying bits are interpreted rather
    /// than performing any smarter coercion.
    pub fn build_bitcast(
        &mut self,
        val: LLVMValueRef,
        typ: LLVMTypeRef,
        name: &str,
    ) -> LLVMValueRef {
        unsafe {
            let name = CString::new(name).unwrap();
            core::LLVMBuildBitCast(self.raw, val, typ, name.as_ptr())
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
