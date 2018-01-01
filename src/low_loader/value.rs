use super::llvm_sys::prelude::LLVMValueRef;

/// Wrapped Value Reference
#[derive(Debug,PartialEq)]
pub struct Value(LLVMValueRef);

// Allow conversion from our wrapped type to the underlying LLVM
// one. This is intended more as an escape hatch while converting code
// to use the new safe wrappers rather than as a permanent solution.
impl From<Value> for LLVMValueRef {
    /// From Value
    ///
    /// Convert a wrapped value into a raw LLVM value reference.
    fn from(v: Value) -> Self {
        let Value(inner) = v;
        inner
    }
}

impl Value {
    pub fn as_llvm_value(&self) -> LLVMValueRef {
        let &Value(v) = self;
        v
    }
}
