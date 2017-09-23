//! LLVM Types

use super::llvm_sys::{core, LLVMTypeKind};
use super::llvm_sys::prelude::*;

#[allow(non_camel_case_types)]
pub enum FloatWidth {
    Half,
    Float,
    Double,
    X86_FP80,
    FP128,
    PPC_FP128,
}

pub enum Type {
    Void, // LLVMVoidTypeKind
    Float(FloatWidth),
    Label,
    Int(usize),
    Function,
    Struct,
    Array,
    Pointer,
    Vector,
    Metadata,
    MMX,
    Token,
}

impl From<LLVMTypeRef> for Type {
    fn from(llvm_type: LLVMTypeRef) -> Self {
        use self::LLVMTypeKind::*;
        match unsafe { core::LLVMGetTypeKind(llvm_type) } {
            LLVMVoidTypeKind => Type::Void,
            LLVMHalfTypeKind => Type::Float(FloatWidth::Half),
            LLVMFloatTypeKind => Type::Float(FloatWidth::Half),
            LLVMDoubleTypeKind => Type::Float(FloatWidth::Double),
            LLVMX86_FP80TypeKind => Type::Float(FloatWidth::X86_FP80),
            LLVMFP128TypeKind => Type::Float(FloatWidth::FP128),
            LLVMPPC_FP128TypeKind => Type::Float(FloatWidth::PPC_FP128),
            LLVMLabelTypeKind => Type::Label,
            LLVMIntegerTypeKind => {
                Type::Int(unsafe { core::LLVMGetIntTypeWidth(llvm_type) } as usize)
            }
            LLVMFunctionTypeKind => Type::Function,
            LLVMStructTypeKind => Type::Struct,
            LLVMArrayTypeKind => Type::Array,
            LLVMPointerTypeKind => Type::Pointer,
            LLVMVectorTypeKind => Type::Vector,
            LLVMMetadataTypeKind => Type::Metadata,
            LLVMX86_MMXTypeKind => Type::MMX,
            LLVMTokenTypeKind => Type::Token,
        }
    }
}
