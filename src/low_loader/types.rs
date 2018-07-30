//! LLVM Types

use super::llvm_sys::prelude::*;
use super::llvm_sys::{core, LLVMTypeKind};

/// Floating Point Sizes
///
/// LLVM floating point types can only be one of a given set of
/// sizes. Some of these sizes are architecture-specific.
#[allow(non_camel_case_types)]
pub enum FloatWidth {
    /// Hafl-width float (16 bits)
    Half,
    /// Standard float (32 bits)
    Float,
    /// Double-precision float (64 bits)
    Double,
    /// X86 / 8087 'extended precision' float (80 bits)
    X86_FP80,
    /// 128 bit float (112 bit mantissa)
    FP128,
    /// PowerPC Specific 128 bit float
    PPC_FP128,
}

/// Type Enumeration
///
/// Represents the different types possible for a given value.
pub enum Type {
    /// The void type represents the absence of a value and has no
    /// size. Void can't be used as the target type of a pointer.
    Void,

    /// Floating point number of a given width.
    Float(FloatWidth),

    /// A code label.
    Label,

    /// An arbitrary-sized integer type.
    Int(usize),

    /// A callable funciton.
    Function,

    /// A structure type.
    ///
    /// Represents a collection of values together in memory.
    Struct,

    /// An n-dimensional array type.
    Array,

    /// A memory location.
    Pointer,

    /// A 'vector' type
    ///
    /// Used to represent a collection of primitives which can be
    /// processed in parallel.
    Vector,

    /// A metadata value
    Metadata,

    /// An X86 MMX Value
    MMX,

    /// A value which can't be inspected
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
