//! Syntax Types
//!
//! This module contians the structures used in the syntax tree to
//! reference types.

use super::Token;

/// Type Reference
///
/// Represents a reference to a type. This could be a simple reference
/// to a named type or a complex type such as an array or Tuple.
#[derive(Debug, PartialEq)]
pub enum TypeRef {
    /// Simple Named Type
    Simple(Box<Token>),
    /// The Unit Type
    Unit,
    /// A non-empty Tuple
    Tuple(Vec<TypeRef>),
    /// An Array Type
    Array(Box<TypeRef>),
}

/// Type Annotation
///
/// Reference to a type annotated to a variable or function. This is a
/// type reference and the accompanying `:` token.
#[derive(Debug, PartialEq)]
pub struct TypeAnno {
    /// The `:` Token
    pub anno_tok: Box<Token>,
    /// The type reference
    pub type_ref: TypeRef,
}

impl TypeRef {
    /// Create a New Simple Type
    ///
    /// A simple type is a direct reference to a non-generic non-array
    /// type, such as `Num` or `String`. We keep track of the token
    /// and the inner identifier separately for convenience.
    pub fn simple(tok: Token) -> Self {
        TypeRef::Simple(Box::new(tok))
    }

    /// Create a new Unit Type Reference
    ///
    /// The unit type is represented as a struct with no contents. It
    /// has special meaning in some areas as it can be used to idicate
    /// the absence of a value.
    pub fn unit() -> Self {
        TypeRef::Unit
    }

    /// Create a Tuple Type
    ///
    /// A tuple type is an ordered collection of values. Each value
    /// can be of a different type.
    pub fn tuple(inner: Vec<TypeRef>) -> Self {
        if inner.is_empty() {
            Self::unit()
        } else {
            TypeRef::Tuple(inner)
        }
    }

    /// Create an Array Type
    ///
    /// An array type represents a contiguous collection of another
    /// type.
    pub fn array(inner: TypeRef) -> Self {
        TypeRef::Array(Box::new(inner))
    }
}

impl TypeAnno {
    /// Create a Type Annotation
    ///
    /// Constructs a new `TypeAnno` structure with the given
    /// annotation separation token and inner type reference.
    pub fn new(anno_tok: Token, inner_ty: TypeRef) -> Self {
        TypeAnno {
            anno_tok: Box::new(anno_tok),
            type_ref: inner_ty,
        }
    }
}
