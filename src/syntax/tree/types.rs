//! Syntax Types
//!
//! This module contians the structures used in the syntax tree to
//! reference types.

use super::Token;
use crate::syntax::text::{Location, Pos};

/// Type Reference
///
/// Represents a reference to a type. This could be a simple reference
/// to a named type or a complex type such as an array or Tuple.
#[derive(Debug, PartialEq)]
pub enum TypeRef {
    /// Simple Named Type
    Simple(Box<Token>),
    /// The Unit Type
    Unit(Box<Token>, Box<Token>),
    /// A non-empty Tuple
    Tuple(Box<Token>, Vec<TypeRef>, Box<Token>),
    /// An Array Type
    Array(Box<Token>, Box<TypeRef>, Box<Token>),
    /// Missing type. Used to represent type information being missing
    /// at a given location.
    Missing,
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
    pub fn unit(open: Token, close: Token) -> Self {
        TypeRef::Unit(Box::new(open), Box::new(close))
    }

    /// Create a Tuple Type
    ///
    /// A tuple type is an ordered collection of values. Each value
    /// can be of a different type.
    pub fn tuple(open: Token, inner: Vec<TypeRef>, close: Token) -> Self {
        if inner.is_empty() {
            Self::unit(open, close)
        } else {
            TypeRef::Tuple(Box::new(open), inner, Box::new(close))
        }
    }

    /// Create an Array Type
    ///
    /// An array type represents a contiguous collection of another
    /// type.
    pub fn array(open: Token, inner: TypeRef, close: Token) -> Self {
        TypeRef::Array(Box::new(open), Box::new(inner), Box::new(close))
    }

    /// Create a missing type
    pub fn missing() -> Self {
        TypeRef::Missing
    }
}

impl Location for TypeRef {
    fn start(&self) -> Pos {
        match self {
            TypeRef::Simple(tok) => tok.span().start(),
            TypeRef::Unit(open, ..) => open.span().start(),
            TypeRef::Tuple(open, ..) => open.span().start(),
            TypeRef::Array(open, ..) => open.span().start(),
            TypeRef::Missing => Pos::from(0),
        }
    }

    fn end(&self) -> Pos {
        match self {
            TypeRef::Simple(tok) => tok.span().end(),
            TypeRef::Unit(_, close) => close.span().end(),
            TypeRef::Tuple(_, _, close) => close.span().end(),
            TypeRef::Array(_, _, close) => close.span().end(),
            TypeRef::Missing => Pos::from(0),
        }
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
