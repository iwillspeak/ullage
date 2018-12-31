//! Syntax Types
//!
//! This module contians the structures used in the syntax tree to
//! reference types.

/// Type Reference
///
/// Represents a reference to a type. This could be a simple reference
/// to a named type or a complex type such as an array or Tuple.
#[derive(Debug, PartialEq)]
pub enum TypeRef {
    /// Simple Named Type
    Simple(String),
    /// The Unit Type
    Unit,
    /// A non-empty Tuple
    Tuple(Vec<TypeRef>),
    /// An Array Type
    Array(Box<TypeRef>),
}

impl TypeRef {
    /// Create a New Simple Type
    ///
    /// A simple type is a direct reference to a non-generic non-array
    /// type, such as `Num` or `String`.
    pub fn simple<S: Into<String>>(name: S) -> Self {
        TypeRef::Simple(name.into())
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

    /// Get the Simple Name of a Type
    ///
    /// Panics if the type is not simple
    pub fn simple_name(&self) -> &str {
        match *self {
            TypeRef::Simple(ref name) => name.as_ref(),
            _ => panic!("not a simple type"),
        }
    }
}
