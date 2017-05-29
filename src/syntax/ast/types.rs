/// Type Reference
///
/// Represents a reference to a type. This could be a simple reference
/// to a named type or a complex type such as an array or Tuple.
#[derive(Debug,PartialEq)]
pub enum TypeRef {
    Simple(String),
    Unit,
    Tuple(Vec<TypeRef>),
    Array(Box<TypeRef>),
}

impl TypeRef {
    /// Create a New Simple Type
    ///
    /// A simple type is a direct reference to a non-generic non-array
    /// type, such as `Num` or `String`.
    pub fn simple(name: &str) -> Self {
        TypeRef::Simple(String::from(name))
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
        if inner.len() == 0 {
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

/// An identifier, with an optional type attached
#[derive(Debug,PartialEq)]
pub struct TypedId {
    pub typ: Option<TypeRef>,
    pub id: String,
}

impl TypedId {
    /// Create an Id with a Known Type
    ///
    /// Constructs a new idnetifier declaration where the identifier
    /// definitely has a known type.
    pub fn new(id: String, typ: TypeRef) -> Self {
        Self::from_parts(id, Some(typ))
    }

    /// Create an Id without a Known Type
    ///
    /// Constructs a new identifier declaraiton where the identifier
    /// does not have a type specified in the source. This is used
    /// where the type will be infered at a later date.
    pub fn new_without_type(id: String) -> Self {
        Self::from_parts(id, None)
    }

    /// Create an Id from Constituent Parts
    ///
    /// Used to construct a new identifier when a type has only
    /// optionally been specified.
    pub fn from_parts(id: String, typ: Option<TypeRef>) -> Self {
        TypedId { id: id, typ: typ }
    }
}
