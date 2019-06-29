//! Rich Type Information
//!
//! This module contains the `Typ` structure which defines the
//! properties of builtin and user-specified types along with the
//! global type registry which is used during compilation for type
//! checking and inferrance.

use std::borrow::Cow;

/// Semantic Type
///
/// This enum defines the different type values that each node in the
/// semantic tree could have.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum Typ {
    /// The given expression is invalid. Futher binding or inference
    /// based off this type is meaningless.
    Error,

    /// Type yet to be inferred.
    Unknown,

    /// The unit type
    Unit,

    /// One of the given basic types in the language.
    Builtin(BuiltinType),
}

impl Typ {
    /// Returns the name of a given type
    pub fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(match *self {
            Typ::Error => "!ERROR!",
            Typ::Unknown => "_",
            Typ::Unit => "()",
            Typ::Builtin(ref b) => match *b {
                BuiltinType::Number => "Number",
                BuiltinType::Bool => "Bool",
                BuiltinType::String => "String",
            },
        })
    }
}

/// The fixed builtin types
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum BuiltinType {
    /// 64 bit numerical value
    Number,
    /// Boolean
    Bool,
    /// String type. This is a pointer-lenght pair
    String,
}
