//! Rich Type Information
//!
//! This module contains the `Typ` structure which defines the
//! properties of builtin and user-specified types along with the
//! global type registry which is used during compilation for type
//! checking and inferrance.

use crate::syntax::text::Ident;
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

    /// A function. We can't store the type in here properly as that
    /// would break the `Copy` of `typ`. Instead we just store the
    /// ident of the function to use later when looking it back up.
    /// It's major HAXX.
    ///
    /// # Issues
    /// 
    /// FIXME: We need to create a sepration between the
    /// trivially-copyable `Typ` and some `TypeInfo` which contains
    /// the full information for the type rather than using this
    /// id-stashing workaround
    Function(Ident),
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
            Typ::Function(id) => return Cow::Owned(format!("Function({:?})", id)),
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
