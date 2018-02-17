//! Rich Type Information
//!
//! This module contains the `Typ` structure which defines the
//! properties of builtin and user-specified types along with the
//! global type registry which is used during compilation for type
//! checking and inferrance.

/// Semantic Type
///
/// This enum defines the different type values that each node in the
/// semantic tree could have.
#[derive(Clone)]
pub enum Typ {
    /// The unit type
    Unit,

    /// One of the given basic types in the language.
    Builtin(BuiltinType),
}

/// The fixed builtin types
#[derive(Clone)]
pub enum BuiltinType {
    /// 64 bit numerical value
    Number,
    /// Boolean
    Bool,
    /// String type. This is a pointer-lenght pair
    String,
}
