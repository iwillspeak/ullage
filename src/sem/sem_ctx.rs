//! Semantic Context
//!
//! This module defines the state that is passes while transforming
//! expressions from AST representation to semantic.

use super::types::{BuiltinType, Typ};
use syntax::TypeRef;

/// SemCtx Structure
///
/// Holds the context when trasnforming. This is basically the current
/// type state information along with symbol table.
pub struct SemCtx;

impl SemCtx {
    /// Create a new Semantic Context
    pub fn new() -> Self {
        SemCtx
    }

    /// Find Type in Context
    ///
    /// Returns the `sem::Typ` declaration for the type if one is
    /// available.
    pub fn sem_ty(&self, ast_ty: TypeRef) -> Option<Typ> {
        // TODO: This should be looked up dynamically.
        Some(match ast_ty {
            TypeRef::Unit => Typ::Unit,
            TypeRef::Simple(name) => match &name[..] {
                "String" => Typ::Builtin(BuiltinType::String),
                "Bool" => Typ::Builtin(BuiltinType::Bool),
                "Number" => Typ::Builtin(BuiltinType::Number),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        })
    }

}
