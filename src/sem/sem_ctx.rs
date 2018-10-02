//! Semantic Context
//!
//! This module defines the state that is passes while transforming
//! expressions from AST representation to semantic.

use std::collections::HashMap;
use super::types::{BuiltinType, Typ};
use crate::syntax::TypeRef;

/// SemCtx Structure
///
/// Holds the context when trasnforming. This is basically the current
/// type state information along with symbol table.
pub struct SemCtx {
    /// Symbol Table for Local Variables
    locals: Vec<HashMap<String, Typ>>,
}

impl SemCtx {
    /// Create a new Semantic Context
    pub fn new() -> Self {
        SemCtx {
            locals: vec![
                HashMap::new()
            ]
        }
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

    /// Add Local
    ///
    /// Inserts a local declaration into the locals map.
    pub fn add_local<S>(&mut self, id: S, typ: Typ)
        where S: Into<String>
    {
        self.locals[0].insert(id.into(), typ);
    }

    /// Find a Local Declaration
    pub fn find_local(&self, id: &str) -> Option<Typ> {
        self.locals[0].get(id).cloned()
    }

    /// Push Scope
    ///
    /// Add a new empty scope to the top of the scope stack
    pub fn push_scope(&mut self) {
        self.locals.push(HashMap::new())
    }

    /// Pop Scope
    ///
    /// Remove and discard the top scope from the stack
    pub fn pop_scope(&mut self) {
        self.locals.pop();
    }
}
