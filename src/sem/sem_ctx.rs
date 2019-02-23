//! Semantic Context
//!
//! This module defines the state that is passes while transforming
//! expressions from AST representation to semantic.

use super::types::{BuiltinType, Typ};
use crate::syntax::text::{Ident, SourceText};
use crate::syntax::tree::TokenKind;
use crate::syntax::TypeRef;
use std::collections::HashMap;

/// SemCtx Structure
///
/// Holds the context when trasnforming. This is basically the current
/// type state information along with symbol table.
pub struct SemCtx<'a> {
    /// Symbol Table for Local Variables
    locals: Vec<HashMap<Ident, Typ>>,
    /// The source text
    source: &'a SourceText,
}

impl<'a> SemCtx<'a> {
    /// Create a new Semantic Context
    pub fn new(source: &'a SourceText) -> Self {
        SemCtx {
            locals: vec![HashMap::new()],
            source,
        }
    }

    /// Find Type in Context
    ///
    /// Returns the `sem::Typ` declaration for the type if one is
    /// available.
    pub fn sem_ty(&self, ast_ty: &TypeRef) -> Option<Typ> {
        // TODO: This should be looked up dynamically.
        Some(match *ast_ty {
            TypeRef::Unit(..) => Typ::Unit,
            TypeRef::Simple(ref name) => {
                let id = match name.kind {
                    TokenKind::Word(id) => id,
                    _ => panic!("Expected word token"),
                };
                let name = self.source().interned_value(id);
                match &name[..] {
                    "String" => Typ::Builtin(BuiltinType::String),
                    "Bool" => Typ::Builtin(BuiltinType::Bool),
                    "Number" => Typ::Builtin(BuiltinType::Number),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        })
    }

    /// Add Local
    ///
    /// Inserts a local declaration into the locals map.
    pub fn add_local(&mut self, id: Ident, typ: Typ) {
        self.locals[0].insert(id, typ);
    }

    /// Find a Local Declaration
    pub fn find_local(&self, id: Ident) -> Option<Typ> {
        self.locals[0].get(&id).cloned()
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

    /// Borrow the Source
    pub fn source(&self) -> &SourceText {
        &self.source
    }
}
