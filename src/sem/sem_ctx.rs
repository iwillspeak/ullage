//! Semantic Context
//!
//! This module defines the state that is passes while transforming
//! expressions from AST representation to semantic.

use super::types::{BuiltinType, Typ};
use crate::diag::Diagnostic;
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
    /// Named types map
    named_types: HashMap<Ident, Typ>,
    /// The source text
    source: &'a SourceText,
    /// A collection of diagnostics emitted when transforming
    diagnostics: Vec<Diagnostic>,
}

impl<'a> SemCtx<'a> {
    /// Create a new Semantic Context
    pub fn new(source: &'a SourceText) -> Self {
        SemCtx {
            locals: vec![HashMap::new()],
            named_types: [
                (source.intern("String"), Typ::Builtin(BuiltinType::String)),
                (source.intern("Bool"), Typ::Builtin(BuiltinType::Bool)),
                (source.intern("Number"), Typ::Builtin(BuiltinType::Number)),
            ]
            .iter()
            .cloned()
            .collect(),
            source,
            diagnostics: Vec::new(),
        }
    }

    /// Find Type in Context
    ///
    /// Returns the `sem::Typ` declaration for the type if one is
    /// available.
    pub fn sem_ty(&self, ast_ty: &TypeRef) -> Option<Typ> {
        Some(match *ast_ty {
            TypeRef::Unit(..) => Typ::Unit,
            TypeRef::Simple(ref name) => {
                let id = match name.kind {
                    TokenKind::Word(id) => id,
                    _ => panic!("Expected word token"),
                };
                return self.named_types.get(&id).cloned();
            }
            // TODO: array and tuple types
            TypeRef::Array(..) => unimplemented!("array types are not yet supported"),
            TypeRef::Tuple(..) => unimplemented!("tuple types are not yet supported"),
            TypeRef::Missing => panic!("Can't lower missing type"),
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

    /// Emit a diagnostic into the context
    pub fn emit(&mut self, diagnostic: Diagnostic)
    {
        self.diagnostics.push(diagnostic);
    }

    /// Check if there are any diagnostics in the translation session
    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Get the diagnostics
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}
