//! Expression Binder
//!
//! This module contains the syntax binder. It's job is to walk the
//! incoming syntax tree and bind each part to produce the semantic
//! tree.
//!
//! At any given level the bind consists of two main steps. Firs the
//! list of expressions is walked to define any items that need to be
//! made available for mutual recursion. Once this walk is complete a
//! second traveral visits each item and binds symbol and name
//! information.

// FIXME: disable this once finished
#![allow(dead_code)]

use std::collections::{hash_map::Entry, HashMap};
use std::default::Default;

use super::Typ;
use super::{SemCtx, Expression, ExpressionKind, transform_expression};
use crate::diag::Diagnostic;
use crate::syntax::{self, text::{Ident, DUMMY_SPAN}};

/// Symbol
///
/// Symbols represent the different kinds of items that can be bound
/// to names in a given scope. Examples are function arguments, local
/// variables, and function declarations.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Symbol {
    /// Function argument or local variable
    Variable(Typ),
    /// A Function declaration
    Function,
}

/// Declaration Scope
///
/// Holds the declared items at a given level in the scope stack
/// during a bind.
pub struct Scope {
    /// Symbols declared in this scope
    symbols: HashMap<Ident, Symbol>,
    /// The parent of this scope if there is one. When looking items
    /// up parent scopes are searched if no matches are found in the
    /// child scopes.
    parent: Option<Box<Scope>>,
}

impl Default for Scope {
    /// Create the Default Scope
    fn default() -> Scope {
        Scope {
            symbols: Default::default(),
            parent: None,
        }
    }
}

impl Scope {
    /// Create a Scope
    ///
    /// The new scope doesn't have a parent scope. This scope is
    /// effectively a global scope.
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a Child Scope
    ///
    /// Builds a scope which forwards lookups for undefined items to
    /// the given parent scope.
    pub fn with_parent(parent: Scope) -> Self {
        Scope {
            symbols: Default::default(),
            parent: Some(Box::new(parent)),
        }
    }

    /// Lookup a Symbol
    ///
    /// Searches the current scope, and any parent scopes, for the
    /// given identifier. If any symbol is bound to the idnetifier it
    /// is returned otherwise `None` is returned.
    pub fn lookup(&self, ident: Ident) -> Option<Symbol> {
        if let Some(sym) = self.symbols.get(&ident) {
            return Some(*sym);
        }

        self.parent.as_ref().and_then(|p| p.lookup(ident))
    }

    /// Declare a Symbol
    ///
    /// Attempts to insert the given symbol into the symbol
    /// table. Returns `true` if the symbol was inserted succesfully.
    pub fn try_declare(&mut self, ident: Ident, sym: Symbol) -> bool {
        match self.symbols.entry(ident) {
            Entry::Occupied(_) => false,
            Entry::Vacant(v) => {
                v.insert(sym);
                true
            }
        }
    }
}

/// Syntax Binder
///
/// Holds the scope information and declared items for an ongoing
/// binding operation.
pub struct Binder {
    /// The current scope
    scope: Scope,
    /// The diagnostics for the current bind
    diagnostics: Vec<Diagnostic>,
}

impl Binder {
    /// Create Binder for the Given Scope
    pub fn new(scope: Scope) -> Self {
        Binder {
            scope,
            diagnostics: Vec::new()
        }
    }

    /// Bind an Expression
    ///
    /// Converts a syntax expression into a semantic one by binding it
    /// in the binder's current scope.
    pub fn bind_expression(&mut self, tree: syntax::SyntaxTree<'_>) -> Expression {
        let mut trans_sess = SemCtx::new(tree.source());
        let (expr, _end) = tree.into_parts();
        match transform_expression(&mut trans_sess, expr) {
            Ok(sem_expr) => {
                self.diagnostics.extend(trans_sess.into_diagnostics());
                sem_expr
            },
            Err(comp_err) => {
                let diag = Diagnostic::new(comp_err.to_string(), DUMMY_SPAN);
                self.diagnostics.push(diag);
                Expression::new(ExpressionKind::Sequence(Vec::new()), None)
            }
        }
    }

    /// Clears out the diagnostics list and returns any diagnostics
    /// that have been accumulated.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.drain(..).collect()
    }
}

#[cfg(test)]
mod test {
    use super::super::BuiltinType;
    use super::*;
    use crate::syntax::text::Interner;

    #[test]
    fn create_scope() {
        let mut interner = Interner::new();

        let parent = Scope::new();
        let child = Scope::with_parent(Default::default());

        assert_eq!(None, parent.lookup(interner.intern("foo")));
        assert_eq!(None, child.lookup(interner.intern("foo")));
    }

    #[test]
    fn declare_and_lookup() {
        let mut interner = Interner::new();
        let mut scope = Scope::new();
        let id = interner.intern("test§");

        assert!(scope.try_declare(id, Symbol::Variable(Typ::Unit)));
        let found = scope.lookup(id);
        assert!(!scope.try_declare(id, Symbol::Variable(Typ::Unit)));

        assert_eq!(Some(Symbol::Variable(Typ::Unit)), found);
    }

    #[test]
    fn declare_in_parent() {
        let mut interner = Interner::new();
        let foo_id = interner.intern("foo");
        let bar_id = interner.intern("bar");
        let baz_id = interner.intern("baz");

        let mut scope = Scope::new();
        assert!(scope.try_declare(foo_id, Symbol::Variable(Typ::Builtin(BuiltinType::Number))));
        assert!(scope.try_declare(bar_id, Symbol::Variable(Typ::Unit)));

        let mut scope = Scope::with_parent(scope);

        assert!(scope.try_declare(bar_id, Symbol::Variable(Typ::Builtin(BuiltinType::String))));
        assert!(scope.try_declare(baz_id, Symbol::Variable(Typ::Builtin(BuiltinType::Bool))));

        let foo_lookup = scope.lookup(foo_id);
        let bar_lookup = scope.lookup(bar_id);
        let baz_lookup = scope.lookup(baz_id);
        let failed = scope.lookup(interner.intern("nothere"));

        assert_eq!(
            Some(Symbol::Variable(Typ::Builtin(BuiltinType::Number))),
            foo_lookup
        );
        assert_eq!(
            Some(Symbol::Variable(Typ::Builtin(BuiltinType::String))),
            bar_lookup
        );
        assert_eq!(
            Some(Symbol::Variable(Typ::Builtin(BuiltinType::Bool))),
            baz_lookup
        );
        assert_eq!(None, failed);
    }

    #[test]
    fn create_binder()
    {
        let _binder = Binder::new(Scope::new());
    }
}
