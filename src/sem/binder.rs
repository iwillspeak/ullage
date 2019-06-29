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

use std::collections::{hash_map::Entry, HashMap};
use std::default::Default;

use super::tree::VarDecl;
use super::{BuiltinType, Expression, ExpressionKind, Typ};
use crate::diag::Diagnostic;
use crate::syntax::{
    self,
    text::{Ident, SourceText},
    Constant, TokenKind, TypeRef, VarStyle,
};

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
    /// A type
    Type(Typ),
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
            diagnostics: Vec::new(),
        }
    }

    /// Bind an Expression
    ///
    /// Converts a syntax expression into a semantic one by binding it
    /// in the binder's current scope.
    pub fn bind_tree(&mut self, tree: syntax::SyntaxTree<'_>) -> Expression {
        let source = tree.source();
        let (expr, _end) = tree.into_parts();
        add_builtin_types(&mut self.scope, source);
        self.bind_expression(&expr, source)
    }

    /// Bind a Single Expression
    pub fn bind_expression(
        &mut self,
        expression: &syntax::Expression,
        source: &SourceText,
    ) -> Expression {
        use syntax::Expression::*;
        match *expression {
            Identifier(ref ident) => self.bind_identifier(ident, source),
            Literal(ref lit) => self.bind_literal(lit),
            // TODO: bind remainning expression kinds
            Sequence(ref exprs) => self.bind_sequence(&exprs[..], source),
            Print(ref print) => self.bind_print(print, source),
            Declaration(ref decl) => self.bind_declaration(decl, source),
            Grouping(ref group) => self.bind_expression(&group.inner, source),
            _ => Expression::error(),
        }
    }

    /// Bind a refernece to an identifier
    pub fn bind_identifier(
        &mut self,
        ident: &syntax::IdentifierExpression,
        source: &SourceText,
    ) -> Expression {
        match self.scope.lookup(ident.ident) {
            Some(Symbol::Variable(typ)) => {
                let id_str = source.interned_value(ident.ident);
                Expression::new(ExpressionKind::Identifier(id_str), Some(typ))
            }
            Some(_) => {
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Attempt to read from '{}' which isn't a vairable",
                        source.interned_value(ident.ident)
                    ),
                    ident.token.span(),
                ));
                Expression::error()
            }
            None => {
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Reference to undefined item '{}'",
                        source.interned_value(ident.ident)
                    ),
                    ident.token.span(),
                ));
                Expression::error()
            }
        }
    }

    /// Bind a literal value
    pub fn bind_literal(&mut self, lit: &syntax::LiteralExpression) -> Expression {
        let constant_value = lit.value.clone();
        let typ = Typ::Builtin(match constant_value {
            Constant::Bool(_) => BuiltinType::Bool,
            Constant::Number(_) => BuiltinType::Number,
            Constant::String(_) => BuiltinType::String,
        });
        Expression::new(ExpressionKind::Literal(constant_value), Some(typ))
    }

    /// Bind a sequence of expressions
    pub fn bind_sequence(
        &mut self,
        exprs: &[syntax::Expression],
        source: &SourceText,
    ) -> Expression {
        // TODO: Do we want to two-pass this and allow the option to
        // declare things? This would allow us to declare the
        // functions in a given block makign them available to be
        // called with known types...

        let transformed: Vec<_> = exprs
            .iter()
            .map(|e| self.bind_expression(e, source))
            .collect();
        let typ = transformed.last().and_then(|e| e.typ).unwrap_or(Typ::Unit);
        Expression::new(ExpressionKind::Sequence(transformed), Some(typ))
    }

    /// Bind a `print` expression
    pub fn bind_print(
        &mut self,
        print: &syntax::PrintExpression,
        source: &SourceText,
    ) -> Expression {
        let bound_printee = self.bind_expression(&print.inner, source);
        // TODO: Does the print expression convert things to `String`s?
        let typ = bound_printee.typ;
        Expression::new(ExpressionKind::Print(Box::new(bound_printee)), typ)
    }

    /// Bind Variable Declaration Statement
    pub fn bind_declaration(
        &mut self,
        decl: &syntax::DeclarationExpression,
        source: &SourceText,
    ) -> Expression {
        let decl_type = if let Some(anno) = &decl.id.typ {
            self.bind_type(&anno.type_ref)
        } else {
            Typ::Unknown
        };
        let bound_initialiser = self.bind_expression(&decl.initialiser, source);

        let id = decl.id.id;

        // If we don't have a type annotation in the declaration then
        // infer the type from the initialiser
        let ty = if decl_type != Typ::Unknown {
            match bound_initialiser.typ {
                Some(t) if t != decl_type => {
                    // The declaration type doesn't match the
                    // expression being used to initialise it.
                    self.diagnostics.push(Diagnostic::new(
                        format!(
                            "Initialiser doesn't match declaration type for '{}'",
                            source.interned_value(id)
                        ),
                        decl.id.id_tok.span(),
                    ));
                    Some(Typ::Error)
                }
                _ => Some(decl_type),
            }
        } else {
            bound_initialiser.typ
        };

        self.scope
            .try_declare(id, Symbol::Variable(ty.unwrap_or(Typ::Unknown)));

        let is_mut = decl.style == VarStyle::Mutable;
        Expression::new(
            ExpressionKind::Declaration(
                VarDecl {
                    ident: source.interned_value(id),
                    ty,
                },
                is_mut,
                Box::new(bound_initialiser),
            ),
            ty,
        )
    }

    /// Bind the type in the current scope
    ///
    /// Looks the type up if there is an annotation. If the annotation
    /// is missing then `None` is retunred.
    pub fn bind_type(&mut self, ty_ref: &TypeRef) -> Typ {
        match *ty_ref {
            TypeRef::Unit(..) => Typ::Unit,
            TypeRef::Simple(ref name) => {
                let id = match name.kind {
                    TokenKind::Word(id) => id,
                    _ => panic!("Expected word token"),
                };
                match self.scope.lookup(id) {
                    Some(Symbol::Type(ty)) => ty.clone(),
                    _ => {
                        self.diagnostics
                            .push(Diagnostic::new("Reference to undefined type", name.span()));
                        Typ::Error
                    }
                }
            }
            // TODO: array and tuple types
            TypeRef::Array(..) => unimplemented!("array types are not yet supported"),
            TypeRef::Tuple(..) => unimplemented!("tuple types are not yet supported"),
            TypeRef::Missing => panic!("Can't lower missing type"),
        }
    }

    /// Clears out the diagnostics list and returns any diagnostics
    /// that have been accumulated.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.diagnostics.drain(..).collect()
    }
}

/// Add the Default Type Declarations
fn add_builtin_types(scope: &mut Scope, source: &SourceText) {
    scope.try_declare(
        source.intern("String"),
        Symbol::Type(Typ::Builtin(BuiltinType::String)),
    );
    scope.try_declare(
        source.intern("Bool"),
        Symbol::Type(Typ::Builtin(BuiltinType::Bool)),
    );
    scope.try_declare(
        source.intern("Number"),
        Symbol::Type(Typ::Builtin(BuiltinType::Number)),
    );
}

#[cfg(test)]
mod test {
    use super::super::BuiltinType;
    use super::*;
    use crate::syntax::text::Interner;
    use crate::syntax::{IdentifierExpression, Literal, LiteralExpression, Token, TokenKind};

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
    fn test_add_default_types() {
        let mut scope = Scope::new();
        let source = SourceText::new("");

        add_builtin_types(&mut scope, &source);

        let string_lookup = scope.lookup(source.intern("String"));
        assert_eq!(
            Some(Symbol::Type(Typ::Builtin(BuiltinType::String))),
            string_lookup
        );

        let bool_lookup = scope.lookup(source.intern("Bool"));
        assert_eq!(
            Some(Symbol::Type(Typ::Builtin(BuiltinType::Bool))),
            bool_lookup
        );

        let num_lookup = scope.lookup(source.intern("Number"));
        assert_eq!(
            Some(Symbol::Type(Typ::Builtin(BuiltinType::Number))),
            num_lookup
        );
    }

    #[test]
    fn bind_lookup() {
        let source = SourceText::new("");
        let mut scope = Scope::new();
        scope.try_declare(
            source.intern("melles"),
            Symbol::Variable(Typ::Builtin(BuiltinType::Bool)),
        );
        let mut binder = Binder::new(scope);

        let bound = binder.bind_identifier(
            &IdentifierExpression {
                ident: source.intern("melles"),
                token: Box::new(Token::new(TokenKind::Word(source.intern("melles")))),
            },
            &source,
        );

        assert_eq!(ExpressionKind::Identifier("melles".into()), bound.kind);
        assert_eq!(Some(Typ::Builtin(BuiltinType::Bool)), bound.typ);
    }

    #[test]
    fn bind_const() {
        let mut binder = Binder::new(Scope::new());

        let bound = binder.bind_literal(&LiteralExpression {
            token: Box::new(Token::new(TokenKind::Literal(Literal::Number(1337)))),
            value: Constant::Number(1337),
        });

        assert_eq!(ExpressionKind::Literal(Constant::Number(1337)), bound.kind);
        assert_eq!(Some(Typ::Builtin(BuiltinType::Number)), bound.typ);
    }

    // TODO: Do we want some kind of snapshot testing here to check
    //       the transformation / bind of known parses?
}
