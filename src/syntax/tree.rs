//! Syntax Tree
//!
//! This module defines the types which make up the parsed syntax
//! tree. This tree defines the full strcuture of a parsed source file
//! before any semantic transformation is done.

pub mod expression;
pub mod operators;
mod token;
mod trivia;
pub mod types;

pub use self::token::{Literal, Token, TokenKind};
pub use self::trivia::{TriviaToken, TriviaTokenKind};

use expression::Expression;

/// Syntax tree
///
/// The syntax tree represents the parsed source of a given file. It
/// contains multiple expressions followed by an end of file token.
pub struct SyntaxTree {
    /// The root of the main expression tree
    root: Expression,
    /// Diagnostics related to the given tree
    diagnostics: Vec<String>,
    /// End token
    end: Token,
}

impl SyntaxTree {
    /// Create a new syntax tree
    ///
    /// The syntax tree represents a single parsed item of source
    /// text. Syntax trees are usually constructed by the compiler.
    ///
    /// # Parameters
    ///
    ///  * `root`: The body of the file. This could be an empty
    ///  sequence if the file is empty
    ///  * `diagnostics`: Diagnostics raised in the parsing of the
    ///  source.
    ///  * `end`: The closing EOF token. This may have some leading
    ///  trivia attached and is therefore required for a full-fidelity
    ///  tree.
    pub fn new(root: Expression, diagnostics: Vec<String>, end: Token) -> Self {
        SyntaxTree {
            root,
            diagnostics,
            end,
        }
    }

    /// Get the root of the tree
    pub fn root(&self) -> &Expression {
        &self.root
    }

    /// Get the end token
    pub fn end(&self) -> &Token {
        &self.end
    }

    /// Get diagnostics
    pub fn diagnostics(&self) -> &[String] {
        &self.diagnostics
    }

    /// Check if the tree has buffered diagnostics
    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn tree_without_diagnositcs_reports_false() {
        let tree = SyntaxTree::new(Expression::empty(), Vec::new(), Token::new(TokenKind::End));

        assert_ne!(true, tree.has_diagnostics());
    }

    #[test]
    fn tree_with_diagnostics_reports_true() {
        let tree = SyntaxTree::new(
            Expression::empty(),
            vec![String::from("error: test")],
            Token::new(TokenKind::End),
        );

        assert_eq!(true, tree.has_diagnostics());
    }
}
