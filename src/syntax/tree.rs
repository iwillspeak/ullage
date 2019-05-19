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

use std::io::{self, prelude::*};

use crate::diag::Diagnostic;
use crate::parse::Parser;
use crate::text::SourceText;

pub use self::token::{Literal, Token, TokenKind};
pub use self::trivia::{TriviaToken, TriviaTokenKind};

use super::SyntaxNode;
use expression::Expression;

/// Syntax tree
///
/// The syntax tree represents the parsed source of a given file. It
/// contains multiple expressions followed by an end of file token.
pub struct SyntaxTree<'a> {
    /// The root of the main expression tree
    root: Expression,
    /// Diagnostics related to the given tree
    diagnostics: Vec<Diagnostic>,
    /// End token
    end: Token,
    /// The source for this tree
    source: &'a SourceText,
}

impl<'a> SyntaxTree<'a> {
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
    pub fn new(
        source: &'a SourceText,
        root: Expression,
        diagnostics: Vec<Diagnostic>,
        end: Token,
    ) -> Self {
        SyntaxTree {
            root,
            diagnostics,
            end,
            source,
        }
    }

    /// Parse a tree from source text
    pub fn parse(source: &'a SourceText) -> Self {
        Parser::new(source).parse()
    }

    /// Parse a source tree containing a single expression
    pub fn parse_single(source: &'a SourceText) -> Self {
        Parser::new(source).parse_single()
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
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Check if the tree has buffered diagnostics
    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Returns the root of the expression tree and the EOF token
    ///
    /// FIXME: should root and token just be public and remove this,
    /// `root()`, and `end()`?
    pub fn into_parts(self) -> (Expression, Token) {
        (self.root, self.end)
    }

    /// Access the Borrowed Source
    ///
    /// Allows access to the source this syntax tree was parsed from.
    pub fn source(&self) -> &'a SourceText {
        self.source
    }

    /// Dump the Expression Tree
    ///
    /// Walks the subnodes of this tree and prints a text representation
    /// of them as an ASCII tree
    pub fn write_to<W>(&self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        let mut writer = io::BufWriter::new(writer);
        let mut prefix = String::new();
        pretty_tree(&mut writer, self.source, self.root(), &mut prefix, "•")
    }
}

///
/// Walks the subnodes of this tree and prints a text representation
/// of them as an ASCII tree.
fn pretty_tree<W>(
    writer: &mut io::BufWriter<W>,
    source: &SourceText,
    expr: &Expression,
    prefix: &mut String,
    lead: &str,
) -> io::Result<()>
where
    W: io::Write,
{
    writeln!(writer, "{}{} {}", prefix, lead, expr.description(source))?;
    let children: Vec<&Expression> = match expr {
        Expression::Identifier(_) => Vec::new(),
        Expression::Literal(_) => Vec::new(),
        Expression::Prefix(p) => vec![&p.inner],
        Expression::Infix(i) => vec![&i.left, &i.right],
        Expression::Call(c) => std::iter::once(&*c.callee)
            .chain(c.arguments.iter().map(|a| a.as_inner()))
            .collect(),
        Expression::Index(i) => vec![&i.index, &i.indexee],
        Expression::IfThenElse(i) => vec![&i.cond, &i.if_true, &i.if_false],
        Expression::Function(f) => vec![&f.body.contents],
        Expression::Loop(l) => vec![&l.condition, &l.body.contents],
        Expression::Sequence(s) => s.iter().collect(),
        Expression::Print(p) => vec![&p.inner],
        Expression::Declaration(d) => vec![&d.initialiser],
        Expression::Grouping(g) => vec![&g.inner],
    };

    let orig_prefix_len = prefix.len();
    match lead {
        "└─" => prefix.push_str("  "),
        "├─" => prefix.push_str("│ "),
        _ => (),
    }
    if let Some((last, rest)) = children.split_last() {
        for child in rest {
            pretty_tree(writer, source, child, prefix, "├─")?;
        }
        pretty_tree(writer, source, last, prefix, "└─")?;
    }
    if orig_prefix_len < prefix.len() {
        prefix.truncate(orig_prefix_len);
    }
    Ok(())
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::syntax::text::{SourceText, DUMMY_SPAN};

    #[test]
    fn tree_without_diagnositcs_reports_false() {
        let source = SourceText::new("");
        let tree = SyntaxTree::new(
            &source,
            Expression::empty(),
            Vec::new(),
            Token::new(TokenKind::End),
        );

        assert_ne!(true, tree.has_diagnostics());
    }

    #[test]
    fn tree_with_diagnostics_reports_true() {
        let source = SourceText::new("");
        let tree = SyntaxTree::new(
            &source,
            Expression::empty(),
            vec![Diagnostic::new("error: test", DUMMY_SPAN)],
            Token::new(TokenKind::End),
        );

        assert_eq!(true, tree.has_diagnostics());
    }

    #[test]
    fn tree_write_to_string() {
        let source = SourceText::new("(1 + 2) - 3");
        let tree = SyntaxTree::parse(&source);
        let mut buff = Vec::new();

        tree.write_to(&mut buff).unwrap();
        let written = String::from_utf8(buff).unwrap();

        assert_eq!(
            "
• Sequence
└─ Infix <Sub>
  ├─ Grouping
  │ └─ Infix <Add>
  │   ├─ Literal <Number(1)>
  │   └─ Literal <Number(2)>
  └─ Literal <Number(3)>
"
            .trim(),
            written.trim()
        );
    }
}
