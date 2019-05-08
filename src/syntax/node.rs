//! Syntax node abstractions

use super::text::{SourceText, Span};

/// Syntax Node
///
/// This trait abstracts over things which can appear in a given
/// `SyntaxTree`.
pub trait SyntaxNode {
    /// A short description of the node. Used for tree pretty
    /// printing.
    fn description(&self, source: &SourceText) -> std::borrow::Cow<str>;

    /// Get the span this node covers in the tree
    ///
    /// If the node has no real location then `DUMMY_SPAN` is returned
    fn span(&self) -> Span;
}
