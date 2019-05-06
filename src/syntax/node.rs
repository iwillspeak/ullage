//! Syntax node abstractions

use super::text::SourceText;

/// Syntax Node
///
/// This trait abstracts over things which can appear in a given
/// `SyntaxTree`.
pub trait SyntaxNode {
    /// A short description of the node. Used for tree pretty
    /// printing.
    fn description(&self, source: &SourceText) -> std::borrow::Cow<str>;
}
