//! Semantic Tree
//!
//! This module contains the types used to construct the
//! decorated/semantic expression tree.

use super::types::*;

/// A Semantically Decorated Expression
///
/// This struct represents the expression tree after semantic
/// analysis. This is no longer guaranteed to be a a lieral
/// representation of the code as it was written.
pub struct Expression {
    /// TODO: Properly implement this.
    pub expr: ::syntax::Expression,

    /// The type of this node
    pub typ: Option<Typ>,
}
