//! Semantic Tree
//!
//! This module contains the types used to construct the
//! decorated/semantic expression tree.

use super::types::*;
use syntax::Constant;
use syntax::operators::PrefixOp;

/// A Semantically Decorated Expression
///
/// This struct represents the expression tree after semantic
/// analysis. This is no longer guaranteed to be a a lieral
/// representation of the code as it was written.
pub struct Expression {
    /// The contents of this expression.
    pub kind: ExpressionKind,

    /// The type of this node, if known
    pub typ: Option<Typ>,
}

/// The Expression Kind Enum
///
/// This enum contains a variant for the different types of expression
/// in the semantic tree. This is similar to the `syntax::Expression`
/// enum however some information may have been elided or reordered to
/// better suit the lowering process.
pub enum ExpressionKind {
    /// Literal Value
    ///
    /// A constant value. This is just plucked straight from the
    /// syntax tree.
    Literal(Constant),
    /// A Sequence of Expressions
    Sequence(Vec<Expression>),
    /// A prefix operator
    Prefix(PrefixOp, Box<Expression>),
    /// Print Expression
    ///
    /// Converts the inner expression to a string and prints it to
    /// standard output. This underpins the spec tests by allowing
    /// simple output.
    Print(Box<Expression>),
    /// FIXME: Move the different expression kinds in here.
    Fixme(::syntax::Expression),
}

impl Expression {
    /// Create a New Expression from parts
    ///
    /// Constructs a new semantic expression tree node from
    /// constituent parts. The type information for a given node can
    /// be set to none if no type inference has yet been run for this
    /// expression.
    pub fn new(kind: ExpressionKind, typ: Option<Typ>) -> Self {
        Expression { kind, typ }
    }
}