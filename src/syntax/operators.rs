//! Enums for Operator Types
//!
//! There are two groups of operators in the syntax tree, infix and
//! prefix. This module contains a pair of enums used to discrimiate
//! between these variants.

/// Represents an AST prefix operator.
#[derive(Debug, PartialEq)]
pub enum PrefixOp {
    /// Unary Airthmetic Negation
    ///
    /// Represents the application of the `-` operator to an
    /// expression. Can be used to arithmetically negate a computed
    /// expression or literal value.
    Negate,

    /// Unary Boolean Negation
    ///
    /// Represents the application of the `!` operator to an
    /// expression. Can be used to perfrom boolean negation on an
    /// expression or literal value.
    Not,
}

/// Represents an AST infix operator
#[derive(Debug, PartialEq)]
pub enum InfixOp {
    /// Assignment Operator (`=`)
    Assign,

    /// Arithmetic Addition (`+`)
    Add,
    /// Arithmetic Subtraction (`-`)
    Sub,
    /// Arithmetic Multiplication (`*`)
    Mul,
    /// Arithmetic Division (`/`)
    Div,

    /// Boolean Equals (`==`)
    Eq,
    /// Boolean Not Equals (`!=`)
    NotEq,
    /// Less than comparision operator (`<`)
    Lt,
    /// Greater than comparison operator (`>`)
    Gt,
}
