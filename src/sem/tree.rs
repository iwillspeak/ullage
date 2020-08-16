//! Semantic Tree
//!
//! This module contains the types used to construct the
//! decorated/semantic expression tree.

use super::types::*;
use crate::syntax::{Constant, InfixOp, PrefixOp};

/// A Function Decclaration
///
/// Represents the context contained in the semantic tree for a
/// function declaration. Defined as a struct for convenience.
#[derive(Debug, PartialEq)]
pub struct FnDecl {
    /// The declaration's logical name
    pub ident: String,

    /// The return type of the function
    pub ret_ty: Typ,

    /// Parameters to the function
    pub params: Vec<VarDecl>,

    /// The body of the function
    ///
    /// The function's return is the value of the expression
    pub body: Box<Expression>,
}

/// Variable Declaration
///
/// Represents the binding of a given type to an identifier to create
/// a variable. Used both for local variable declarations as well as
/// function parameters.
#[derive(Debug, PartialEq)]
pub struct VarDecl {
    /// The logical name of the declataion
    ///
    /// FIXME: proper idents rather than strings in the bound tree.
    pub ident: String,

    /// The type of the identifier, if one was specified or inferred.
    pub ty: Typ,
}

/// A Semantically Decorated Expression
///
/// This struct represents the expression tree after semantic
/// analysis. This is no longer guaranteed to be a a lieral
/// representation of the code as it was written.
#[derive(Debug, PartialEq)]
pub struct Expression {
    /// The contents of this expression
    ///
    /// This is a union of the different expression kinds.
    pub kind: ExpressionKind,

    /// The type of this node
    ///
    /// All expressions in the tree will have some form of type. Error
    /// expressions or expressions where the type can't be calculated
    /// due to a syntax or semantic error in the program are typed
    /// `Typ::Error`. Parts of the tree where the type has yet to be
    /// inferred are typed `Typ::Unknown`.
    ///
    /// Before lowering a well-formed tree should contain neither of
    /// these types.
    pub typ: Typ,
}

/// The Expression Kind Enum
///
/// This enum contains a variant for the different types of expression
/// in the semantic tree. This is similar to the `syntax::Expression`
/// enum however some information may have been elided or reordered to
/// better suit the lowering process.
#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    /// Invalid Expression
    ///
    /// Emitted when transformation or binding can't produce a
    /// properly typed or bound expression. This should only be
    /// produced if a diagnostic has already been raised. Further
    /// passes can safely return error if an `Error` expression is
    /// encountered to avoid error cascades.
    Error,

    /// Identifier
    ///
    /// Variable reference. If this is an lvalue then it represents a
    /// write to the named variable, otherwise a read. Identifier
    /// expressions appear in other compound expressions such as
    /// functions when those expressions reference bound values.
    Identifier(String),

    /// Literal Value
    ///
    /// A constant value. This is just plucked straight from the
    /// syntax tree.
    Literal(Constant),

    /// A prefix operator
    Prefix(PrefixOp, Box<Expression>),

    /// An infix operator
    ///
    /// This only counts comparison and arithmetic operators as infix
    /// operators. Assignment is handled by the assignment node.
    Infix(Box<Expression>, InfixOp, Box<Expression>),

    /// Call Expression
    ///
    /// Represents the applicaiton of arguments to a function. The
    /// callee is represented as an expression as more than just
    /// identifiers are callable.
    Call(Box<Expression>, Vec<Expression>),

    /// Assignment
    Assignment(String, Box<Expression>),

    /// Index Expression
    Index(Box<Expression>, Box<Expression>),

    /// If Then Else
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>),

    /// Function Declaraiton
    ///
    /// A function declaration expression both registers a function in
    /// the symbol table and represents the callable function itself.
    Function(FnDecl),

    /// Loop with Condition
    Loop(Box<Expression>, Box<Expression>),

    /// A Sequence of Expressions
    Sequence(Vec<Expression>),

    /// Print Expression
    ///
    /// Converts the inner expression to a string and prints it to
    /// standard output. This underpins the spec tests by allowing
    /// simple output.
    Print(Box<Expression>),

    /// Variable Declaration
    Declaration(VarDecl, bool, Box<Expression>),
}

impl Expression {
    /// Create a New Expression from parts
    ///
    /// Constructs a new semantic expression tree node from
    /// constituent parts. The type information for a given node can
    /// be set to `Typ::Unknown` if no type inference has yet been run
    /// for this expression.
    pub fn new(kind: ExpressionKind, typ: Typ) -> Self {
        Expression { kind, typ }
    }

    /// Create an Error Expresion
    ///
    /// Convenience function for returning error expressions. Error
    /// expressions have a kind of `ExpressionKind::Error` and a type
    /// of `Typ::Error`. They are used to mark invalid or
    /// uncalculateable portions of the bound tree.
    pub fn error() -> Self {
        Expression::new(ExpressionKind::Error, Typ::Error)
    }
}
