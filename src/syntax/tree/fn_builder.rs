//! Function Bilder Support
//!
//! Contains the `Functiondeclarationbuilder` which is used by the
//! parser when creating `Expression::Function`s.

use super::expression::{Expression, TypedId};
use super::types::TypeRef;
use crate::syntax::text::Ident;

/// Builder Struct for Function Declarations
///
/// This can be used to iteratively construct a function declaration.
/// Useful to be able to build up the creation of a function piece by
/// piece when parsing.
///
/// If no call to any of the methods are made then it is assuemd that
/// the return type is `()`, the function acepts no arguments and the
/// body is empty.
pub struct FunctionDeclarationBuilder {
    id: Ident,
    typ: TypeRef,
    args: Vec<TypedId>,
    body: Vec<Expression>,
}

impl FunctionDeclarationBuilder {
    /// Create a New Function Builder
    ///
    /// Start building a function for `id`.
    pub fn new(id: Ident) -> Self {
        FunctionDeclarationBuilder {
            id,
            typ: TypeRef::unit(),
            args: Vec::new(),
            body: Vec::new(),
        }
    }

    /// # Append Function Arugment
    ///
    /// Adsd an optionally-typed argument declaration to this function
    /// declartion. If no type is specified it should be inferred
    /// later.
    ///
    /// # Returns
    ///
    /// The modified builder, to continue building this declaration.
    pub fn with_arg(mut self, param: TypedId) -> Self {
        self.args.push(param);
        self
    }

    /// # Set Return Type
    ///
    /// Update the return type of the function.
    ///
    /// # Returns
    ///
    /// The modified builder, to continue building this declaration.
    pub fn with_return_type(mut self, typ: TypeRef) -> Self {
        self.typ = typ;
        self
    }

    /// # Set the Function Body
    ///
    /// Update the function body to the given sequence of expressions.
    ///
    /// # Returns
    ///
    /// The modified builder, to continue building this declaration.
    pub fn with_body(mut self, body: Vec<Expression>) -> Self {
        self.body = body;
        self
    }
}

/// Support Converting the Builder into an Expression
impl From<FunctionDeclarationBuilder> for Expression {
    fn from(builder: FunctionDeclarationBuilder) -> Expression {
        let body = Expression::sequence(builder.body);
        Expression::Function(builder.id, builder.typ, builder.args, Box::new(body))
    }
}
