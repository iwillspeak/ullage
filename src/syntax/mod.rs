//! Syntax parsing and expression tree
//!
//! This crate provides a simple abstract syntax tree, and a parser
//! implementation which recognises a simple lanugage using
//! Pratt-style operator precedence parsing.

pub mod parse;
pub mod ast;

use self::ast::operators::*;


/// Type Reference
///
/// Represents a reference to a type. This could be a simple reference
/// to a named type or a complex type such as an array or Tuple.
#[derive(Debug,PartialEq)]
pub enum TypeRef {
    Simple(String),
    Unit,
    Tuple(Vec<TypeRef>),
    Array(Box<TypeRef>),
}

impl TypeRef {
    /// Create a New Simple Type
    ///
    /// A simple type is a direct reference to a non-generic non-array
    /// type, such as `Num` or `String`.
    pub fn simple(name: &str) -> Self {
        TypeRef::Simple(String::from(name))
    }

    /// Create a new Unit Type Reference
    ///
    /// The unit type is represented as a struct with no contents. It
    /// has special meaning in some areas as it can be used to idicate
    /// the absence of a value.
    pub fn unit() -> Self {
        TypeRef::Unit
    }

    /// Create a Tuple Type
    ///
    /// A tuple type is an ordered collection of values. Each value
    /// can be of a different type.
    pub fn tuple(inner: Vec<TypeRef>) -> Self {
        if inner.len() == 0 {
            Self::unit()
        } else {
            TypeRef::Tuple(inner)
        }
    }

    /// Create an Array Type
    ///
    /// An array type represents a contiguous collection of another
    /// type.
    pub fn array(inner: TypeRef) -> Self {
        TypeRef::Array(Box::new(inner))
    }
}

/// An identifier, with an optional type attached
#[derive(Debug,PartialEq)]
pub struct TypedId {
    pub typ: Option<TypeRef>,
    pub id: String,
}

impl TypedId {
    /// Create an Id with a Known Type
    ///
    /// Constructs a new idnetifier declaration where the identifier
    /// definitely has a known type.
    pub fn new(id: String, typ: TypeRef) -> Self {
        Self::from_parts(id, Some(typ))
    }

    /// Create an Id without a Known Type
    ///
    /// Constructs a new identifier declaraiton where the identifier
    /// does not have a type specified in the source. This is used
    /// where the type will be infered at a later date.
    pub fn new_without_type(id: String) -> Self {
        Self::from_parts(id, None)
    }

    /// Create an Id from Constituent Parts
    ///
    /// Used to construct a new identifier when a type has only
    /// optionally been specified.
    pub fn from_parts(id: String, typ: Option<TypeRef>) -> Self {
        TypedId { id: id, typ: typ }
    }
}

/// Literal / Constant Value
#[derive(Debug,PartialEq)]
pub enum Constant {
    Number(i64),
    Bool(bool),
    String(String),
}

/// Represents an AST expression.
#[derive(Debug,PartialEq)]
pub enum Expression {
    Identifier(String),
    Literal(Constant),
    Prefix(PrefixOp, Box<Expression>),
    Infix(Box<Expression>, InfixOp, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>),
    Function(String, TypeRef, Vec<TypedId>, Box<Expression>),
    Loop(Box<Expression>, Box<Expression>),
    Sequence(Vec<Expression>),
    Print(Box<Expression>),
    Declaration(TypedId, bool, Box<Expression>),
}

impl Expression {
    /// # New Identifier Expression
    ///
    /// A reference to an identifier, either as a variable reference
    /// or declaration, part of a function definition or function call.
    pub fn identifier(s: String) -> Self {
        Expression::Identifier(s.into())
    }

    /// # New Numeric Constant
    ///
    /// A constant numeric value, either specified inline using a
    /// numeric literal or compputed from other known compile-time
    /// constants.
    pub fn constant_num(n: i64) -> Self {
        Expression::Literal(Constant::Number(n))
    }

    /// # New String Constant
    ///
    /// A constant string value, either specified inline using a
    /// string literal or computed from other known compile-time
    /// constants.
    pub fn constant_string(s: String) -> Self {
        Expression::Literal(Constant::String(s))
    }

    /// New Bool Constant
    ///
    /// A constant boolean value. Created from the literal 'true' or 'false'.
    pub fn constant_bool(b: bool) -> Self {
        Expression::Literal(Constant::Bool(b))
    }

    /// # New Prefix Operator Expression
    ///
    /// Represents the application of a prefix unary operator to
    /// another expression.
    pub fn prefix(op: PrefixOp, expr: Expression) -> Self {
        Expression::Prefix(op, Box::new(expr))
    }

    /// # New Infix Operator Expression
    ///
    /// Represents the application of an infix binary operator to two
    /// expression operands.
    pub fn infix(lhs: Expression, op: InfixOp, rhs: Expression) -> Self {
        Expression::Infix(Box::new(lhs), op, Box::new(rhs))
    }

    /// # New Function Call Expression
    ///
    /// Represents calling a given function with a numer of arguments.
    pub fn call(callee: Expression, args: Vec<Expression>) -> Self {
        Expression::Call(Box::new(callee), args)
    }

    /// # New Index Expression
    ///
    /// Represents indexing one expression by another. This could be
    /// an array lookup, or slice operation.
    pub fn index(lhs: Expression, index: Expression) -> Self {
        Expression::Index(Box::new(lhs), Box::new(index))
    }

    /// # New If Then Else Expression
    ///
    /// Represents either a single conditional expression, or a
    /// ternary expression.
    pub fn if_then_else(iff: Expression, then: Expression, els: Expression) -> Self {
        Expression::IfThenElse(Box::new(iff), Box::new(then), Box::new(els))
    }

    /// # New Function Definition
    ///
    /// Create a function delcaration builder. This can be used to
    /// create a function expression.
    pub fn function(id: String) -> FunctionDeclarationBuilder {
        FunctionDeclarationBuilder {
            id: id,
            typ: TypeRef::unit(),
            args: Vec::new(),
            body: Vec::new(),
        }
    }

    /// # New Loop Expression
    ///
    /// Represents the repeated evaluation of an expression until a
    /// condition changes.
    pub fn loop_while(condition: Expression, body: Vec<Expression>) -> Self {
        let body = Expression::sequence(body);
        Expression::Loop(Box::new(condition), Box::new(body))
    }

    /// # New Variable Declaration
    ///
    /// Represents the declaration of a local variable.
    pub fn declaration(var: TypedId, is_mut: bool, expr: Expression) -> Self {
        Expression::Declaration(var, is_mut, Box::new(expr))
    }

    /// # New Sequence Expression
    ///
    /// Represents a sequence of expressions evaluated one after the other.
    pub fn sequence(exprs: Vec<Expression>) -> Self {
        Expression::Sequence(exprs)
    }

    /// # Print Expression
    ///
    /// Evaluates an inner expression, prints it to standard output,
    /// and then returns the inner expression's value.
    pub fn print(expr: Expression) -> Self {
        Expression::Print(Box::new(expr))
    }
}

/// # Builder Struct for Function Declarations
pub struct FunctionDeclarationBuilder {
    id: String,
    typ: TypeRef,
    args: Vec<TypedId>,
    body: Vec<Expression>,
}

/// # Builder for Function Declarations
///
/// This can be used to iteratively construct a function declaration.
///
/// If no call to any of the methods are made then it is assuemd that
/// the return type is `()`, the function acepts no arguments and the
/// body is empty.
impl FunctionDeclarationBuilder {
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
