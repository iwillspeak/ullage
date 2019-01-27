//! Syntax Expression
//!
//! A syntax expression represents the value of a given node in the
//! syntax tree.

use super::super::text::Ident;
use super::fn_builder::FunctionDeclarationBuilder;
use super::operators::{InfixOp, PrefixOp};
use super::token::Token;
use super::types::TypeRef;

/// An identifier, with an optional type attached
#[derive(Debug, PartialEq)]
pub struct TypedId {
    /// The Type of this Identifier
    ///
    /// If a type was specified then this contains the type
    /// reference. If empty then the type should be inferred.
    pub typ: Option<TypeRef>,

    /// The Idnetifier Itself
    pub id: Ident,
}

impl TypedId {
    /// Create an Id with a Known Type
    ///
    /// Constructs a new idnetifier declaration where the identifier
    /// definitely has a known type.
    pub fn new(id: Ident, typ: TypeRef) -> Self {
        Self::from_parts(id, Some(typ))
    }

    /// Create an Id without a Known Type
    ///
    /// Constructs a new identifier declaraiton where the identifier
    /// does not have a type specified in the source. This is used
    /// where the type will be infered at a later date.
    pub fn new_without_type(id: Ident) -> Self {
        Self::from_parts(id, None)
    }

    /// Create an Id from Constituent Parts
    ///
    /// Used to construct a new identifier when a type has only
    /// optionally been specified.
    pub fn from_parts(id: Ident, typ: Option<TypeRef>) -> Self {
        TypedId { id, typ }
    }
}

/// Literal / Constant Value
#[derive(Debug, PartialEq)]
pub enum Constant {
    /// A Numeric Value
    Number(i64),
    /// A Boolean Value
    Bool(bool),
    /// A Literal String
    String(String),
}

/// A single identifier token
#[derive(Debug, PartialEq)]
pub struct IdentifierExpression {
    /// The underlying token
    pub token: Box<Token>,
    /// The identifier for the token. This should be the same as the
    /// contents of the `Token::Word`.
    pub ident: Ident,
}

/// A Literal constant vlaue
#[derive(Debug, PartialEq)]
pub struct LiteralExpression {
    /// The underlying token
    pub token: Box<Token>,
    /// The 'cooked' Literal, ready to be used.
    pub value: Constant,
}

/// Prefix Expression
///
/// Holds the contents of the prefix expression. This is the operator
/// token and the inner expression.
#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    /// The token for the operator
    pub op_token: Box<Token>,
    /// The operator itself
    pub op: PrefixOp,
    /// The inner Expression
    pub inner: Box<Expression>,
}

/// Infix Operator Expression
///
/// Represents two expressions joined by an inner operator. This does
/// not distignuish between assignment and other operators.
#[derive(Debug, PartialEq)]
pub struct InfixOperatorExpression {
    /// The left hand side expression
    pub left: Box<Expression>,
    /// The token for the operator
    pub op_token: Box<Token>,
    /// The operator itself
    pub op: InfixOp,
    /// The right hand side expression
    pub right: Box<Expression>,
}

/// Call Expression
///
/// Represnets the application of the call operator `()` to an
/// expression. The arguments to the function are groupeed together in
/// a `,` delimited list.
#[derive(Debug, PartialEq)]
pub struct CallExpression {
    /// The item this funcion call should target
    pub callee: Box<Expression>,
    /// The opening `(` of this call
    pub open_paren: Box<Token>,
    /// The list of arguments to the call. This could be empty.
    pub arguments: Vec<Expression>,
    /// THe closing `)` of this call
    pub close_paren: Box<Token>,
}

/// Array Indexing
///
/// Index expressions represent accessing one or more values from an
/// aggregate. The plan is to allow slicing by passing a rage to the
/// index operator.
#[derive(Debug, PartialEq)]
pub struct IndexExpression {
    /// The expression being indexed into
    pub indexee: Box<Expression>,
    /// The opening `[` of the index expression
    pub open_bracket: Box<Token>,
    /// The index being accessed
    pub index: Box<Expression>,
    /// The closing `]` of the expression
    pub close_bracket: Box<Token>,
}

/// Loop Expression
///
/// Represents a loop operator. Loops always evaluate to `()` but can
/// run the body of the loop more than once.
#[derive(Debug, PartialEq)]
pub struct LoopExpression {
    /// The word used to introduce the loop
    pub kw_token: Box<Token>,
    /// The loop header expression
    pub condition: Box<Expression>,
    /// The loop body
    pub body: Box<Expression>,
}

/// Print Expressin
///
/// The appliation of the prefix `print` operator.
#[derive(Debug, PartialEq)]
pub struct PrintExpression {
    /// The `print` token
    pub print_tok: Box<Token>,
    /// The expression to be printed
    pub inner: Box<Expression>,
}

/// Represents an AST expression.
///
/// Each variant represnets a unique kind of expression. The data for
/// that expresison is carried in a data `struct`.
#[derive(Debug, PartialEq)]
pub enum Expression {
    /// A reference to a variable or function parameter
    Identifier(IdentifierExpression),
    /// A hardcoded value in the program, such as a number or string
    /// literal.
    Literal(LiteralExpression),
    /// The application of a prefix operator to a value.
    Prefix(PrefixExpression),
    /// The application of an infix operator
    Infix(InfixOperatorExpression),
    /// Function call
    Call(CallExpression),
    /// Array indexing
    Index(IndexExpression),
    #[allow(missing_docs)]
    IfThenElse(Box<Expression>, Box<Expression>, Box<Expression>),
    #[allow(missing_docs)]
    Function(Ident, TypeRef, Vec<TypedId>, Box<Expression>),
    /// Conditional Loop
    Loop(LoopExpression),
    #[allow(missing_docs)]
    Sequence(Vec<Expression>),
    /// Print Expression
    Print(PrintExpression),
    #[allow(missing_docs)]
    Declaration(TypedId, bool, Box<Expression>),
    /// Expression grouped with paranthesis
    Grouping(Box<Token>, Box<Expression>, Box<Token>),
}

impl Expression {
    /// New Identifier Expression
    ///
    /// A reference to an identifier, either as a variable reference
    /// or declaration, part of a function definition or function
    /// call.
    pub fn identifier(token: Token, ident: Ident) -> Self {
        Expression::Identifier(IdentifierExpression {
            token: Box::new(token),
            ident,
        })
    }

    /// New Numeric Constant
    ///
    /// A constant numeric value, either specified inline using a
    /// numeric literal or computed from other known compile-time
    /// constants.
    pub fn constant_num(token: Token, n: i64) -> Self {
        Expression::Literal(LiteralExpression {
            token: Box::new(token),
            value: Constant::Number(n),
        })
    }

    /// New String Constant
    ///
    /// A constant string value, either specified inline using a
    /// string literal or computed from other known compile-time
    /// constants.
    pub fn constant_string<T>(token: Token, s: T) -> Self
    where
        T: Into<String>,
    {
        Expression::Literal(LiteralExpression {
            token: Box::new(token),
            value: Constant::String(s.into()),
        })
    }

    /// New Bool Constant
    ///
    /// A constant boolean value. Created from the literal 'true' or
    /// 'false'.
    pub fn constant_bool(token: Token, b: bool) -> Self {
        Expression::Literal(LiteralExpression {
            token: Box::new(token),
            value: Constant::Bool(b),
        })
    }

    /// New Prefix Operator Expression
    ///
    /// Represents the application of a prefix unary operator to
    /// another expression.
    pub fn prefix(op_token: Token, op: PrefixOp, expr: Expression) -> Self {
        Expression::Prefix(PrefixExpression {
            op_token: Box::new(op_token),
            op,
            inner: Box::new(expr),
        })
    }

    /// New Infix Operator Expression
    ///
    /// Represents the application of an infix binary operator to two
    /// expression operands.
    pub fn infix(lhs: Expression, op_token: Token, op: InfixOp, rhs: Expression) -> Self {
        Expression::Infix(InfixOperatorExpression {
            left: Box::new(lhs),
            op_token: Box::new(op_token),
            op,
            right: Box::new(rhs),
        })
    }

    /// New Function Call Expression
    ///
    /// Represents calling a given function with a numer of arguments.
    pub fn call(
        callee: Expression,
        open_paren: Token,
        args: Vec<Expression>,
        close_paren: Token,
    ) -> Self {
        Expression::Call(CallExpression {
            callee: Box::new(callee),
            open_paren: Box::new(open_paren),
            arguments: args,
            close_paren: Box::new(close_paren),
        })
    }

    /// New Index Expression
    ///
    /// Represents indexing one expression by another. This could be
    /// an array lookup, or slice operation.
    pub fn index(lhs: Expression, open: Token, index: Expression, close: Token) -> Self {
        Expression::Index(IndexExpression {
            indexee: Box::new(lhs),
            open_bracket: Box::new(open),
            index: Box::new(index),
            close_bracket: Box::new(close),
        })
    }

    /// New If Then Else Expression
    ///
    /// Represents either a single conditional expression, or a
    /// ternary expression.
    pub fn if_then_else(iff: Expression, then: Expression, els: Expression) -> Self {
        Expression::IfThenElse(Box::new(iff), Box::new(then), Box::new(els))
    }

    /// New Function Definition
    ///
    /// Create a function delcaration builder. This can be used to
    /// create a function expression.
    pub fn function(id: Ident) -> FunctionDeclarationBuilder {
        FunctionDeclarationBuilder::new(id)
    }

    /// New Loop Expression
    ///
    /// Represents the repeated evaluation of an expression until a
    /// condition changes.
    pub fn loop_while(kw_token: Token, condition: Expression, body: Vec<Expression>) -> Self {
        Expression::Loop(LoopExpression {
            kw_token: Box::new(kw_token),
            condition: Box::new(condition),
            body: Box::new(Expression::sequence(body)),
        })
    }

    /// New Variable Declaration
    ///
    /// Represents the declaration of a local variable.
    pub fn declaration(var: TypedId, is_mut: bool, expr: Expression) -> Self {
        Expression::Declaration(var, is_mut, Box::new(expr))
    }

    /// New Sequence Expression
    ///
    /// Represents a sequence of expressions evaluated one after the
    /// other.
    pub fn sequence(exprs: Vec<Expression>) -> Self {
        Expression::Sequence(exprs)
    }

    /// Print Expression
    ///
    /// Evaluates an inner expression, prints it to standard output,
    /// and then returns the inner expression's value.
    pub fn print(print: Token, expr: Expression) -> Self {
        Expression::Print(PrintExpression {
            print_tok: Box::new(print),
            inner: Box::new(expr),
        })
    }

    /// Grouping Expression
    ///
    /// Represents an expression wrapped in `(` and `)`.
    pub fn grouping(left: Token, inner: Expression, right: Token) -> Self {
        Expression::Grouping(Box::new(left), Box::new(inner), Box::new(right))
    }
}
