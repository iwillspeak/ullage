//! Syntax Expression
//!
//! A syntax expression represents the value of a given node in the
//! syntax tree.

use super::super::text::Ident;
use super::operators::{InfixOp, PrefixOp};
use super::token::{Token, TokenKind};
use super::types::TypeAnno;

/// An identifier, with an optional type attached
#[derive(Debug, PartialEq)]
pub struct TypedId {
    /// The Type of this Identifier
    ///
    /// If a type was specified then this contains the type
    /// reference. If empty then the type should be inferred.
    pub typ: Option<TypeAnno>,

    /// The Idnetifier Itself
    pub id: Ident,

    /// The token for the identifier itself
    pub id_tok: Token,
}

impl TypedId {
    /// Create an Id with a Known Type
    ///
    /// Constructs a new idnetifier declaration where the identifier
    /// definitely has a known type.
    pub fn new(id: Token, typ: TypeAnno) -> Self {
        Self::from_parts(id, Some(typ))
    }

    /// Create an Id without a Known Type
    ///
    /// Constructs a new identifier declaraiton where the identifier
    /// does not have a type specified in the source. This is used
    /// where the type will be infered at a later date.
    pub fn new_without_type(id_tok: Token) -> Self {
        Self::from_parts(id_tok, None)
    }

    /// Create an Id from Constituent Parts
    ///
    /// Used to construct a new identifier when a type has only
    /// optionally been specified.
    pub fn from_parts(id_tok: Token, typ: Option<TypeAnno>) -> Self {
        if let TokenKind::Word(id) = id_tok.kind {
            TypedId { typ, id, id_tok }
        } else {
            panic!("Creating a `TypedId` requires an `Word` token")
        }
    }
}

/// Delimited Item
///
/// A single element in a list of token-delimited values.
#[derive(Debug, PartialEq)]
pub enum DelimItem<T> {
    /// The first item in a list. Doesn't have a corresponding
    /// dlimiter token.
    First(T),
    /// The follwing items in the list. Carries the token which
    /// separated it from the previous element.
    Follow(Token, T),
}

impl<T> DelimItem<T> {
    /// Borrow in inner item
    pub fn as_inner(&self) -> &T {
        match *self {
            DelimItem::First(ref t) => t,
            DelimItem::Follow(_, ref t) => t,
        }
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

/// If Else Expression
///
/// The base conditional expression.
#[derive(Debug, PartialEq)]
pub struct IfElseExpression {
    /// The `if` token`
    pub if_tok: Box<Token>,
    /// The condition for the if block
    pub cond: Box<Expression>,
    /// The expression to evaluate if the condition is true
    pub if_true: Box<Expression>,
    /// The `else` token
    pub else_tok: Box<Token>,
    /// The expression to evaluate if the condition is false
    pub if_false: Box<Expression>,
}

/// Function Declaration Expression
///
/// Represents the definition of a function and the implementation of
/// it.
#[derive(Debug, PartialEq)]
pub struct FunctionExpression {
    /// The function's identifier
    pub fn_kw: Box<Token>,
    /// The function's identifier
    pub identifier: Ident,
    /// The open `(` before the parameter list
    pub params_open: Box<Token>,
    /// Function parameters
    pub params: Vec<DelimItem<TypedId>>,
    /// The closing `)` after the parameter list
    pub params_close: Box<Token>,
    /// Function return type
    pub return_type: TypeAnno,
    /// Body of the function
    pub body: BlockBody,
}

/// Block Body
///
/// represents the sequence of expressions within a given block, along
/// with the closing delimiter of the block
#[derive(Debug, PartialEq)]
pub struct BlockBody {
    /// The inner expressions
    pub contents: Box<Expression>,
    /// The closing delimiter
    pub close: Box<Token>,
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
    pub body: BlockBody,
}

/// Print Expression
///
/// The appliation of the prefix `print` operator.
#[derive(Debug, PartialEq)]
pub struct PrintExpression {
    /// The `print` token
    pub print_tok: Box<Token>,
    /// The expression to be printed
    pub inner: Box<Expression>,
}

/// Variable mutability style
#[derive(Debug, PartialEq)]
pub enum VarStyle {
    /// No modifications can be made
    Immutable,
    /// The variable can be re-assigned later
    Mutable,
}

/// Declaration Expression
///
/// Variable declaration. Holds the identifier the declaration
/// introduces and the initial value of the expression.
#[derive(Debug, PartialEq)]
pub struct DeclarationExpression {
    /// The keyword token which introduces this declaration
    pub var_kw: Box<Token>,
    /// is the variable mutable
    pub style: VarStyle,
    /// The identifier to introduce
    pub id: TypedId,
    /// The assignment token
    pub assignment_tok: Box<Token>,
    /// Initialiser for the variable
    pub initialiser: Box<Expression>,
}

/// Parathesis Grouping Expression
///
/// Represents an inner expression, wrapped in a pair of `()`.
#[derive(Debug, PartialEq)]
pub struct GroupingExpression {
    /// The opening `(`
    pub open_tok: Box<Token>,
    /// The inner expression
    pub inner: Box<Expression>,
    /// The closing `)`
    pub close_tok: Box<Token>,
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
    /// An if expression
    IfThenElse(IfElseExpression),
    /// Function declaration
    Function(FunctionExpression),
    /// Conditional Loop
    Loop(LoopExpression),
    /// Sequence expression. Represents a series of expressions and
    /// evaluates to the last one. If there are no expressions this
    /// evaluates to the unit value `()`.
    Sequence(Vec<Expression>),
    /// Print Expression
    Print(PrintExpression),
    /// Variable delcaration expression
    Declaration(DeclarationExpression),
    /// Expression grouped with paranthesis
    Grouping(GroupingExpression),
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
    pub fn if_then_else(
        if_tok: Token,
        cond: Expression,
        then: Expression,
        else_tok: Token,
        els: Expression,
    ) -> Self {
        Expression::IfThenElse(IfElseExpression {
            if_tok: Box::new(if_tok),
            cond: Box::new(cond),
            if_true: Box::new(then),
            else_tok: Box::new(else_tok),
            if_false: Box::new(els),
        })
    }

    /// New Function Definition
    ///
    /// Create a function delcaration builder. This can be used to
    /// create a function expression.
    pub fn function(
        fn_kw: Token,
        identifier: Ident,
        params_open: Token,
        params: Vec<DelimItem<TypedId>>,
        params_close: Token,
        return_type: TypeAnno,
        body: BlockBody,
    ) -> Expression {
        Expression::Function(FunctionExpression {
            fn_kw: Box::new(fn_kw),
            identifier,
            params_open: Box::new(params_open),
            params,
            params_close: Box::new(params_close),
            return_type,
            body,
        })
    }

    /// New Loop Expression
    ///
    /// Represents the repeated evaluation of an expression until a
    /// condition changes.
    pub fn loop_while(kw_token: Token, condition: Expression, body: BlockBody) -> Self {
        Expression::Loop(LoopExpression {
            kw_token: Box::new(kw_token),
            condition: Box::new(condition),
            body,
        })
    }

    /// New Variable Declaration
    ///
    /// Represents the declaration of a local variable.
    pub fn declaration(
        var_kw: Token,
        var: TypedId,
        style: VarStyle,
        assign_tok: Token,
        expr: Expression,
    ) -> Self {
        Expression::Declaration(DeclarationExpression {
            style,
            var_kw: Box::new(var_kw),
            id: var,
            assignment_tok: Box::new(assign_tok),
            initialiser: Box::new(expr),
        })
    }

    /// New Sequence Expression
    ///
    /// Represents a sequence of expressions evaluated one after the
    /// other.
    pub fn sequence(exprs: Vec<Expression>) -> Self {
        Expression::Sequence(exprs)
    }

    /// Empty Expression
    ///
    /// Empty expressions have no value
    pub fn empty() -> Self {
        Expression::sequence(Vec::new())
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
    pub fn grouping(open: Token, inner: Expression, close: Token) -> Self {
        Expression::Grouping(GroupingExpression {
            open_tok: Box::new(open),
            inner: Box::new(inner),
            close_tok: Box::new(close),
        })
    }
}
