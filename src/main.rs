//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.
//!
//! This crate provides a simple abstract syntax tree, and a parser
//! implementation which recognises a simple lanugage using
//! Pratt-style operator precedence parsing.

mod parse;

pub mod meta {
    pub const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
}

/// Represents an AST prefix operator.
#[derive(Debug,PartialEq)]
pub enum PrefixOp {
    Negate,
}

/// Represents an AST infix operator
#[derive(Debug,PartialEq)]
pub enum InfixOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

/// Represents a reference to a type
#[derive(Debug,PartialEq)]
pub struct TypeReference(String);

/// An identifier, with an optional type attached
#[derive(Debug,PartialEq)]
pub struct TypedId {
    typ: Option<TypeReference>,
    id: String,
}

impl TypedId {
    pub fn new(id: String, typ: TypeReference) -> Self {
        Self::from_parts(id, Some(typ))
    }

    pub fn new_without_type(id: String) -> Self {
        Self::from_parts(id, None)
    }

    pub fn from_parts(id: String, typ: Option<TypeReference>) -> Self {
        TypedId { id: id, typ: typ }
    }
}

/// Literal / Constant Value
#[derive(Debug,PartialEq)]
pub enum Constant {
    Number(i64),
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
    Function(String, TypeReference, Vec<TypedId>, Vec<Expression>),
    Loop(Box<Expression>, Vec<Expression>),
    Variable(TypedId),
    Sequence(Vec<Expression>),
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
            typ: TypeReference("()".to_string()),
            args: Vec::new(),
            body: Vec::new(),
        }
    }

    /// # New Loop Expression
    ///
    /// Represents the repeated evaluation of an expression until a
    /// condition changes.
    pub fn loop_while(condition: Expression, body: Vec<Expression>) -> Self {
        Expression::Loop(Box::new(condition), body)
    }

    /// # New Variable Declaration
    ///
    /// Represents the declaration of a local variable.
    pub fn variable(var: TypedId) -> Self {
        Expression::Variable(var)
    }

    /// # New Sequence Expression
    ///
    /// Represents a sequence of expressions evaluated one after the other.
    pub fn sequence(exprs: Vec<Expression>) -> Self {
        Expression::Sequence(exprs)
    }
}

/// # Builder Struct for Function Declarations
pub struct FunctionDeclarationBuilder {
    id: String,
    typ: TypeReference,
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
    pub fn with_return_type(mut self, typ: TypeReference) -> Self {
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
        Expression::Function(builder.id, builder.typ, builder.args, builder.body)
    }
}

#[cfg(not(test))]
fn main() {

    use std::io;
    use std::io::prelude::*;
    use std::process::*;

    fn prompt(c: char) {
        print!("{0}{0}{0} ", c);
        io::stdout().flush().unwrap();
    }

    let mut failures = 0;

    println!("ullage ({})", meta::VERSION.unwrap_or("unknown"));
    prompt('>');

    let quit_expr = vec![Expression::call(Expression::identifier("quit".to_string()), vec![])];

    let mut buffered = String::new();
    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            buffered.push_str(&line);
            buffered.push('\n');
            match Expression::parse_str(&buffered) {
                Ok(ref expr) if expr == &quit_expr => break,
                Ok(parsed) => {
                    buffered.clear();
                    println!("OK > {:?}", parsed);
                    prompt('>');
                }
                Err(parse::Error::Incomplete) => {
                    prompt('.');
                }
                Err(err) => {
                    buffered.clear();
                    failures += 1;
                    println!("Error: {:?} ({})", err, buffered);
                    prompt('>');
                }
            };
        };
    }

    exit(failures);
}
