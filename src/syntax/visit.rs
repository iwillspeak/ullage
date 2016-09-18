//! Expression Visitor

use super::*;
use super::operators::*;

/// Ullage Expression Visitor
///
/// This trait defines a type which can observe each node in an
/// expression tree.
pub trait Visitor {

    /// The output type of this vistor. This can be used to allow
    /// expression visitors to transform the tree as they walk it.
    type Output;
    
    /// Visit Identifier
    ///
    /// Represents a reference to a local or global value. This could
    /// be a variable, constant or funciton.
    fn on_identifier(&mut self, id: String) -> Self::Output;

    /// Visit Literal Value
    ///
    /// Represents a literal value in the program, e.g. a numeric
    /// literal or string literal.
    fn on_literal(&mut self, lit: Constant) -> Self::Output;

    /// Visit Prefix Expression
    ///
    /// Represents the application of a prefix operator to another
    /// expression.
    fn on_prefix(&mut self, op: PrefixOp, value: Expression) -> Self::Output;

    /// Visit Infix Expression
    ///
    /// Represents the application of an infix operator to a pair of
    /// expressions.
    fn on_infix(&mut self, lhs: Expression, op: InfixOp, rhs: Expression) -> Self::Output;

    /// Visit a Function Call
    ///
    /// Represents a function call, with a variable number of
    /// arguments.
    fn on_call(&mut self, calee: Expression, args: Vec<Expression>) -> Self::Output;

    /// Visit an Index Operation
    ///
    /// Represents indexing one expression by another.
    fn on_index(&mut self, target: Expression, index: Expression) -> Self::Output;

    /// Visit an If Expression
    ///
    /// Represents the conditional evaluation of two expressions.
    fn on_if(&mut self, cond: Expression, then: Expression, els: Expression) -> Self::Output;

    /// Visit a Function Declaration
    ///
    /// Represents the introduction of a new funciton into the scope.
    fn on_function(&mut self, id: String, ty: TypeReference, args: Vec<TypedId>, body: Expression) -> Self::Output;

    /// Visit a Loop
    ///
    /// Represents repeatedly evaluating an expression while a
    /// condition is true.
    fn on_loop(&mut self, cond: Expression, body: Expression) -> Self::Output;

    /// Visit a Variable Declaration
    ///
    /// Represents introducing a new varaible.
    fn on_variable(&mut self, var: TypedId) -> Self::Output;

    /// Visit a Sequence
    ///
    /// Represents a series of expressions evaluated one after the
    /// other.
    fn on_sequence(&mut self, mut exprs: Vec<Expression>) -> Self::Output;
}

impl Expression {

    /// Visit this Expression
    pub fn visit<V, O>(self, v: &mut V) -> O
        where V: Visitor<Output=O>
    {
        match self {
            Expression::Identifier(id) => v.on_identifier(id),
            Expression::Literal(c) => v.on_literal(c),
            Expression::Prefix(op, expr) => v.on_prefix(op, *expr),
            Expression::Infix(lhs, op, rhs) => v.on_infix(*lhs, op, *rhs),
            Expression::Call(callee, args) => v.on_call(*callee, args),
            Expression::Index(target, index) => v.on_index(*target, *index),
            Expression::IfThenElse(i, t, e) => v.on_if(*i, *t, *e),
            Expression::Function(name, ret, args, body) => v.on_function(name, ret, args, *body),
            Expression::Loop(cond, expr) => v.on_loop(*cond, *expr),
            Expression::Variable(var) => v.on_variable(var),
            Expression::Sequence(seq) => v.on_sequence(seq),
        }
    }
}
