use std::fmt::{Display, Formatter, Error};

/// Operator
///
/// Represents an operation that can be applied to an expression
/// either as a prefix operation or as an infix binary operation.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
}

/// Expression
///
/// A single Expression node. Represents either a value or complex
/// expression.
pub enum Expression {
    /// Value Expression
    ///
    /// Represents a simple numeric value
    ValueExpression(i64),

    /// Variable Expression
    ///
    /// Represents a reference to a variable
    VariableExpression(String),

    /// Prefix Operator Expression
    ///
    /// Represents a unary prefix operator
    PrefixOperatorExpression {
        operator: Operator,
        expression: Box<Expression>,
    },

    /// Binary Operator Expression
    ///
    /// Represents a binary operator.
    BinaryOperatorExpression {
        lhs: Box<Expression>,
        operator: Operator,
        rhs: Box<Expression>,
    },

    /// Function Call Expression
    FnCall {
        callee_expr: Box<Expression>,
        params: Vec<Expression>
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use super::expression::Expression::*;
        match self {
            &ValueExpression(ref val) => write!(f, "{}", val),
            &VariableExpression(ref var) => write!(f, "{}", var),
            &PrefixOperatorExpression{ref operator, ref expression} => {
                write!(f, "({:?} {})", operator, *expression)
            }
            &BinaryOperatorExpression{ref lhs, ref operator, ref rhs} => {
                write!(f, "({} {:?} {})", *lhs, operator, *rhs)
            }
            &FnCall{ref callee_expr, ref params} => {
                try!(write!(f, "{}.call(", *callee_expr));
                for param in params {
                    try!(write!(f, "{}", param));
                }
                write!(f, ")")
            }
        }
    }
}

impl Expression {
    pub fn from_value(val: i64) -> Self {
        Expression::ValueExpression(val)
    }

    pub fn from_ident(identifier: &str) -> Self {
        Expression::VariableExpression(identifier.to_string())
    }

    pub fn from_prefix_op(op: Operator, expression: Expression) -> Expression {
        Expression::PrefixOperatorExpression {
            operator: op,
            expression: Box::new(expression),
        }
    }

    pub fn from_binary_op(lhs: Expression, op: Operator, rhs: Expression) -> Expression {
        Expression::BinaryOperatorExpression {
            lhs: Box::new(lhs),
            operator: op,
            rhs: Box::new(rhs),
        }
    }

    pub fn from_function_call(calee: Expression, params: Vec<Expression>) -> Self {
        Expression::FnCall {
            callee_expr: Box::new(calee),
            params: params,
        }
    }
}

#[cfg(test)]
mod test {

    use super::{Expression, Operator};
    use super::Expression::*;

    #[test]
    pub fn test_create_value_expression() {
        let expr = Expression::from_value(32);
        match expr {
            ValueExpression(value) => assert_eq!(value, 32),
            _ => panic!(),
        }
    }

    #[test]
    pub fn test_create_variable_expression() {
        let expr = Expression::from_ident("hello");
        match expr {
            VariableExpression(name) => assert_eq!(name, "hello".to_string()),
            _ => panic!(),
        }
    }

    #[test]
    pub fn test_create_prefix_operation_expression() {
        let value_expr = Expression::from_value(64);
        let expr = Expression::from_prefix_op(Operator::Add, value_expr);
        match expr {
            PrefixOperatorExpression{ operator, expression } => {
                assert_eq!(Operator::Add, operator);
                if let ValueExpression(value) = *expression {
                    assert_eq!(64, value)
                } else {
                    panic!();
                }
            }
            _ => panic!(),
        }
    }

    #[test]
    pub fn test_create_binary_operator_expression() {
        let lhs_expr = Expression::from_value(111);
        let rhs_expr = Expression::from_value(222);
        let expr = Expression::from_binary_op(lhs_expr, Operator::Add, rhs_expr);

        match expr {
            BinaryOperatorExpression { lhs, operator, rhs } => {
                match *lhs {
                    ValueExpression(value) => assert_eq!(value, 111),
                    _ => panic!(),
                };
                match *rhs {
                    ValueExpression(value) => assert_eq!(value, 222),
                    _ => panic!(),
                };
                assert_eq!(operator, Operator::Add)
            }
            _ => panic!(),
        }
    }

}
