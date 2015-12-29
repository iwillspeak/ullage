/// Operator
///
/// Represents an operation that can be applied to an expression
/// either as a prefix operation or as an infix binary operation.
#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

/// Expression
///
/// A single Expression node. Represents either a value or complex
/// expression.
#[derive(Debug)]
pub enum Expression<'a> {
    /// Value Expression
    ///
    /// Represents a simple numeric value
    ValueExpression(i32),

    /// Variable Expression
    ///
    /// Represents a reference to a variable
    VariableExpression(String),

    /// Prefix Operator Expression
    ///
    /// Represents a unary prefix operator
    PrefixOperatorExpression {
        operator: Operator,
        expression: &'a Expression<'a>,
    },

    /// Binary Operator Expression
    ///
    /// Represents a binary operator.
    BinaryOperatorExpression {
        lhs: &'a Expression<'a>,
        operator: Operator,
        rhs: &'a Expression<'a>,
    },
}

#[cfg(test)]
mod test {

    use super::Expression::*;
    use super::Operator;

    #[test]
    pub fn test_create_value_expression() {
        let expr = ValueExpression(32);
        match expr {
            ValueExpression(value) => assert_eq!(value, 32),
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_create_variable_expression() {
        let expr = VariableExpression("hello".to_string());
        match expr {
            VariableExpression(name) => assert_eq!(name, "hello".to_string()),
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_create_prefix_operation_expression() {
        let value_expr = ValueExpression(64);
        let expr = PrefixOperatorExpression {
            operator: Operator::Add,
            expression: &value_expr,
        };
        match expr {
            PrefixOperatorExpression{ operator, expression } => {
                assert_eq!(Operator::Add, operator);
                if let &ValueExpression(value) = expression {
                    assert_eq!(64, value)
                } else {
                    assert!(false);
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_create_binary_operator_expression() {
        let lhs_expr = ValueExpression(111);
        let rhs_expr = ValueExpression(222);
        let expr = BinaryOperatorExpression {
            lhs: &lhs_expr,
            operator: Operator::Add,
            rhs: &rhs_expr,
        };
        match expr {
            BinaryOperatorExpression { lhs, operator, rhs } => {
                match lhs {
                    &ValueExpression(value) => assert_eq!(value, 111),
                    _ => assert!(false),
                };
                match rhs {
                    &ValueExpression(value) => assert_eq!(value, 222),
                    _ => assert!(false),
                };
                assert_eq!(operator, Operator::Add)
            }
            _ => assert!(false),
        }
    }

}
