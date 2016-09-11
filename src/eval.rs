//! Expression Tree Evaluation

use std::collections::HashMap;
use super::{Expression, Constant, PrefixOp, InfixOp};

/// Ullage Expression Evaluation Context
///
/// Holds all of the state require to evaluate expressions.
pub struct Evaluator {
    sym_tab: HashMap<String, Value>,
}

impl Evaluator {
    /// Construct an Evaluator
    pub fn new() -> Self {
        Evaluator { sym_tab: HashMap::new() }
    }

    /// Evaluate an Expression
    pub fn eval(&mut self, expr: Expression) -> Value {
        match expr {
            Expression::Identifier(id) => self.sym_tab.get(&id).unwrap().clone(),
            Expression::Literal(c) => c.into(),
            Expression::Prefix(PrefixOp::Negate, expr) => {
                match self.eval(*expr) {
                    Value::Num(i) => Value::Num(-i),
                    _ => panic!("type mismatch"),
                }
            }
            Expression::Prefix(PrefixOp::Not, expr) => {
                match self.eval(*expr) {
                    Value::Num(i) if i != 0 => Value::Num(0),
                    Value::Num(i) if i == 0 => Value::Num(1),
                    _ => panic!("type mismatch"),
                }
            }
            Expression::Infix(lhs, op, rhs) => self.eval_infix(*lhs, op, *rhs),
            Expression::Variable(typed_id) => {
                self.sym_tab.insert(typed_id.id, Value::Num(0));
                Value::Num(0)
            }
            Expression::Sequence(mut exprs) => {
                exprs.reverse();
                let mut value = Value::Num(0);
                while let Some(e) = exprs.pop() {
                    value = self.eval(e);
                }
                value
            }
            _ => panic!(format!("Unimplemented: {:?}", expr)),
        }
    }

    fn eval_infix(&mut self, lhs: Expression, op: InfixOp, rhs: Expression) -> Value {
        let rhs_val = self.eval(rhs);

        if op == InfixOp::Assign {
            let id = match lhs {
                Expression::Identifier(id) => id,
                _ => panic!("assing to something which isn't an id"),
            };
            self.sym_tab.insert(id, rhs_val.clone());
            return rhs_val;
        }

        let lhs_val = self.eval(lhs);

        match (lhs_val, rhs_val) {
            (Value::Num(i), Value::Num(j)) => self.eval_infix_num(i, op, j),
            _ => panic!("TODO"),
        }
    }

    fn eval_infix_num(&mut self, i: i64, op: InfixOp, j: i64) -> Value {
        match op {
            InfixOp::Add => Value::Num(i + j),
            InfixOp::Sub => Value::Num(i - j),
            InfixOp::Mul => Value::Num(i * j),
            InfixOp::Div => Value::Num(i / j),
            InfixOp::Eq => Value::Num(if i == j { 1 } else { 0 }),
            InfixOp::NotEq => Value::Num(if i != j { 1 } else { 0 }),
            InfixOp::Lt => Value::Num(if i < j { 1 } else { 0 }),
            InfixOp::Gt => Value::Num(if i > j { 1 } else { 0 }),
            InfixOp::Assign => Value::Num(j),
        }
    }
}

#[derive(PartialEq,Debug,Clone)]
pub enum Value {
    Num(i64),
    String(String),
}

impl From<Constant> for Value {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Number(i) => Value::Num(i),
            Constant::String(s) => Value::String(s),
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use super::super::*;

    #[test]
    fn create_evaluator() {
        let _ = Evaluator::new();
    }

    #[test]
    fn evaluate_constant_expression_returns_value() {
        let mut e = Evaluator::new();
        let expr = Expression::constant_num(1337);
        assert_eq!(Value::Num(1337), e.eval(expr));
    }

    #[test]
    fn evaluate_prefix_expression() {
        let mut e = Evaluator::new();
        let expr = Expression::prefix(PrefixOp::Negate, Expression::constant_num(1234));
        assert_eq!(Value::Num(-1234), e.eval(expr));
        let expr1 = Expression::prefix(PrefixOp::Not,
                                       Expression::prefix(PrefixOp::Negate,
                                                          Expression::constant_num(100)));
        assert_eq!(Value::Num(0), e.eval(expr1));
    }

    #[test]
    fn evaluate_infix_operator() {
        let mut e = Evaluator::new();
        let expr_add = Expression::infix(Expression::constant_num(1),
                                         InfixOp::Add,
                                         Expression::constant_num(2));
        assert_eq!(Value::Num(3), e.eval(expr_add));
        let expr_sub = Expression::infix(Expression::constant_num(1),
                                         InfixOp::Sub,
                                         Expression::constant_num(2));
        assert_eq!(Value::Num(-1), e.eval(expr_sub));
        let expr_mul = Expression::infix(Expression::constant_num(1),
                                         InfixOp::Mul,
                                         Expression::constant_num(2));
        assert_eq!(Value::Num(2), e.eval(expr_mul));
        let expr_div = Expression::infix(Expression::constant_num(1),
                                         InfixOp::Div,
                                         Expression::constant_num(2));
        assert_eq!(Value::Num(0), e.eval(expr_div));
        let expr_eq = Expression::infix(Expression::constant_num(1),
                                        InfixOp::Eq,
                                        Expression::constant_num(2));
        assert_eq!(Value::Num(0), e.eval(expr_eq));
        let expr_noteq = Expression::infix(Expression::constant_num(1),
                                           InfixOp::NotEq,
                                           Expression::constant_num(2));
        assert_eq!(Value::Num(1), e.eval(expr_noteq));
        let expr_lt = Expression::infix(Expression::constant_num(1),
                                        InfixOp::Lt,
                                        Expression::constant_num(2));
        assert_eq!(Value::Num(1), e.eval(expr_lt));
        let expr_gt = Expression::infix(Expression::constant_num(1),
                                        InfixOp::Gt,
                                        Expression::constant_num(2));
        assert_eq!(Value::Num(0), e.eval(expr_gt));
    }
}
