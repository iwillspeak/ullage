use std::collections::HashMap;

use syntax::{Expression, Constant, TypeRef, TypedId};
use syntax::operators::{PrefixOp, InfixOp};
use syntax::visit::Visitor;

use super::{Evaluator, Value};

/// Ullage Expression Evaluation Context
///
/// Holds all of the state require to evaluate expressions.
pub struct TreeWalkEvaluator {
    sym_tab: HashMap<String, Value>,
}

impl Evaluator for TreeWalkEvaluator {
    /// Evaluate an Expression
    fn eval(&mut self, expr: Expression) -> Value {
        self.visit(expr)
    }
}

impl TreeWalkEvaluator {
    /// Construct an TreeWalkEvaluator
    pub fn new() -> Self {
        TreeWalkEvaluator { sym_tab: HashMap::new() }
    }

    /// Evaluate an infix operator on two numbers
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


impl Visitor for TreeWalkEvaluator {
    type Output = Value;

    fn on_identifier(&mut self, id: String) -> Self::Output {
        self.sym_tab.get(&id).unwrap().clone()
    }

    fn on_literal(&mut self, lit: Constant) -> Self::Output {
        lit.into()
    }

    fn on_prefix(&mut self, op: PrefixOp, value: Expression) -> Self::Output {
        let value = self.visit(value);
        match op {
            PrefixOp::Negate => {
                match value {
                    Value::Num(i) => Value::Num(-i),
                    _ => panic!("type mismatch"),
                }
            }
            PrefixOp::Not => {
                match value {
                    Value::Num(i) if i != 0 => Value::Num(0),
                    Value::Num(i) if i == 0 => Value::Num(1),
                    _ => panic!("type mismatch"),
                }
            }
        }
    }

    fn on_infix(&mut self, lhs: Expression, op: InfixOp, rhs: Expression) -> Self::Output {
        let rhs_val = self.visit(rhs);

        if op == InfixOp::Assign {
            let id = match lhs {
                Expression::Identifier(id) => id,
                _ => panic!("assign to something which isn't an id"),
            };
            self.sym_tab.insert(id, rhs_val.clone());
            return rhs_val;
        }

        let lhs_val = self.visit(lhs);

        match (lhs_val, rhs_val) {
            (Value::Num(i), Value::Num(j)) => self.eval_infix_num(i, op, j),
            _ => panic!("TODO"),
        }
    }

    fn on_call(&mut self, _calee: Expression, _args: Vec<Expression>) -> Self::Output {
        unimplemented!();
    }

    fn on_index(&mut self, _target: Expression, _index: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_if(&mut self, _cond: Expression, _then: Expression, _els: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_function(&mut self,
                   _id: String,
                   _ty: TypeRef,
                   _args: Vec<TypedId>,
                   _body: Expression)
                   -> Self::Output {
        unimplemented!();
    }

    fn on_loop(&mut self, _cond: Expression, _body: Expression) -> Self::Output {
        unimplemented!();
    }

    fn on_variable(&mut self, var: TypedId) -> Self::Output {
        self.sym_tab.insert(var.id, Value::Num(0));
        Value::Num(0)
    }

    fn on_sequence(&mut self, mut exprs: Vec<Expression>) -> Self::Output {
        exprs.reverse();
        let mut value = Value::Num(0);
        while let Some(e) = exprs.pop() {
            value = self.visit(e);
        }
        value
    }
}

#[cfg(test)]
mod test {

    use syntax::*;
    use syntax::operators::*;

    use super::*;
    use super::super::*;

    #[test]
    fn create_evaluator() {
        let _ = TreeWalkEvaluator::new();
    }

    #[test]
    fn evaluate_constant_expression_returns_value() {
        let mut e = TreeWalkEvaluator::new();
        let expr = Expression::constant_num(1337);
        assert_eq!(Value::Num(1337), e.eval(expr));
    }

    #[test]
    fn evaluate_prefix_expression() {
        let mut e = TreeWalkEvaluator::new();
        let expr = Expression::prefix(PrefixOp::Negate, Expression::constant_num(1234));
        assert_eq!(Value::Num(-1234), e.eval(expr));
        let expr1 = Expression::prefix(PrefixOp::Not,
                                       Expression::prefix(PrefixOp::Negate,
                                                          Expression::constant_num(100)));
        assert_eq!(Value::Num(0), e.eval(expr1));
    }

    #[test]
    fn evaluate_infix_operator() {
        let mut e = TreeWalkEvaluator::new();
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
