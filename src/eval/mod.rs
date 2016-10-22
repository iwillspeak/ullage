//! Expression Tree Evaluation

use syntax::{Expression, Constant};

pub mod tree_walk;
pub mod jit;

pub trait Evaluator {
    fn eval(&mut self, expr: Expression) -> Value;
}

/// Value Type
///
/// Represents the output of an evaluation of an expression. For now
/// this is just numbers and strings.
#[derive(PartialEq,Debug,Clone)]
pub enum Value {
    Num(i64),
    String(String),
}

/// Convert from AST Constatnts to Values
///
/// All values in the AST can be converted to `Value` instances.
impl From<Constant> for Value {
    fn from(c: Constant) -> Self {
        match c {
            Constant::Number(i) => Value::Num(i),
            Constant::String(s) => Value::String(s),
        }
    }
}
