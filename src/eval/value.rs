use syntax::{Expression, Constant, TypeRef, TypedId};

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
