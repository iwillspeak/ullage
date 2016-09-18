/// Represents an AST prefix operator.
#[derive(Debug,PartialEq)]
pub enum PrefixOp {
    Negate,
    Not,
}

/// Represents an AST infix operator
#[derive(Debug,PartialEq)]
pub enum InfixOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
}
