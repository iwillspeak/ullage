//! # Semantic Operators
//!
//! This module provides semantic undestanding of the builtin
//! operators. The main entry point is the

use super::types::{BuiltinType, Typ};
use crate::syntax::*;

/// The Semantic Operator
///
/// Semantically bound operator. This is an operator with knowlege of
/// the types it is to be bound to.
pub struct SemOp {
    pub lhs_typ: Typ,
    pub rhs_typ: Typ,
    pub op: InfixOp,
    pub result_typ: Typ,
}

fn num_op(op: InfixOp) -> Option<SemOp> {
    Some(SemOp {
        lhs_typ: Typ::Builtin(BuiltinType::Number),
        rhs_typ: Typ::Builtin(BuiltinType::Number),
        op,
        result_typ: Typ::Builtin(BuiltinType::Number),
    })
}

fn comp_op(op: InfixOp) -> Option<SemOp> {
    Some(SemOp {
        lhs_typ: Typ::Builtin(BuiltinType::Number),
        rhs_typ: Typ::Builtin(BuiltinType::Number),
        op,
        result_typ: Typ::Builtin(BuiltinType::Bool),
    })
}

/// Find Operator
///
/// Searches for the result type for a given operator.
pub fn find_builtin_op(op: InfixOp, lhs_typ: Typ, rhs_typ: Typ) -> Option<SemOp> {
    match (op, lhs_typ, rhs_typ) {
        (InfixOp::Add, Typ::Builtin(BuiltinType::String), Typ::Builtin(BuiltinType::String)) => {
            Some(SemOp {
                lhs_typ,
                rhs_typ,
                op,
                result_typ: Typ::Builtin(BuiltinType::String),
            })
        }
        (InfixOp::Add, Typ::Builtin(BuiltinType::Number), Typ::Builtin(BuiltinType::Number)) => {
            num_op(op)
        }
        (InfixOp::Sub, _, _) | (InfixOp::Mul, _, _) | (InfixOp::Div, _, _) => num_op(op),

        (InfixOp::Eq, _, _)
        | (InfixOp::NotEq, _, _)
        | (InfixOp::Lt, _, _)
        | (InfixOp::Gt, _, _) => comp_op(op),

        _ => None,
    }
}
