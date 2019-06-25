//! Semantic Analysis and Translation
//!
//! This module is responsible for translating the syntactic
//! representation of a program, as produced by the parser, into a
//! semantically rich model ready to be lowered for execution.

mod operators;
mod sem_ctx;
mod transform;
mod tree;
mod types;
mod binder;

pub use self::sem_ctx::SemCtx;
pub use self::transform::transform_expression;
pub use self::tree::{Expression, ExpressionKind};
pub use self::types::{BuiltinType, Typ};
pub use self::binder::{Binder, Scope, Symbol};
