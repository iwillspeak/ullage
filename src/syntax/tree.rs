//! Syntax Tree
//!
//! This module defines the types which make up the parsed syntax
//! tree. This tree defines the full strcuture of a parsed source file
//! before any semantic transformation is done.

pub mod types;
pub mod operators;
pub mod expression;
mod fn_builder;
