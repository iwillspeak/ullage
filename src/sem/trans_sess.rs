//! Transformation Session
//!
//! This module defines the state that is passes while transforming
//! expressions from AST representation to semantic.

/// Trans Session Structure
///
/// Holds the context when trasnforming. This is basically the current
/// type state information along with symbol table.
pub struct TransSess;

impl TransSess {
    /// Create a new Transformation Session
    pub fn new() -> Self {
        TransSess
    }
}
