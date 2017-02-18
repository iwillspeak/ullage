//! Low Loader
//!
//! Low-level bindings to LLVM for building JIT compilers.

extern crate llvm_sys;

pub mod module;
pub mod context;

/// Prelude Module
///
/// This module just re-exports useful types to help cut down on using
/// statements.
pub mod prelude {
    pub use super::context::Context;
    pub use super::module::Module;
}
