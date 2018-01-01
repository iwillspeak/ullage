//! Low Loader
//!
//! Low-level bindings to LLVM for building JIT compilers.

extern crate llvm_sys;

pub mod module;
pub mod context;
pub mod function;
pub mod builder;
pub mod types;
pub mod value;

/// Prelude Module
///
/// This module just re-exports useful types to help cut down on using
/// statements.
pub mod prelude {
    pub use super::context::Context;
    pub use super::module::Module;
    pub use super::function::Function;
    pub use super::builder::Builder;
    pub use super::builder::Predicate;
    pub use super::types::Type;
    pub use super::value::Value;

    // FIXME: only expose Value in public interface.
    pub use super::llvm_sys::prelude::LLVMValueRef;
}
