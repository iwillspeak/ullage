//! Low Loader
//!
//! Low-level bindings to LLVM. This module provides a set of safe
//! wrappers around LLVM objects and types to allow them to be used
//! more ergonomically from Rust code.
//!
//! # Key Types
//!
//! Any use of low_loader requires creating a [`Context`] first. This
//! type controls access to [`Module`]s and [`Type`]s. Each Module
//! represents a single 'compilation unit' which maps down to a single
//! output object when compiled.
//!
//! After creating a module LLVM IR can be added using a [`Builder`]
//! object. The builder has a set of factory methods for appending IR
//! to a given basic block.
//!
//! [`Context`]: ./context/struct.Context.html
//! [`Module`]: ./module/struct.Module.html
//! [`Type`]: ./types/enum.Type.html
//! [`Builder`]: ./builder/struct.Builder.html

#![deny(missing_docs)]

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
