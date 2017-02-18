//! # Meta Module
//!
//! This module contains metadata about the compiler driver. It's
//! basically just used to expose the version number.

/// Version Number
///
/// The version number of the crate (as known by Cargo) as a
/// string. If the exe wasn't built by Cargo then this will be
/// empty.
const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");

/// Retrieve the Version Numer
///
/// If no version number is available then 'unknown' is returned.
pub fn version() -> &'static str {
    VERSION.unwrap_or("unknown")
}
