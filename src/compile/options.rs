//! Compilation Options
//!
//! This module defines the options structure used to tweak
//! compilation output.

/// Compilation Options
///
/// This is used to control how each `Compilation` instance behaves.
pub struct CompilationOptions {
    /// Dump the LLVM IR when the module is compiled
    pub dump_ir: bool,
}

impl Default for CompilationOptions {
    fn default() -> Self {
        CompilationOptions {
            dump_ir: false,
        }
    }
}

impl CompilationOptions {
    /// Se the `dump_ir` flag
    ///
    /// Enables or disables dumping the LLVM IR when modules are
    /// compiled.
    pub fn with_dump_ir(self, dump_ir: bool) -> Self {
        CompilationOptions {
            dump_ir: dump_ir,
            ..self
        }
    }
}
