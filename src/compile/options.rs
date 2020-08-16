//! Compilation Options
//!
//! This module defines the options structure used to tweak
//! compilation output.

use crate::low_loader::pass_manager as pm;
use super::linker::Linker;

/// Compilation Options
///
/// This is used to control how each `Compilation` instance behaves.
#[derive(Default)]
pub struct CompilationOptions {
    /// Dump the LLVM IR when the module is compiled
    pub dump_ir: bool,
    /// Optimisation level to use when emitting code
    pub opt_level: OptimisationLevel,
    /// Linker option
    pub linker: Option<Linker>,
}

/// Optimisation levels
///
/// Defines the different levels of optimisation that the compiler
/// supports. These levels are usually controlled from the command
/// line.
pub enum OptimisationLevel {
    /// No optimisation
    Off,
    /// Low optimisation, same as -O1
    Low,
    /// Medium optimisation. Same as -O2
    Med,
    /// High optimisation. Same as -O3
    High,
    /// Optimise for size not speed
    Size,
}

impl CompilationOptions {
    /// Se the `dump_ir` flag
    ///
    /// Enables or disables dumping the LLVM IR when modules are
    /// compiled.
    pub fn with_dump_ir(self, dump_ir: bool) -> Self {
        CompilationOptions { dump_ir, ..self }
    }

    /// Set the Optimisation Level
    ///
    /// Controls the optimisation level for the given options.
    pub fn with_opt_level(self, opt_level: OptimisationLevel) -> Self {
        CompilationOptions { opt_level, ..self }
    }

    /// Set the linker command to use
    pub fn with_linker(self, linker: Linker) -> Self {
        CompilationOptions { linker: Some(linker), ..self }
    }
}

impl Default for OptimisationLevel {
    fn default() -> Self {
        OptimisationLevel::Off
    }
}

impl OptimisationLevel {
    /// Unpack an Optimistaion Level
    ///
    /// Retrieves a (level, size) tuple which defines how to configure
    /// the LLVM optimiser for this optimisation level.
    pub fn unpack(&self) -> Option<(pm::OptLevel, pm::OptSize)> {
        use crate::OptimisationLevel::*;
        match self {
            Off => None,
            Low => Some((pm::OptLevel::Low, pm::OptSize::Off)),
            Med => Some((pm::OptLevel::Medium, pm::OptSize::Off)),
            High => Some((pm::OptLevel::High, pm::OptSize::Off)),
            Size => Some((pm::OptLevel::Medium, pm::OptSize::Size)),
        }
    }
}
