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
    /// Optimisation level to use when emitting code
    pub opt_level: OptimisationLevel,
}

/// Optimisation levels
///
/// Defines the different levels of optimisation that the compiler
/// supports. These levels are usually controlled from the command
/// line.
// FIXME: this deserialisation stuff seems quite tightly coupled to
// the command line interface.
#[derive(Debug, Deserialize)]
pub enum OptimisationLevel {
    /// No optimisation
    #[serde(rename = "0")]
    Off,
    /// Low optimisation, same as -O1
    #[serde(rename = "1")]
    Low,
    /// Medium optimisation. Same as -O2
    #[serde(rename = "2")]
    Med,
    /// High optimisation. Same as -O3
    #[serde(rename = "3")]
    High,
    /// Optimise for size not speed
    #[serde(rename = "s")]
    Size,
}

impl Default for CompilationOptions {
    fn default() -> Self {
        CompilationOptions {
            dump_ir: false,
            opt_level: OptimisationLevel::default(),
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

    /// Set the Optimisation Level
    ///
    /// Controls the optimisation level for the given options.
    pub fn with_opt_level(self, opt_level: OptimisationLevel) -> Self {
        CompilationOptions {
            opt_level: opt_level,
            ..self
        }
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
    pub fn unpack(&self) -> Option<(u32, bool)> {
        use OptimisationLevel::*;
        match self {
            Off => None,
            Low => Some((1, false)),
            Med => Some((2, false)),
            High => Some((3, false)),
            Size => Some((2, true)),
        }
    }
}
