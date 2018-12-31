//! Pass Manager
//!
//! This module defines a safe wrapper around the `PassManager` and
//! `PassManagerBuilder` LLVM C APIs.
//!
//! Pass managers are used to run a given set of transforms against
//! either a module or function.

use super::module::Module;

use super::llvm_sys::core;
use super::llvm_sys::prelude::*;
use super::llvm_sys::transforms::pass_manager_builder as pm_builder;

/// LLVM Module PassManager
pub struct ModulePassManager(LLVMPassManagerRef);

/// Builder API for the PassManager
pub struct PassManagerBuilder(pm_builder::LLVMPassManagerBuilderRef);

/// Optimisation Levels
///
/// Used to control the level of optimisaton that the pass manager
/// builder should target when adding passes
pub enum OptLevel {
    /// No Speed Optimisation
    Off,
    /// Some Optimisation for Speed
    ///
    /// This corresponds to the `-O1` optimisation flag.
    Low,
    /// Medium Optimisation for Speed
    ///
    /// This corresponds to the `-O2` optimisation flag.
    Medium,
    /// Full Optimisation for Speed
    ///
    /// This corresponds to the `-O1` optimisation flag.
    High,
}

/// Size Optimisation Levels
///
/// Used to control the code size that the optimisation passes
/// target. This can be used to encourage the optimiser to favour
/// smaller binaries rather than faster ones.
pub enum OptSize {
    /// Size Optimisation Disabled
    Off,
    /// Normal Size Optimisations
    ///
    /// This corresponds to the `-Os` optimisation flag.
    Size,
    /// Full Size Optimisations
    ///
    /// This corresponds to the `-Oz` optimisation flag.
    SizeFull,
}

impl From<OptLevel> for ::libc::c_uint {
    fn from(level: OptLevel) -> Self {
        use self::OptLevel::*;
        match level {
            Off => 0,
            Low => 1,
            Medium => 2,
            High => 3,
        }
    }
}

impl From<OptSize> for ::libc::c_uint {
    fn from(size: OptSize) -> Self {
        use self::OptSize::*;
        match size {
            Off => 0,
            Size => 1,
            SizeFull => 2,
        }
    }
}

impl Drop for PassManagerBuilder {
    fn drop(&mut self) {
        unsafe {
            pm_builder::LLVMPassManagerBuilderDispose(self.0);
        }
    }
}

impl Default for PassManagerBuilder {
    fn default() -> Self {
        PassManagerBuilder::new()
    }
}

impl PassManagerBuilder {
    /// Create a Pass Manager Builder
    pub fn new() -> Self {
        let raw = unsafe { pm_builder::LLVMPassManagerBuilderCreate() };
        PassManagerBuilder(raw)
    }

    /// Set the Optimisation Level
    ///
    /// Controls the optimisation level the pass manager bulder will
    /// target.
    pub fn with_opt_level(self, level: OptLevel) -> Self {
        unsafe {
            pm_builder::LLVMPassManagerBuilderSetOptLevel(self.0, level.into());
        }
        self
    }

    /// Set the Size Optimisation Level
    ///
    /// Controls the optimiser's preference for smaller code size over
    /// speed. Traditionally setting this to anything other than `Off`
    /// is paired with setting the optimisation level to `Med`.
    pub fn with_opt_size(self, size: OptSize) -> Self {
        unsafe {
            pm_builder::LLVMPassManagerBuilderSetSizeLevel(self.0, size.into());
        }
        self
    }

    /// Create a Moduel Pass Manager
    ///
    /// Takes the current state of the pass manager builder and
    /// creates a new module pass manager populated with the
    /// configured passes.
    pub fn create_module_pass_manager(&self) -> ModulePassManager {
        let pm = unsafe { core::LLVMCreatePassManager() };
        unsafe {
            pm_builder::LLVMPassManagerBuilderPopulateModulePassManager(self.0, pm);
        }
        ModulePassManager(pm)
    }
}

impl ModulePassManager {
    /// Run the Pass Manager
    ///
    /// Attempt to transform the given module with the pass
    /// manager. Returns a boolean representing if the pass manager
    /// made changes to the module.
    pub fn run(&self, module: &mut Module) -> bool {
        unsafe { core::LLVMRunPassManager(self.0, module.as_raw()) != 0 }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_build_module_pass_manager() {
        let _pm = PassManagerBuilder::new().create_module_pass_manager();
    }

    #[test]
    fn test_set_optimisation_settings() {
        let _pm = PassManagerBuilder::new()
            .with_opt_level(OptLevel::Medium)
            .with_opt_size(OptSize::SizeFull)
            .create_module_pass_manager();
    }
}
