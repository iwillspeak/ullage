//! LLVM Module Wrapper
//!
//! Contains types and wrappers for dealing with LLVM Modules.

use super::function::Function;
use super::llvm_sys::prelude::*;
use super::llvm_sys::target_machine;
use super::llvm_sys::{analysis, bit_writer, core};
use super::pass_manager::{OptLevel, OptSize, PassManagerBuilder};
use super::targets::Target;

use std::ffi::{CStr, CString};
use std::path::Path;
use std::ptr;

/// Module
///
/// A module repsents a single code unit. It maps down to a library or
/// executable when compiled by LLVM. This type provides a safe
/// abstraction around the raw `LLVMModule` type.
#[derive(Debug, PartialEq)]
pub struct Module {
    raw: LLVMModuleRef,
}

/// The kind of output file to write
///
/// Used when writing modules to disk.
#[derive(Debug, PartialEq)]
pub enum OutputFileKind {
    /// LLVM IL files
    LLVMIl,
    /// LLVM Bitcode file
    Bitcode,
    /// Native executable object files
    NativeObject,
}

impl Module {
    /// Module from Raw
    ///
    /// Creates a new module from a raw module reference. This takes
    /// ownership of the given module. When the returned Module
    /// instance goes out of scope the module will be disposed.
    ///
    /// *Note*: You shouldn't need to use this directly, instead modules
    /// can be created with `Context::add_module`.
    pub fn from_raw(mod_ref: LLVMModuleRef) -> Self {
        Module { raw: mod_ref }
    }

    /// Set the Modules's Target
    ///
    /// Defines which targe the module is being comiled for. This can
    /// enable target-specific optimisations in the compilation of
    /// this module.
    pub fn set_target(&mut self, target: &Target) {
        let triple = CString::new(target.norm_triple()).unwrap();
        unsafe {
            core::LLVMSetTarget(self.as_raw(), triple.as_ptr());
        }
    }

    /// Dump the Module
    ///
    /// Writes a representation of the module to standard output. This
    /// is intended to be used as an aid to debugging.
    pub fn dump(&self) {
        unsafe { core::LLVMDumpModule(self.raw) }
    }

    /// Verify the Module
    ///
    /// Checks that the whole module is valid before continuing
    pub fn verify_or_panic(&self) {
        let verified = unsafe {
            analysis::LLVMVerifyModule(
                self.as_raw(),
                analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                ptr::null_mut(),
            )
        };
        if verified != 0 {
            panic!("Module failed validation");
        }
    }

    /// Run the Optimisation Passes over the Module
    ///
    /// Given a target optimisation level transform the module to
    /// improve exectuion speed.
    ///
    /// # Parameters
    ///  * `level` - the optimisation level to target.
    ///  * `size` - Enum to control size optimisation.
    pub fn run_optimiser(&mut self, level: OptLevel, size: OptSize) {
        let pass_manager = PassManagerBuilder::new()
            .with_opt_level(level)
            .with_opt_size(size)
            .create_module_pass_manager();

        pass_manager.run(self);
    }

    /// Write the Module to the Given File as LLVM IR or Bitcode
    ///
    /// The kind of file written depends on `kind`.
    pub fn write_to_file(
        &self,
        target: &Target,
        path: &Path,
        kind: OutputFileKind,
    ) -> Result<(), String> {
        let path = path.to_str().and_then(|s| CString::new(s).ok()).unwrap();

        unsafe {
            let mut message = ptr::null_mut();
            let r = match kind {
                OutputFileKind::LLVMIl => {
                    core::LLVMPrintModuleToFile(self.raw, path.as_ptr(), &mut message)
                }
                OutputFileKind::Bitcode => {
                    bit_writer::LLVMWriteBitcodeToFile(self.raw, path.as_ptr())
                }
                OutputFileKind::NativeObject => {
                    let trip = CString::new(target.triple()).unwrap();
                    // To emit code we need to do a few things:
                    //  * Create an LLVM TargetMachine from our target.
                    //  * Create a pass manager
                    //  * Call targetMachine emit to file
                    let tm = target_machine::LLVMCreateTargetMachine(
                        target.as_llvm_target(),
                        trip.as_ptr(),
                        target_machine::LLVMGetHostCPUName(),
                        target_machine::LLVMGetHostCPUFeatures(),
                        target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                        target_machine::LLVMRelocMode::LLVMRelocDefault,
                        target_machine::LLVMCodeModel::LLVMCodeModelSmall,
                    );
                    let r = target_machine::LLVMTargetMachineEmitToFile(
                        tm,
                        self.as_raw(),
                        path.as_ptr() as *mut _,
                        target_machine::LLVMCodeGenFileType::LLVMObjectFile,
                        &mut message,
                    );
                    target_machine::LLVMDisposeTargetMachine(tm);
                    r
                }
            };
            if r == 0 {
                Ok(())
            } else {
                let err_str = CStr::from_ptr(message).to_string_lossy().into();
                core::LLVMDisposeMessage(message);
                Err(err_str)
            }
        }
    }

    /// Find a Function by Name
    pub fn find_function(&self, name: &str) -> Option<Function> {
        let function_name = CString::new(name).unwrap();
        unsafe {
            let found = core::LLVMGetNamedFunction(self.as_raw(), function_name.as_ptr());
            if found.is_null() {
                None
            } else {
                Some(Function::from_raw(found))
            }
        }
    }

    /// Add a Global Variable
    pub fn add_global(&mut self, initialiser: LLVMValueRef, name: &str) -> LLVMValueRef {
        let global_name = CString::new(name).unwrap();
        unsafe {
            let global = core::LLVMAddGlobal(
                self.as_raw(),
                core::LLVMTypeOf(initialiser),
                global_name.as_ptr(),
            );
            core::LLVMSetInitializer(global, initialiser);
            global
        }
    }

    /// Find a Global Variable in the Module by Name
    ///
    /// Looks up a given global variale in the module and returns
    /// it. If the variable doesn't exist in the module then `None` is
    /// returned.
    pub fn find_global(&self, name: &str) -> Option<LLVMValueRef> {
        let global_name = CString::new(name).unwrap();
        unsafe {
            let found = core::LLVMGetNamedGlobal(self.as_raw(), global_name.as_ptr());
            if found.is_null() {
                None
            } else {
                Some(found)
            }
        }
    }

    /// Raw Borrow
    ///
    /// # Safety
    ///
    /// This method returns a raw pointer to the underlying
    /// LLVMModule. It's up to you to make sure it doesn't outlive the
    /// `Module`, and to make sure you don't break any of LLVMs thread
    /// safety requirements.
    pub unsafe fn as_raw(&self) -> LLVMModuleRef {
        self.raw
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { core::LLVMDisposeModule(self.raw) }
    }
}

impl From<Module> for LLVMModuleRef {
    /// Consume the wrapped module and return it's interal module
    /// reference. This transfers the ownership of the module back to
    /// the caller preventing the it from being automaticaly freed.
    fn from(m: Module) -> LLVMModuleRef {
        unsafe {
            // an apparently nicer alterantive to calling `forget` we
            // instead create a `ManuallyDrop` item and then don't
            // drop it here.
            std::mem::ManuallyDrop::new(m).as_raw()
        }
    }
}
