//! LLVM Targets

use super::llvm_sys::core::LLVMDisposeMessage;
use super::llvm_sys::target_machine::*;
use failure::Fail;
use libc;
use std::ffi::{CStr, CString};
use std::{fmt, ptr};

/// Compilation Target
///
/// Represents a validated target triple. Can be used to build a
/// target machine to compile to.
pub struct Target {
    llvm_target: LLVMTargetRef,
    triple: String,
}

/// Target Lookup Error
///
/// Returned if a target couldn't be resolved from the given triple.
#[derive(Fail, Debug)]
#[fail(display = "Could not find target: '{}'", _0)]
pub struct TargetLookupError(String);

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Target information for {}:", self.triple)?;
        writeln!(f, " name: {}", self.name())?;
        writeln!(f, " description: {}", self.description())?;
        writeln!(f, " normalised triple: {}", self.norm_triple())?;
        writeln!(f, " has asm backend: {}", self.has_asm_backend())
    }
}

impl Default for Target {
    /// Get the Default (host) Target
    fn default() -> Self {
        let trip = get_default_triple();
        Target::from_triple(&trip).expect("Default triple should be found")
    }
}

impl Target {
    /// Create a Target from a Triple
    ///
    /// Looks the given target trip up and returns the coresponding
    /// concrete target. If the triple isn't a valid compilation targe
    /// then a `TargetLookupError` is returned.
    pub fn from_triple(triple: &str) -> Result<Self, TargetLookupError> {
        super::context::ensure_initialised();

        let ffi_trip = CString::new(triple).unwrap();

        let mut target: LLVMTargetRef = ptr::null_mut();
        let mut error: *mut libc::c_char = ptr::null_mut();
        let r = unsafe {
            LLVMGetTargetFromTriple(
                ffi_trip.as_ptr(),
                &mut target as *mut LLVMTargetRef,
                &mut error as *mut *mut libc::c_char,
            )
        };
        if r != 0 {
            let err = unsafe { CStr::from_ptr(error) };
            let err = err.to_string_lossy().to_string();
            unsafe { LLVMDisposeMessage(error) };
            return Err(TargetLookupError(err));
        }
        Ok(Target {
            llvm_target: target,
            triple: triple.to_owned(),
        })
    }

    /// Get the Target name
    ///
    /// Retrieves the logical name for this target
    pub fn name(&self) -> &str {
        let name = unsafe { CStr::from_ptr(LLVMGetTargetName(self.llvm_target)) };
        name.to_str().expect("Target name should be valid unicode")
    }

    /// Get the Target Descirption
    ///
    /// Retrieves the short description of the target
    pub fn description(&self) -> &str {
        let desc = unsafe { CStr::from_ptr(LLVMGetTargetDescription(self.llvm_target)) };
        desc.to_str()
            .expect("Target description should be valid unicode")
    }

    /// Get the Target Triple
    ///
    /// Retrieves the triple that was used to create this target.
    pub fn triple(&self) -> &str {
        &self.triple[..]
    }

    /// Get the Normalised Target Triple
    ///
    /// This is the canonical version of the target triple
    pub fn norm_triple(&self) -> &str {
        let ffi_trip = CString::new(&self.triple[..]).unwrap();
        let norm_trip = unsafe { CStr::from_ptr(LLVMNormalizeTargetTriple(ffi_trip.as_ptr())) };
        norm_trip
            .to_str()
            .expect("normalised triple should be valid unicode")
    }

    /// Does the Target have an ASM Backend
    pub fn has_asm_backend(&self) -> bool {
        let has_backend = unsafe { LLVMTargetHasAsmBackend(self.llvm_target) };
        has_backend != 0
    }
}

/// Get the Default Target Triple
///
/// Returns the triple for the native target. To be used as a fallback
/// if no triple is specified by the user.
pub fn get_default_triple() -> String {
    let native_target = unsafe { CStr::from_ptr(LLVMGetDefaultTargetTriple()) };
    native_target.to_string_lossy().to_owned().to_string()
}

/// Dump Available LLVM Targets
///
/// Prints a list of LLVM target triple available in this build.
pub fn dump_targets() {
    super::context::ensure_initialised();

    let native_target = unsafe { LLVMGetDefaultTargetTriple() };

    println!("default triple: {:?}", unsafe {
        CStr::from_ptr(native_target)
    });
    println!();
    println!("supported targets:");
    let mut target = unsafe { LLVMGetFirstTarget() };
    while target.is_null() {
        let name = unsafe { CStr::from_ptr(LLVMGetTargetName(target)) };
        let desc = unsafe { CStr::from_ptr(LLVMGetTargetDescription(target)) };
        println!(" * {} ({})", name.to_str().unwrap(), desc.to_str().unwrap());
        target = unsafe { LLVMGetNextTarget(target) };
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn create_default_triple() {
        let default: Target = Default::default();
        assert_eq!(get_default_triple(), default.triple());
        assert_eq!(default.triple(), default.norm_triple());
    }

    #[test]
    fn create_invalid_triple() {
        let r = Target::from_triple("im-not-valid");
        assert!(r.is_err());
    }

    #[test]
    fn create_known_triple() {
        let target = Target::from_triple("aarch64-linux-gnu").unwrap();
        assert_eq!("aarch64", target.name());
        assert_eq!("AArch64 (little endian)", target.description());
        assert_eq!(true, target.has_asm_backend());
        assert_eq!("aarch64-linux-gnu", target.triple());
        assert_eq!("aarch64-unknown-linux-gnu", target.norm_triple());
    }

    #[test]
    fn create_long_triple() {
        let target = Target::from_triple("arm-eabi-linux-gnu-elf").unwrap();
        assert_eq!("arm", target.name());
        assert_eq!("ARM", target.description());
        assert_eq!(true, target.has_asm_backend());
        assert_eq!("arm-eabi-linux-gnu-elf", target.triple());
        assert_eq!("arm-eabi-linux-gnu-elf", target.norm_triple());
    }
}
