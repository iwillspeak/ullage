//! LLVM Targets

use std::ffi::{CStr, CString};
use std::ptr;
use libc;
use super::llvm_sys::target_machine::*;
use super::llvm_sys::core::LLVMDisposeMessage;

/// Dump Available LLVM Targets
///
/// Prints a list of LLVM target triple available in this build.
pub fn dump_targets() {
    super::context::ensure_initialised();

    let native_target = unsafe { LLVMGetDefaultTargetTriple() };

    println!("default triple: {:?}", unsafe { CStr::from_ptr(native_target) });
    println!("");
    println!("supported targets:");
    let mut target = unsafe { LLVMGetFirstTarget() };
    while target != ptr::null_mut() {
        let name = unsafe { CStr::from_ptr(LLVMGetTargetName(target)) };
        let desc = unsafe { CStr::from_ptr(LLVMGetTargetDescription(target)) };
        println!(" * {} ({})", name.to_str().unwrap(), desc.to_str().unwrap());
        target = unsafe { LLVMGetNextTarget(target)};
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

/// Dump the Target Info
///
/// Provides a summary of the target to the standard output.
pub fn dump_target_info(triple: &str) {
    super::context::ensure_initialised();

    println!("Target information for {}:", triple);
    let triple = CString::new(triple).unwrap();
    let mut target: LLVMTargetRef = ptr::null_mut();
    let mut error: *mut libc::c_char = ptr::null_mut();
    unsafe {
        if LLVMGetTargetFromTriple(
            triple.as_ptr(),
            &mut target as *mut LLVMTargetRef,
            &mut error as *mut *mut libc::c_char) != 0 {
            let err = CStr::from_ptr(error);
            eprintln!("error getting target information: {:?}", err);
            LLVMDisposeMessage(error);
            return;
        }
    }
    let name = unsafe { CStr::from_ptr(LLVMGetTargetName(target)) };
    println!(" name: {}", name.to_str().unwrap());
    let desc = unsafe { CStr::from_ptr(LLVMGetTargetDescription(target)) };
    println!(" description: {}", desc.to_str().unwrap());
    let norm_trip = unsafe { CStr::from_ptr(LLVMNormalizeTargetTriple(triple.as_ptr())) };
    println!(" normalised triple: {}", norm_trip.to_str().unwrap());
    let asm_back = unsafe { LLVMTargetHasAsmBackend(target) != 0 };
    println!(" has asm backend: {}", asm_back);
}
