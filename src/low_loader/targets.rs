//! LLVM Targets

use std::ffi::CStr;
use std::ptr;
use super::llvm_sys::target_machine::*;

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
