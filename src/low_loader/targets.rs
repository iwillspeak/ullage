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
        let name = unsafe { LLVMGetTargetName(target) };
        println!(" * {:?}", unsafe { CStr::from_ptr(name)});
        target = unsafe { LLVMGetNextTarget(target)};
    }
}
