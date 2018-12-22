//! String Builtins
//!
//! This module contains logic for interacting with string values.

use super::lower_context::LowerContext;
use crate::low_loader::prelude::*;

/// String Copy Guts
///
/// Copies the body of a source string into a destiation string's
/// buffer. This is used by the lowering of stirng concatentation.
pub(crate) fn string_copy_guts(
    ctx: &mut LowerContext<'_>,
    builder: &mut Builder,
    dest: LLVMValueRef,
    src: LLVMValueRef,
    len: LLVMValueRef,
    offset: LLVMValueRef,
) {
    let memcpy = ctx
        .module
        .find_function("llvm.memcpy.p0i8.p0i8.i32")
        .expect("can't find memcpy intrinsic");

    let src_buffer = string_get_buffer(builder, src);
    let src_buffer = builder.build_gep(
        src_buffer,
        &mut vec![ctx.llvm_ctx.const_int(0), ctx.llvm_ctx.const_int(0)],
    );
    let dest_buffer = string_get_buffer(builder, dest);
    let dest_buffer = builder.build_gep(dest_buffer, &mut vec![ctx.llvm_ctx.const_int(0), offset]);

    builder.build_void_call(
        &memcpy,
        &mut vec![dest_buffer, src_buffer, len, ctx.llvm_ctx.const_bool(false)],
    );
}

/// Get String's Buffer Pointer
///
/// Returns a poitner to the buffer which contains the `String`'s
/// body. This is a `[0 x i8]*`. It can be converted into a poitner to
/// a given character offset with a GEP instruction]
pub(crate) fn string_get_buffer(builder: &mut Builder, s: LLVMValueRef) -> LLVMValueRef {
    builder.build_struct_gep(s, 1)
}

/// Get String Length
///
/// Reads the length field out of the `String`'s internal
/// representation. This is a constant-time operation and returns the
/// length in bytes.
pub(crate) fn string_get_len(builder: &mut Builder, val: LLVMValueRef) -> LLVMValueRef {
    let len_field = builder.build_struct_gep(val, 0);
    builder.build_load(len_field)
}

/// Set Stirng Length
///
/// Set the internal string length field to a new value.
pub(crate) fn string_set_len(builder: &mut Builder, val: LLVMValueRef, size: LLVMValueRef) {
    let len_field = builder.build_struct_gep(val, 0);
    builder.build_store(size, len_field);
}
