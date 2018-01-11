//! Lower Context
//!
//! The lower context defines the state which is passed around as an
//! expresion is lowered to LLVM.

use low_loader::prelude::*;

pub struct LowerContext<'a> {
    /// The LLVM Context this lower context is using.
    pub llvm_ctx: &'a mut Context,
    /// The LLVM Module this context is building IR into.
    pub module: &'a mut Module,
}

impl<'a> LowerContext<'a> {
    pub fn new(ctx: &'a mut Context, module: &'a mut Module) -> Self {
        LowerContext {
            llvm_ctx: ctx,
            module: module,
        }
    }
}
