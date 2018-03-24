//! Lower Context
//!
//! The lower context defines the state which is passed around as an
//! expresion is lowered to LLVM.

use std::collections::HashMap;
use low_loader::prelude::*;
use sem::{BuiltinType, Typ};

/// Lower Context
///
/// Pairs an LLVM Context with a single LLVM Module. Used as the
/// target when lowering a tree to LLVM.
pub struct LowerContext<'a> {
    /// The LLVM Context this lower context is using.
    pub llvm_ctx: &'a mut Context,
    /// The LLVM Module this context is building IR into.
    pub module: &'a mut Module,
    /// Map of Ty values to LLVM Types
    ty_map: HashMap<Typ, LLVMTypeRef>,
}

impl<'a> LowerContext<'a> {

    /// Create a New Lowering Context
    ///
    /// Wraps the given module and LLVM context to create the required
    /// context for lowering ASTs.
    pub fn new(ctx: &'a mut Context, module: &'a mut Module) -> Self {
        LowerContext {
            llvm_ctx: ctx,
            module: module,
            ty_map: Default::default(),
        }
    }

    pub fn add_core_types(&mut self) {
        // TODO: Unit -> void
        // TODO: String -> (len, [u8])
        let llvm_bool = self.llvm_ctx.bool_type();
        self.add_type(Typ::Builtin(BuiltinType::Bool), llvm_bool);
        let llvm_number = self.llvm_ctx.int_type(64);
        self.add_type(Typ::Builtin(BuiltinType::Number), llvm_number);
    }

    /// Add a Type to the Context
    ///
    /// Makes the given type available for lowering in the current
    /// context by providing a mapping through to an LLVM type.
    pub fn add_type(&mut self, ty: Typ, llvm_ty: LLVMTypeRef) {
        self.ty_map.insert(ty, llvm_ty);
    }

    /// Look up a Given Type
    pub fn llvm_type(&self, ty: &Typ) -> Option<LLVMTypeRef> {
        self.ty_map.get(ty).cloned()
    }
}
