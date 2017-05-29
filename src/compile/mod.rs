//! This module contians the code required to compile a parsed tree
//! down to LLVM bytecode.

use syntax::ast::expression::Expression;
use low_loader::prelude::*;
use std::path::Path;
use tempdir::TempDir;
use std::process::Command;

pub use self::error::{Error, Result};

pub mod error;

mod lower;

/// Add the Core Declarations to the Module
///
/// This method is responsible for making sure that
/// declarations/definitions of any builtin funtions are emitted.
fn add_core_decls(ctx: &mut Context, module: &mut Module) -> Result<()> {
    ctx.add_printf_decl(module);
    module.add_global(ctx.const_str("%d\n"), "printf_num_format");
    Ok(())
}

/// Compilation State
///
/// Encompases the inputs and settings for a given compilation.
pub struct Compilation {
    /// The `Expression`s which are being compiled.
    exprs: Vec<Expression>,

    /// Should the compilation dump the IR produced?
    dump_ir: bool,
}

impl Compilation {
    /// Create a new compilation
    pub fn new(exprs: Vec<Expression>, dump_ir: bool) -> Self {
        Compilation {
            exprs: exprs,
            dump_ir: dump_ir,
        }
    }

    /// Emit
    ///
    /// Performs the compilation, emitting the results to the given file.
    pub fn emit(self, output_path: &Path) -> Result<()> {
        let mut ctx = Context::new();
        let mut module = ctx.add_module(output_path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap());

        try!(add_core_decls(&mut ctx, &mut module));

        let mut fun = ctx.add_function(&mut module, "main", &mut []);
        let bb = ctx.add_block(&mut fun, "entry");

        let mut builder = ctx.add_builder();
        builder.position_at_end(bb);

        try!(lower::lower_expressions(&mut ctx, &mut module, &mut fun, &mut builder, self.exprs));

        builder.build_ret(ctx.const_int(0));

        // Check what we have, and dump it to the screen
        if self.dump_ir {
            module.dump();
        }
        fun.verify_or_panic();

        // Create a tempdir to write the LLVM IR to
        let tmp_dir = TempDir::new("ullage").expect("create temp dir");
        let temp_path = tmp_dir.path().join("temp.ll");

        try!(module.write_to_file(&temp_path));

        // Shell out to Clang to link the final assembly
        let output = try!(Command::new("clang")
            .arg(temp_path)
            .arg("-o")
            .arg(output_path)
            .output());
        let status = output.status;

        if status.success() {
            Ok(())
        } else {
            Err(Error::Generic(format!("clang failed with exit status: {}",
                                       status.code().unwrap())))
        }
    }
}
