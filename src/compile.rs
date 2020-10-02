//! This module contians the code required to compile a parsed tree
//! down to LLVM bytecode.

use crate::diag::Diagnostic;
use crate::low_loader::prelude::*;
use crate::sem;
use crate::syntax;
use std::path::Path;
use std::process::Command;
use tempfile::Builder;

pub use self::error::{CompError, CompResult};
pub use self::options::{CompilationOptions, OptimisationLevel, LinkKind};

pub mod error;
pub mod options;

mod lower;
mod lower_context;
mod string_builtins;

/// Add the Core Declarations to the Module
///
/// This method is responsible for making sure that
/// declarations/definitions of any builtin funtions are emitted.
fn add_core_decls(ctx: &mut Context, module: &mut Module) -> CompResult<()> {
    add_printf_decl(ctx, module);
    module.add_global(ctx.const_str("%lld\n"), "printf_num_format");
    module.add_global(ctx.const_str("%s\n"), "printf_cstr_format");
    module.add_global(ctx.const_str("%.*s\n"), "printf_ustr_format");
    module.add_global(ctx.const_str("true"), "print_true");
    module.add_global(ctx.const_str("false"), "print_false");
    Ok(())
}

/// Add a Printf Declaration to the Module
///
/// Creates a new function in the given module which maps to the
/// `printf` function. This will be used by the `print` operator
/// to write output.
fn add_printf_decl(ctx: &mut Context, module: &mut Module) {
    let mut params = [ctx.cstr_type()];
    let int_type = ctx.int_type(32);
    let mut printf = ctx.add_varargs_function(module, "printf", int_type, &mut params);
    printf.set_calling_convention(CallConvention::CDecl);
}

/// Compilation State
///
/// Encompases the inputs and settings for a given compilation.
pub struct Compilation {
    /// The `Expression`s which are being compiled.
    expr: sem::Expression,
    /// The options for this compilation
    options: CompilationOptions,
    /// diagnostics from this compilation
    diagnostics: Vec<Diagnostic>,
}

impl Compilation {
    /// Create a new compilation
    ///
    /// # Parameters
    ///  * `source` - The source text for the program
    ///  * `expr` - the expression to compile
    ///  * `opts` - The compilation options
    #[allow(clippy::new_ret_no_self)]
    pub fn new(tree: syntax::SyntaxTree, opts: CompilationOptions) -> CompResult<Self> {
        let mut binder = sem::Binder::new(sem::Scope::new());
        let sem_expr = binder.bind_tree(tree);
        Ok(Compilation {
            expr: sem_expr,
            options: opts,
            diagnostics: binder.take_diagnostics(),
        })
    }

    /// Emit
    ///
    /// Performs the compilation, emitting the results to the given file.
    pub fn emit(self, target: &Target, output_path: &Path) -> CompResult<()> {
        if !self.diagnostics.is_empty() {
            return Err(CompError::Generic(
                "can't emit a compilation contianing diagnostics".into(),
            ));
        }

        let mut ctx = Context::new();
        let name = output_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("fallback_module_name");
        let mut module = ctx.add_module(name);
        module.set_target(target);

        add_core_decls(&mut ctx, &mut module)?;

        let fun = {
            let mut lower_ctx = lower_context::LowerContext::new(&mut ctx, &mut module);
            lower_ctx.add_intrinsics();
            lower_ctx.add_core_types();
            lower::lower_as_main(&mut lower_ctx, self.expr)?
        };

        fun.verify_or_panic();
        module.verify_or_panic();

        // Create a tempdir to write the LLVM IR or bitcode to
		let (suffix, kind) = match self.options.link_kind {
			LinkKind::IL => (".il", OutputFileKind::LLVMIl),
			LinkKind::Bitcode => (".bc", OutputFileKind::Bitcode),
			LinkKind::Object => (".o", OutputFileKind::NativeObject),
		};
        let temp_file = Builder::new().prefix("ullage").suffix(suffix).tempfile()?;

        // check if we have optimiation enabled and run the
        // corresponding optimisations if we do.
        if let Some((level, size)) = self.options.opt_level.unpack() {
            module.run_optimiser(level, size);
        }

        // Check what we have, and dump it to the screen
        if self.options.dump_ir {
            module.dump();
        }
        module.write_to_file(&target, temp_file.path(), kind)?;

        // Shell out to Clang to link the final assembly
        let output = Command::new("clang")
            .arg(temp_file.path())
            .arg(format!("--target={}", target.triple()))
            .arg("-o")
            .arg(output_path)
            .output()?;
        let status = output.status;

        if status.success() {
            Ok(())
        } else {
            Err(CompError::link_fail(status.code(), output.stderr))
        }
    }

    /// Does the compilation have any diagnostics to emit?
    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    /// Borrow a slice of the diagnostics in this compilation
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}
