//! This module contians the code required to compile a parsed tree
//! down to LLVM bytecode.

use syntax::Expression;
use low_loader::prelude::*;
use std::path::Path;
use tempdir::TempDir;
use std::process::Command;
use std::io;

pub use self::error::{Error, Result};

/// Compilation error module. Contains the Result and Error types for the compile module.
pub mod error {

    /// Represents the different types of errors which can be encountered
    /// when compiling.
    #[derive(Debug)]
    pub enum Error {
        Generic(String),
        IO(::std::io::Error),
    }

    /// Compilation result. Returned from each compilation stage.
    pub type Result<T> = ::std::result::Result<T, Error>;
}

impl From<String> for Error {
    /// Convert untyped errors to generic compilation errors.
    fn from(s: String) -> Error {
        Error::Generic(s)
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IO(e)
    }
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
        Compilation { exprs: exprs, dump_ir: dump_ir }
    }

    /// Emit
    ///
    /// Performs the compilation, emitting the results to the given file.
    pub fn emit(self, output_path: &Path) -> Result<()> {
        let mut ctx = Context::new();
        let mut module = ctx.add_module("<test>");
        let mut fun = ctx.add_function(&mut module, "main");
        let bb = ctx.add_block(&mut fun, "entry");
        let mut builder = ctx.add_builder();

        for _ in self.exprs {
            // TODO: Lower expressions here
        }

        let build_ctx = builder.build_at_end(bb);
        build_ctx.build_ret(ctx.const_int(0));

        // Check what we have, and dump it to the screen
        fun.verify_or_panic();

        // Create a tempdir to write the LLVM IR to
        let tmp_dir = TempDir::new("ullage").expect("create temp dir");
        let temp_path = tmp_dir.path().join("temp.ll");

        if self.dump_ir {
            module.dump();
        }

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
