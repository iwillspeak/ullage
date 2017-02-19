//! This module contians the code required to compile a parsed tree
//! down to LLVM bytecode.

use syntax::Expression;
use low_loader::prelude::*;

pub use self::error::{Error,Result};

/// Compilation error module. Contains the Result and Error types for the compile module.
pub mod error {
    /// Represents the different types of errors which can be encountered
    /// when compiling.
    #[derive(Debug,PartialEq)]
    pub enum Error {}

    /// Compilation result. Returned from each compilation stage.
    pub type Result<T> = ::std::result::Result<T, Error>;
}

/// Compilation State
///
/// Encompases the inputs and settings for a given compilation.
pub struct Compilation {

    /// The `Expression`s which are being compiled.
    exprs: Vec<Expression>,
}

impl Compilation {
    /// Create a new compilation
    pub fn new(exprs: Vec<Expression>) -> Self {
        Compilation {
            exprs: exprs,
        }
    }

    /// Emit
    ///
    /// Performs the compilation, emitting the results to the given file.
    pub fn emit(self) -> Result<()> {
        let mut ctx =  Context::new();
        let mut module = ctx.add_module("<test>");
        let mut fun = ctx.add_function(&mut module, "main");

        let _ = ctx.add_block(&mut fun, "entry");
        for _ in self.exprs {
            
        }

        // Check what we have, and dump it to the screen
        fun.verify_or_panic();
        module.dump();

        Ok(())
    }
}
