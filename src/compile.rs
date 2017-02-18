//! This module contians the code required to compile a parsed tree
//! down to LLVM bytecode.

use std::io;
use syntax::Expression;
use self::error::{Error,Result};

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
    tree: Vec<Expression>,
}

impl Compilation {
    /// Create a new compilation
    pub fn new(tree: Vec<Expression>) -> Self {
        Compilation {
            tree: tree
        }
    }

    /// Emit
    ///
    /// Performs the compilation, emitting the results to the given file.
    pub fn emit(self, output: &mut io::Write) -> Result<()> {
        Ok(())
    }
}
