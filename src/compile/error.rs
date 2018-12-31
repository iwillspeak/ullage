//! Compilation error module. Contains the Result and Error types for
//! the compile module.

use std::io;

/// Represents the different types of errors which can be encountered
/// when compiling.
#[derive(Fail, Debug)]
pub enum CompError {
    /// Generic Error String
    #[fail(display = "compilation error: {}", _0)]
    Generic(String),

    /// Linker Failure
    #[fail(display = "linker failed: {}", _0)]
    Linker(#[cause] LinkerError),

    /// Wrapped IO Error
    #[fail(display = "IO error: {}", _0)]
    IO(#[cause] ::std::io::Error),
}

/// Compilation result. Returned from each compilation stage.
pub type CompResult<T> = Result<T, CompError>;

/// Link Failure Type
///
/// Used to group together the different failure modes for the linker.
#[derive(Fail, Debug)]
pub enum LinkerError {
    /// The linker failed with a known exit status
    #[fail(display = "linker returned exit status {}: {}", _0, _1)]
    WithExitStatus(i32, String),

    /// The linker failed with an unknown exit status
    #[fail(display = "unknown linker error: {}", _0)]
    UnknownFailure(String),
}

impl From<String> for CompError {
    /// Convert untyped errors to generic compilation errors.
    fn from(s: String) -> Self {
        CompError::Generic(s)
    }
}

impl From<io::Error> for CompError {
    fn from(e: io::Error) -> Self {
        CompError::IO(e)
    }
}

impl CompError {
    /// Compilation Linker Error
    ///
    /// When the linker has failed and caused compilation to fail.
    pub fn link_fail(exit_status: Option<i32>, stderr: Vec<u8>) -> Self {
        let stderr = String::from_utf8(stderr).unwrap();
        CompError::Linker(match exit_status {
            Some(status) => LinkerError::WithExitStatus(status, stderr),
            None => LinkerError::UnknownFailure(stderr),
        })
    }
}
