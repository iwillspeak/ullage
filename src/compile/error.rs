//! Compilation error module. Contains the Result and Error types for
//! the compile module.

use std::{fmt::Display, io};

/// Represents the different types of errors which can be encountered
/// when compiling.
#[derive(Debug)]
pub enum CompError {
    /// Generic Error String
    Generic(String),

    /// Linker Failure
    Linker(LinkerError),

    /// Wrapped IO Error
    IO(io::Error),
}

impl std::error::Error for CompError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            CompError::Linker(e) => Some(e),
            CompError::IO(e) => Some(e),
            _ => None,
        }
    }
}

impl Display for CompError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompError::Generic(msg) => write!(f, "compilation error: {}", msg),
            CompError::Linker(cause) => write!(f, "linker failed.: {}", cause),
            CompError::IO(cause) => write!(f, "IO error: {}", cause),
        }
    }
}

/// Compilation result. Returned from each compilation stage.
pub type CompResult<T> = Result<T, CompError>;

/// Link Failure Type
///
/// Used to group together the different failure modes for the linker.
#[derive(Debug)]
pub enum LinkerError {
    /// The linker failed with a known exit status
    WithExitStatus(i32, String),

    /// The linker failed with an unknown exit status
    UnknownFailure(String),
}

impl std::error::Error for LinkerError {}

impl Display for LinkerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkerError::WithExitStatus(status, msg) => {
                write!(f, "linker returned exit status {}: {}", status, msg)
            }
            LinkerError::UnknownFailure(msg) => write!(f, "unknown linker error: {}", msg),
        }
    }
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
