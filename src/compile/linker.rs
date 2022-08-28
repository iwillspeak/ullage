//! Linker Abstractions
//!
//! This module contains types to deal with linking object files. The
//! main struct `Linker` specifies the information needed to perform
//! a link.

use std::default::Default;

use crate::low_loader::prelude::OutputFileKind;

/// The information for performing a link
pub struct Linker {
    /// The linker command. Currently only `clang` is supported.
    pub cmd: LinkerCommand,
    /// The intermediate asset type the linker expects
    pub asset_ty: LinkerAssetType,
}

/// The executable type to use for linking
#[derive(Debug, Copy, Clone)]
pub enum LinkerCommand {
    /// The Clang c compiler
    Clang,
}

/// The intermediate asset type to pass to the linker
#[derive(Debug, Copy, Clone)]
pub enum LinkerAssetType {
    /// LLVM IR text files
    LlvmIr,
    /// LLVM IR bticode files
    LlvmBc,
    /// Native object
    Object,
}

impl Linker {
    /// Create a new linker from the command and asset type
    pub fn new(cmd: LinkerCommand, asset_ty: LinkerAssetType) -> Self {
        Linker { cmd, asset_ty }
    }

    /// Create a linker from the given command
    pub fn from_command(cmd: LinkerCommand) -> Self {
        Linker {
            cmd,
            asset_ty: cmd.default_asset_ty(),
        }
    }
}

impl Default for Linker {
    fn default() -> Self {
        Linker::from_command(LinkerCommand::default())
    }
}

impl LinkerCommand {
    /// Get the Default Asset Type for this Linker
    pub fn default_asset_ty(&self) -> LinkerAssetType {
        LinkerAssetType::LlvmBc
    }

    /// Get the executable this command should call.
    ///
    pub fn executable(&self) -> &str {
        // FIXME: instead of exposing a &str. We should
        //        make the linker buidl the command
        //        rather than the compiler.
        match *self {
            LinkerCommand::Clang => "clang",
        }
    }
}

impl Default for LinkerCommand {
    fn default() -> Self {
        LinkerCommand::Clang
    }
}

impl LinkerAssetType {
    /// Get the file extension for the asset type
    pub fn extension(&self) -> &str {
        match *self {
            LinkerAssetType::LlvmIr => ".ll",
            LinkerAssetType::LlvmBc => ".bc",
            LinkerAssetType::Object => ".o",
        }
    }

    /// Get the file kind for this asset type
    pub(crate) fn file_kind(&self) -> OutputFileKind {
        match *self {
            LinkerAssetType::LlvmIr => OutputFileKind::LLVMIl,
            LinkerAssetType::LlvmBc => OutputFileKind::Bitcode,
            LinkerAssetType::Object => OutputFileKind::NativeObject,
        }
    }
}
