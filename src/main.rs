//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

#![warn(missing_docs)]

pub mod compile;
pub mod diag;
pub mod low_loader;
pub mod meta;
pub mod sem;
pub mod syntax;

use crate::compile::*;
use crate::low_loader::targets;
use crate::syntax::text::DUMMY_SPAN;
use crate::syntax::*;
use docopt::Docopt;
use serde::{Deserialize, Deserializer};
use std::fmt;
use std::path::Path;
use std::process::*;

/// Usage Information
///
/// This is a [Docopt] compliant usage description of this program.
///
///  [Docopt]: http://docopt.org/
const USAGE: &str = "
Ullage Compiler

Usage:
  ullage [--version --help]
  ullage [options] [-o <outfile>] [<file>]

Options:
  -h, --help             Show this message.
  --version              Show version.
  -O, --optimise=<lvl>   Set the compilation optimisation level.
                         0 = off, 1 = low, 2 = medium, 3 = high, s = size.
  -o, --output=<out>     Write the output to <out>.
  --link-kind=<type>     Set the link type to perform.
  --target=<triple>      Set the compilation target triple.
  --link-mode=<mode>     Set the type of intermediate assets to produce
                         for linking. Use `llvmIr`, or `llvmBc`.
  --dumpir               Dump the LLVM IR for the module.
  --dumpast              Dump the syntax tree to stdout and exit.
  --prettytree           Dump a prettified summary of the syntax tree.
  --dumptargets          Dump the available targets and exit.
  --dumptargetinfo       Dump information about the given triple.
";

/// Program Arguments
///
/// Structure to capture the command line arguments for the
/// program. This is filled in for us by Docopt.
#[derive(Debug, Deserialize)]
struct Args {
    flag_output: Option<String>,
    flag_optimise: Option<OptFlag>,
    flag_target: Option<String>,
    flag_link_mode: Option<LinkMode>,
    arg_file: Option<String>,

    // TODO: maybe move these dump options into a single flag?
    flag_dumpast: bool,
    flag_prettytree: bool,
    flag_dumpir: bool,
    flag_dumptargets: bool,
    flag_dumptargetinfo: bool,
}

/// Optimisation Level
///
/// Used to hold the requested optimisation level
#[derive(Debug)]
enum OptFlag {
    /// numeric optimisation level
    Numeric(u64),
    /// size optimisation
    Size,
}

/// Liker Mode
///
/// Chose the type of intermediate assets to produce when
/// performnig the link.
#[derive(Debug, Deserialize)]
enum LinkMode {
    /// Intermediate langauge files
    #[serde(rename = "il")]
    LlvmIr,
    /// Bitcode files
    #[serde(rename = "bc")]
    LlvmBc,
}

/// Custom Deserialiser for Optimisation Flags
///
/// This deserialiser will handle both numeric values and 's' or
/// 'size'. Numbers greater than 3 are accepted, and transformed into
/// range when converting to the stronger `OptimisationLevel` type.
impl<'de> Deserialize<'de> for OptFlag {
    fn deserialize<D>(d: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct OptFlagVisitor;
        impl<'de> serde::de::Visitor<'de> for OptFlagVisitor {
            type Value = OptFlag;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a number from range 0..3, or s for size")
            }

            fn visit_u64<E>(self, n: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(OptFlag::Numeric(n))
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "size" | "s" => Ok(OptFlag::Size),
                    s => s.parse::<u64>().map(OptFlag::Numeric).map_err(|_| {
                        let err = format!("Could not deserialize '{}' as optimisation flag", value);
                        E::custom(err)
                    }),
                }
            }
        }

        d.deserialize_identifier(OptFlagVisitor)
    }
}

impl From<OptFlag> for OptimisationLevel {
    fn from(flag: OptFlag) -> Self {
        match flag {
            OptFlag::Numeric(level) => match level {
                0 => OptimisationLevel::Off,
                1 => OptimisationLevel::Low,
                2 => OptimisationLevel::Med,
                _ => OptimisationLevel::High,
            },
            OptFlag::Size => OptimisationLevel::Size,
        }
    }
}

impl From<LinkMode> for linker::Linker {
    fn from(mode: LinkMode) -> Self {
        linker::Linker::new(
            linker::LinkerCommand::default(),
            match mode {
                LinkMode::LlvmIr => linker::LinkerAssetType::LlvmIr,
                LinkMode::LlvmBc => linker::LinkerAssetType::LlvmBc,
            },
        )
    }
}

/// Main
///
/// The main function for `ullage`. Parses the options and runs the
/// selected command.
fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| {
            d.help(true)
                .version(Some(meta::descriptive_version()))
                .deserialize()
        })
        .unwrap_or_else(|e| e.exit());

    if args.flag_dumptargets {
        targets::dump_targets();
        if args.arg_file.is_none() {
            exit(0);
        }
    }

    let triple = args.flag_target.unwrap_or_else(targets::get_default_triple);
    let target = targets::Target::from_triple(&triple).unwrap_or_else(|e| {
        eprintln!("error: could not create target: {}", e);
        exit(1);
    });

    if args.flag_dumptargetinfo {
        println!("{}", target);
        if args.arg_file.is_none() {
            exit(0);
        }
    }

    let output_path = &args.flag_output.unwrap_or_else(|| "a.out".to_string());
    let output_path = Path::new(&output_path);

    // Load the file into memory, so we can parse it into a syntax tree
    let source = if let Some(path) = args.arg_file {
        text::SourceText::from_path(Path::new(&path))
    } else {
        text::SourceText::from_stdin()
    };
    let source = source.unwrap_or_else(|e| {
        eprintln!("error: could not read input: {}", e);
        exit(1)
    });

    // Parse the module
    let tree = syntax::SyntaxTree::parse(&source);
    if tree.has_diagnostics() {
        eprintln!("error: could not parse source: one or more errors:");
        dump_diagnostics(&source, tree.diagnostics());
        exit(1)
    };

    // Are we just dumping the AST or compiling the whole thing?
    if args.flag_dumpast {
        println!("parsed AST: {:#?}", tree.root());
        exit(0);
    }
    if args.flag_prettytree {
        tree.write_to(&mut std::io::stdout()).unwrap();
        exit(0);
    }

    let mut options = CompilationOptions::default()
        .with_dump_ir(args.flag_dumpir)
        .with_opt_level(
            args.flag_optimise
                .map_or(OptimisationLevel::Off, |o| o.into()),
        );
    if let Some(link_mode) = args.flag_link_mode {
        let linker = linker::Linker::from(link_mode);
        options = options.with_linker(linker);
    }

    let comp = match Compilation::new(tree, options) {
        Ok(c) => c,
        Err(e) => handle_comp_err(&e),
    };

    if comp.has_diagnostics() {
        dump_diagnostics(&source, comp.diagnostics());
        let diag_count = comp.diagnostics().len();
        eprintln!("error: compilation failed with {} errors", diag_count);
        exit(1);
    }

    // Create a compilation, and emit to the output path
    let emit_result = comp.emit(&target, &output_path);

    // Print any failures encountered and return a failure status
    if let Err(e) = emit_result {
        handle_comp_err(&e);
    }
}

/// Write Dignostics to STDERR
///
fn dump_diagnostics(source: &text::SourceText, diagnostics: &[diag::Diagnostic]) {
    for error in diagnostics.iter() {
        if error.span == DUMMY_SPAN {
            eprintln!("{}:error: {}", source.name(), error.message);
        } else {
            let pos = source.line_pos(error.span.start());
            eprintln!(
                "{}:{}:{}:error: {}",
                source.name(),
                pos.0,
                pos.1,
                error.message
            );
            let (s, e) = source.line_extents(error.span);
            eprintln!("     |");
            let mut line_no = pos.0;
            for line in source.slice(s, e).lines() {
                eprintln!("{:4} | {}", line_no, line);
                line_no += 1;
            }
            eprintln!("");
        }
    }
}

/// Handles a Compilation Error
///
/// Prints the error to standard output and exits the process.
fn handle_comp_err(err: &CompError) -> ! {
    eprintln!("error: compilation error: {}", err);
    exit(1);
}
