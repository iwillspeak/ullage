//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

#![warn(missing_docs)]

use failure;

pub mod compile;
pub mod low_loader;
pub mod meta;
pub mod sem;
pub mod syntax;
pub mod diag;

use crate::compile::*;
use crate::low_loader::targets;
use crate::syntax::*;
use crate::syntax::text::{Location, DUMMY_SPAN};
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
  --target=<triple>      Set the compilation target triple.
  --dumpir               Dump the LLVM IR for the module.
  --dumpast              Dump the syntax tree to stdout and exit.
  --dumptargets          Dump the available targets and exit.
  --dumptargetinfo       Dump information about the given triple.
";

/// Program Arguments
///
/// Structure to capture the command line arguments for the
/// program. This is filled in for us by Docopt.
#[derive(Debug, Deserialize)]
struct Args {
    flag_dumpast: bool,
    flag_output: Option<String>,
    flag_optimise: Option<OptFlag>,
    flag_dumpir: bool,
    flag_dumptargets: bool,
    flag_dumptargetinfo: bool,
    flag_target: Option<String>,
    arg_file: Option<String>,
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
        for error in tree.diagnostics().iter() {
            // TODO: Move this formatting into some library code.
            if error.span == DUMMY_SPAN {
                eprintln!("error: {}", error.message);
            } else {
                let pos = source.line_pos(error.span.start());
                eprintln!("{}:{}:error: {}", pos.0, pos.1, error.message);
            }
        }
        exit(1)
    };

    // Are we just dumping the AST or compiling the whole thing?
    if args.flag_dumpast {
        println!("parsed AST: {:#?}", tree.root());
        exit(0);
    }

    let options = CompilationOptions::default()
        .with_dump_ir(args.flag_dumpir)
        .with_opt_level(
            args.flag_optimise
                .map_or(OptimisationLevel::Off, |o| o.into()),
        );
    let comp = match Compilation::new(&source, tree, options) {
        Ok(c) => c,
        Err(e) => handle_comp_err(&e),
    };

    // Create a compilation, and emit to the output path
    let emit_result = comp.emit(&target, &output_path);

    // Print any failures encountered and return a failure status
    if let Err(e) = emit_result {
        handle_comp_err(&e);
    }
}

/// Handles a Compilation Error
///
/// Prints the error to standard output and exits the process.
fn handle_comp_err(err: &CompError) -> ! {
    eprintln!("error: compilation error: {}", err);
    exit(1);
}
