//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

#![warn(missing_docs)]

extern crate docopt;
extern crate failure;
extern crate libc;
extern crate tempfile;

#[macro_use]
extern crate failure_derive;
#[macro_use]
extern crate serde_derive;

pub mod compile;
pub mod low_loader;
pub mod meta;
pub mod sem;
pub mod syntax;

use crate::compile::*;
use crate::low_loader::targets;
use crate::syntax::*;
use docopt::Docopt;
use failure::Error;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::process::*;

/// Usage Information
///
/// This is a [Docopt] compliant usage description of this program.
///
///  [Docopt]: http://docopt.org/
const USAGE: &'static str = "
Ullage Compiler

Usage:
  ullage [--version --help]
  ullage [options] [-o <outfile>] [<file>]

Options:
  -h, --help             Show this message.
  --version              Show version.
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
    flag_dumpir: bool,
    flag_dumptargets: bool,
    flag_dumptargetinfo: bool,
    flag_target: Option<String>,
    arg_file: Option<String>,
}

/// Main
///
/// The main function for `ullage`. Parses the options and runs the
/// selected command.
fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| {
            d.help(true)
                .version(Some(format!("ullage {}", meta::version())))
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

    let output_path = &args.flag_output.unwrap_or("a.out".to_string());
    let output_path = Path::new(&output_path);

    // Load the file into memory, so we can parse it into a syntax tree
    let source = read_input(args.arg_file).unwrap_or_else(|e| {
        eprintln!("error: could not read input: {}", e);
        exit(1)
    });

    // Parse the module
    let tree = parse::parse_tree(&source).unwrap_or_else(|e| {
        eprintln!("error: could not parse source: {}", e);
        exit(1)
    });

    // Are we just dumping the AST or compiling the whole thing?
    if args.flag_dumpast {
        println!("parsed AST: {:#?}", tree);
        exit(0);
    }

    let options = CompilationOptions::default()
        .with_dump_ir(args.flag_dumpir);
    let comp = match Compilation::new(tree, options) {
        Ok(c) => c,
        Err(e) => handle_comp_err(e),
    };

    // Create a compilation, and emit to the output path
    let emit_result = comp.emit(&output_path);

    // Print any failures encountered and return a failure status
    if let Err(e) = emit_result {
        handle_comp_err(e);
    }
}

/// Read the Compilation Input
///
/// If a file path was supplied then read the contents to a
/// `String`. If no file was provided then the input should be read
/// from standard input instead.
fn read_input(path: Option<String>) -> std::result::Result<String, Error> {
    let mut s = String::new();

    if let Some(path) = path {
        let input_path = Path::new(&path);
        File::open(&input_path)?.read_to_string(&mut s)?;
    } else {
        io::stdin().read_to_string(&mut s)?;
    }
    Ok(s)
}

/// Handles a Compilation Error
///
/// Prints the error to standard output and exits the process.
fn handle_comp_err(err: CompError) -> ! {
    eprintln!("error: compilation error: {}", err);
    exit(1);
}
