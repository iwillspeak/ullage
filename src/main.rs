//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

#![warn(missing_docs)]

extern crate docopt;
#[macro_use]
extern crate serde_derive;
extern crate tempfile;

pub mod compile;
pub mod low_loader;
pub mod meta;
pub mod sem;
pub mod syntax;

use compile::*;
use docopt::Docopt;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::process::*;
use syntax::*;

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
  -h, --help          Show this message.
  --version           Show version.
  -o, --output=<out>  Write the output to <out>.

  --dumpir            Dump the LLVM IR for the module.
  --dumpast           Dump the syntax tree to stdout and exit.
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

    let output_path = &args.flag_output.unwrap_or("a.out".to_string());
    let output_path = Path::new(&output_path);

    // Load the file into memory, so we can parse it into a syntax tree
    let source = {
        let mut s = String::new();

        if let Some(path) = args.arg_file {
            let input_path = Path::new(&path);
            File::open(&input_path)
                .expect("error: could not open input file")
                .read_to_string(&mut s)
                .expect("error: could not read from file");
        } else {
            io::stdin()
                .read_to_string(&mut s)
                .expect("error: could not read from standard input");
        }
        s
    };

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

    let comp = match Compilation::new(tree) {
        Ok(c) => c,
        Err(e) => handle_comp_err(e),
    };

    // Create a compilation, and emit to the output path
    let emit_result = comp.emit(&output_path, args.flag_dumpir);

    // Print any failures encountered and return a failure status
    if let Err(e) = emit_result {
        handle_comp_err(e);
    }
}

/// Handles a Compilation Error
///
/// Prints the error to standard output and exits the process.
fn handle_comp_err(err: Error) -> ! {
    eprintln!("error: compilation error: {}", err);
    exit(1);
}
