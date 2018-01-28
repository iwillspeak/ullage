//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

extern crate docopt;
#[macro_use]
extern crate serde_derive;
extern crate tempdir;

pub mod syntax;
pub mod sem;
pub mod meta;
pub mod compile;
pub mod low_loader;
pub mod diag;

use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
use std::io::stderr;
use std::process::*;
use docopt::Docopt;
use syntax::*;
use compile::*;

/// Usage Information
///
/// This is a [Docopt] compliant usage description of this program.
///
///  [Docopt]: http://docopt.org/
const USAGE: &'static str = "
Ullage Compiler

Usage:
  ullage --version
  ullage [options] [-o <outfile>] <file>

Options:
  -h --help           Show this screen.
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
    flag_help: bool,
    flag_version: bool,
    flag_dumpast: bool,
    flag_output: Option<String>,
    flag_dumpir: bool,
    arg_file: String,
}

/// Main
///
/// The main function for `ullage`. Parses the options and runs the
/// selected command.
fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    if args.flag_version {
        println!("ullage ({})", meta::version());
        exit(0);
    }

    let input_path = Path::new(&args.arg_file);
    let output_path = &args.flag_output.unwrap_or("a.out".to_string());
    let output_path = Path::new(&output_path);

    // Load the file into memory, so we can parse it into a syntax tree
    let source = {
        let mut s = String::new();
        File::open(&input_path)
            .expect("error: could not open input file")
            .read_to_string(&mut s)
            .expect("error: could not read from file");
        s
    };

    // Parse the module
    let tree = parse::parse_tree(&source).expect("error: could not parse source");

    // Are we just dumping the AST or compiling the whole thing?
    if args.flag_dumpast {
        println!("parsed AST: {:?}", tree);
        exit(0);
    }

    // Create a compilation, and emit to the output path
    let emit_result = Compilation::new(tree).emit(&output_path, args.flag_dumpir);

    // Print any failures encountered and return a failure status
    if let Err(e) = emit_result {
        writeln!(&mut stderr(), "error: compilation error: {}", e).unwrap();
        exit(1)
    }
}
