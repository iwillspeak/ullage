//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

extern crate docopt;
extern crate rustc_serialize;

pub mod syntax;
pub mod meta;
pub mod compile;
pub mod low_loader;

use std::fs::File;
use std::path::Path;
use std::io::prelude::*;
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
  ullage [options] <file>

Options:
  -h --help  Show this screen.
  --version  Show version.

  --dumpast  Dump the syntax tree to stdout and exit.
";

/// Program Arguments
///
/// Structure to capture the command line arguments for the
/// program. This is filled in for us by Docopt.
#[derive(Debug, RustcDecodable)]
struct Args {
    flag_help: bool,
    flag_version: bool,
    flag_dumpast: bool,
    arg_file: String,
}

/// Main
///
/// The main function for `ullage`. Parses the options and runs the
/// selected command.
fn main() {

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    if args.flag_version {
        println!("ullage ({})", meta::version());
        exit(0);
    }

    let input_path = Path::new(&args.arg_file);

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
    let tree = Expression::parse_str(&source)
        .expect("error: could not parse source");

    // Are we just dumping the AST or compiling the whole thing?
    if args.flag_dumpast {
        println!("parsed AST: {:?}", tree);
        exit(0);
    }

    // Create a compilation, and dump the results to stdout
    Compilation::new(tree)
        .emit()
        .expect("error: compilation error")
        .dump();
}
