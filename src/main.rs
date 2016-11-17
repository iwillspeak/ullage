//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

extern crate docopt;
extern crate rustc_serialize;

pub mod syntax;
pub mod eval;

mod meta {

    /// Version Number
    ///
    /// The version number of the crate (as known by Cargo) as a
    /// string. If the exe wasn't built by Cargo then this will be
    /// empty.
    pub const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
}

use std::io;
use std::io::prelude::*;
use std::process::*;
use docopt::Docopt;

use syntax::*;
use eval::*;

/// Usage Information
///
/// This is a [Docopt] compliant usage description of this program.
///
///  [Docopt]: http://docopt.org/
const USAGE: &'static str = "
Ullage Compiler

Usage:
  ullage [options] -e <expression>
  ullage [options] [repl]

Options:
  -h --help                 Show this screen.
  --version                 Show version.
  -v, --verbose             Be verbose in Logging.
  -e <expr>, --eval <expr>  Evaluate <expr> and then exit.
";

/// Program Arguments
///
/// Structure to capture the command line arguments for the
/// program. This is filled in for us by Docopt.
#[derive(Debug, RustcDecodable)]
struct Args {
    flag_help: bool,
    flag_version: bool,
    flag_verbose: bool,
    flag_eval: Option<String>,
    cmd_repl: bool,
}

/// Prompt for Input
///
/// Write a simple prompt to stdout. The prompt consists of three
/// repreitions of a given character.
///
/// # Arguments
///
///  * `c` - The character to base the prompt on.
fn prompt(c: char) {
    print!("{0}{0}{0} ", c);
    io::stdout().flush().unwrap();
}

/// Evaluate a String
///
/// Takes a given input string and JIT; parses the input and writes
/// the output to stdout.
///
/// # Arguments
///
///  * `eval` - The evaluator instance to use.
///  * `line` - The input line to parse and evaluate.
///  * `verbose` - Print out extra information about the parse result.
fn evaluate(eval: &mut Evaluator, line: &str, verbose: bool) -> Result<i32,()> {
    match Expression::parse_str(line) {
        Ok(parsed) => {
            if verbose {
                println!("OK > {:?}", parsed);
            }
            let expr = Expression::sequence(parsed);
            println!("=> {:?}", eval.eval(expr));
            Ok(0)
        }
        Err(parse::Error::Incomplete) => Err(()),
        Err(err) => {
            println!("Error: {:?} ({})", err, line);
            Ok(1)
        }
    }
}

/// Run the Read, Eval, Print Loop
///
/// Reads lines from standard input and reapeatedly evaluates them
/// until an exit expression is encountered or standard input is
/// exhausted.
///
/// # Arguments
///
///  * `eval` - The evaulator instance to use
///  * `args` - The executable arguments
///
/// # Returns
///
/// The number of failures encountered during the REPL's execution.
fn run_repl(eval: &mut Evaluator, args: Args) -> i32 {
    let mut failures = 0;
    let stdin = io::stdin();
    let mut buffered = String::new();

    prompt('>');
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            buffered.push_str(&line);
            buffered.push('\n');
            match evaluate(eval, &buffered, args.flag_verbose) {
                Ok(fails) => {
                    failures += fails;
                    prompt('>')
                },
                Err(_) => prompt('.')
            };
        };
    }

    failures
}

/// Main
///
/// The main function for `ullage`. Parses the options and runs the
/// selected command.
fn main() {

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    if args.flag_version || args.flag_verbose {
        println!("ullage ({})", meta::VERSION.unwrap_or("unknown"));
        if args.flag_version {
            exit(0);
        }
    }

    let mut eval = eval::jit::JitEvaluator::new();

    exit(if args.flag_eval.is_some() {
        match evaluate(&mut eval, &args.flag_eval.unwrap(), args.flag_verbose) {
            Ok(fails) => fails,
            Err(_) => {
                let mut stderr = io::stderr();
                writeln!(&mut stderr, "Incomplete expression!").unwrap();
                1
            }
        }
    } else {
        run_repl(&mut eval, args)
    })
}
