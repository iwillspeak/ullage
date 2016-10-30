//! Expression tree parsing using Top-Down Operator Precedence
//! parsing.

pub mod syntax;
pub mod eval;

pub mod meta {
    pub const VERSION: Option<&'static str> = option_env!("CARGO_PKG_VERSION");
}

#[cfg(not(test))]
fn main() {

    use std::io;
    use std::io::prelude::*;
    use std::process::*;

    use syntax::*;
    use eval::*;

    fn prompt(c: char) {
        print!("{0}{0}{0} ", c);
        io::stdout().flush().unwrap();
    }

    let mut failures = 0;

    println!("ullage ({})", meta::VERSION.unwrap_or("unknown"));
    prompt('>');

    let quit_expr = vec![Expression::call(Expression::identifier("quit".to_string()), vec![])];

    let mut eval = eval::jit::JitEvaluator::new();
    let mut buffered = String::new();
    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            buffered.push_str(&line);
            buffered.push('\n');
            match Expression::parse_str(&buffered) {
                Ok(ref expr) if expr == &quit_expr => break,
                Ok(parsed) => {
                    buffered.clear();
                    println!("OK > {:?}", parsed);
                    let expr = Expression::sequence(parsed);
                    println!("=> {:?}", eval.eval(expr));
                    prompt('>');
                }
                Err(parse::Error::Incomplete) => {
                    prompt('.');
                }
                Err(err) => {
                    buffered.clear();
                    failures += 1;
                    println!("Error: {:?} ({})", err, buffered);
                    prompt('>');
                }
            };
        };
    }

    exit(failures);
}
