mod parse;
mod token;
mod tokeniser;
mod expression;

pub use parse::{Parser,Error as ParseError,Result as ParseResult};
pub use token::Token;
pub use tokeniser::Tokeniser;
pub use expression::{Expression,Operator};

#[cfg(not(test))]
fn main() {
    use std::io;
    use std::io::prelude::*;

    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            match Expression::parse(&line) {
                Ok(parsed) => println!("OK > {}", parsed),
                Err(err) => println!("Error: {:?}", err)
            };
        };
    }
}
