mod expression;
mod tokeniser;

mod parser {

    use super::expression::*;
    use super::tokeniser::{Tokeniser,Token};

    /// Parser Result
    pub type Result<T> = ::std::result::Result<T, Error>;

    /// Parse Error
    #[derive(Debug)]
    enum Error {
        /// Incomplete
        ///
        /// There weren't enough tokens to complete parsing the
        /// expression.
        Incomplete,
    }

    /// Parser
    ///
    /// Represents the state required when parsing an expression
    struct Parser {
        ts: Tokeniser,
    }

    impl Parser {

        /// Create a New Parser
        ///
        /// Initialises a new parser from a given token stream.
        pub fn new(tokens: Tokeniser) -> Parser {
            Parser {
                ts: tokens
            }
        }

        /// Parse
        ///
        /// Parses an expression from the input and returns it.
        pub fn parse<'a, 'b>(&'a mut self) -> Result<Expression<'b>> {
            match self.ts.next() {
                Some(Token::Number(value)) =>
                    Ok(Expression::ValueExpression(value)),
                _ => Err(Error::Incomplete)
            }
        }
    }

    pub fn parse_str(input: &str) -> Result<Expression> {
        Parser::new(Tokeniser::new(input)).parse()
    }
}

use parser::*;

fn main() {
    use std::io;
    use std::io::prelude::*;

    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            let parsed = parser::parse_str(&line);
            println!("{:?}", parsed);
        };
    }
}
