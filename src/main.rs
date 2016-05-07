mod expression;
mod tokeniser;

mod parser {

    use std::iter::Peekable;
    use super::expression::*;
    use super::tokeniser::{Tokeniser, Token};

    /// Parser Result
    pub type Result<T> = ::std::result::Result<T, Error>;

    trait InfixParselet {
        fn parse(&self, parser: &mut Parser, lhs: Expression) -> Result<Expression>;

        fn binding_power(&self) -> i32;
    }

    /// Binary Operator Parselet
    ///
    /// Represents the state required to parse an infix operator. This
    /// is a parsing function and a binding precedence.
    struct BinOpParselet {
        binding_power: i32,
        op: Operator,
    }

    impl BinOpParselet {
        fn new(binding_power: i32, op: Operator) -> Self {
            BinOpParselet {
                binding_power: binding_power,
                op: op,
            }
        }
    }

    impl InfixParselet for BinOpParselet {
        fn parse(&self, parser: &mut Parser, lhs: Expression) -> Result<Expression> {

            let rhs = try!(parser.parse(self.binding_power));
            Ok(Expression::from_binary_op(lhs, self.op, rhs))
        }

        fn binding_power(&self) -> i32 {
            self.binding_power
        }
    }

    /// Parse Error
    #[derive(Debug)]
    enum Error {
        /// Incomplete
        ///
        /// There weren't enough tokens to complete parsing the
        /// expression.
        Incomplete,

        /// Unexpected Token
        ///
        /// Signals that there was a token in the token stream which
        /// couldn't be parsed in that position. E.g. when we are
        /// expecting a closnig brace but get another token
        /// instead. Could also be that we were expecting a token but
        /// didn't get one either, hence the Option<Token>`
        Unexpected {
            found: Option<Token>,
            expecting: String,
        },
    }

    fn get_infix_parselet(token: Option<&Token>) -> Option<BinOpParselet> {
        if let Some(&Token::Operator(op)) = token {
            match op {
                Operator::Assign => Some(BinOpParselet::new(1, op)),
                Operator::Add | Operator::Sub => Some(BinOpParselet::new(2, op)),
                Operator::Mul | Operator::Div => Some(BinOpParselet::new(3, op)),
            }
        } else {
            None
        }
    }

    /// Parser
    ///
    /// Represents the state required when parsing an expression
    struct Parser {
        ts: Peekable<Tokeniser>,
    }

    impl Parser {
        /// Create a New Parser
        ///
        /// Initialises a new parser from a given token stream.
        pub fn new(tokens: Tokeniser) -> Parser {
            Parser { ts: tokens.peekable() }
        }

        /// Parse
        ///
        /// Parses an expression from the input and returns it.
        pub fn parse(&mut self, binding_power: i32) -> Result<Expression> {
            let mut lhs = try!(self.parse_prefix_expression());

            let mut maybe_parselet = get_infix_parselet(self.ts.peek());

            while let Some(parselet) = maybe_parselet {
                if binding_power >= parselet.binding_power {
                    break;
                }

                // advance the token stream past the token we used to
                // find the infix parselet.
                self.ts.next();

                lhs = try!(parselet.parse(self, lhs));

                maybe_parselet = get_infix_parselet(self.ts.peek());
            }

            Ok(lhs)
        }

        fn parse_prefix_expression(&mut self) -> Result<Expression> {

            if let Some(token) = self.ts.next() {
                match token {
                    Token::Identifier(id) => Ok(Expression::from_ident(&id)),
                    Token::Number(n) => Ok(Expression::from_value(n)),
                    Token::Operator(op) => {
                        let suffix = try!(self.parse(100));
                        Ok(Expression::from_prefix_op(op, suffix))
                    }
                    Token::OpenBracket => {
                        let expr = try!(self.parse(0));
                        match self.ts.next() {
                            Some(Token::CloseBracket) => Ok(expr),
                            t => {
                                Err(Error::Unexpected {
                                    expecting: "Closing bracket".to_string(),
                                    found: t,
                                })
                            }
                        }
                    }
                    t => {
                        Err(Error::Unexpected {
                            expecting: "Identifier, number or '('".to_string(),
                            found: Some(t),
                        })
                    }
                }
            } else {
                Err(Error::Incomplete)
            }
        }
    }

    pub fn parse_str(input: &str) -> Result<Expression> {
        Parser::new(Tokeniser::new(input)).parse(0)
    }
}

use parser::*;

fn main() {
    use std::io;
    use std::io::prelude::*;

    let stdin = io::stdin();
    for line_io in stdin.lock().lines() {
        if let Ok(line) = line_io {
            let parsed = parser::parse_str(&line).unwrap();
            println!("{}", parsed);
        };
    }
}
