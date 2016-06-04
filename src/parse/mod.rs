pub mod tokeniser;

use std::iter::Peekable;

use super::{Expression, Operator};
use self::tokeniser::{Tokeniser, Token};

/// Parser Result
pub type Result<T> = ::std::result::Result<T, Error>;

/// Parse Error
#[derive(Debug)]
pub enum Error {
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

/// Infix Parselet Data
///
/// The data required to control the parsing of a given type of
/// infix parselet.
enum InfixParseletData {

    /// Binary Operator
    BinOp(Operator),

    /// Function Call
    FnCall
}

/// Infix Parselet
///
/// An infix parselet is an object capable of parsing the trailing
/// part of a given expression. It contains a binding power which
/// expresses associativity and parse data which controls the way
/// the expresison is parsed.
///
/// This parselet consumes operators and expressions which follow
/// a primary expression. This can be a chain of operators at the
/// same precedence level, nested expressions at a lower
/// precedence level, or a more complex expression such as a
/// function call or index expression.
struct InfixParselet {

    /// Binding Power
    ///
    /// How tightly the parselet associates itself with the
    /// expression to the left.
    pub binding_power: i32,

    /// Parse State
    ///
    /// Private state for use by the parselet implementaiton
    parse_state: InfixParseletData
}

impl InfixParselet {

    /// Parse
    ///
    /// Attempt to parse an expression given the current parser
    /// state and expression. This should only be called when the
    /// next token is known to be the first token of this infix
    /// parselet.
    ///
    /// The resulting expression should contain `lhs` wrapped in
    /// one or more operators.
    pub fn parse(&self, parser: &mut Parser, lhs: Expression) -> Result<Expression> {
        match self.parse_state {
            InfixParseletData::BinOp(op) => {
                let rhs = try!(parser.parse(self.binding_power));
                Ok(Expression::from_binary_op(lhs, op, rhs))
            }
            InfixParseletData::FnCall => {
                // collect up the parameters into a vector
                let mut params = Vec::new();
                loop {
                    match parser.parse(0) {
                        Ok(param) => params.push(param),
                        Err(Error::Incomplete) => return Err(Error::Incomplete),
                        _ => break
                    }
                }
                // call should be terminated by a closing bracket
                try!(parser.expect(Token::CloseBracket));
                Ok(Expression::from_function_call(lhs, params))
            }
        }
    }

    /// Bin Op
    ///
    /// Creates a new InfixParselet which will consume a given
    /// binary operator at a given precendence level.
    pub fn bin_op(binding_power: i32, op: Operator) -> Self {
        InfixParselet {
            binding_power: binding_power,
            parse_state: InfixParseletData::BinOp(op),
        }
    }
    
    /// Function Call
    ///
    /// Creates a new InfixParseet which will consume a function
    /// call expression.
    pub fn fn_call() -> Self {
        InfixParselet {
            binding_power: 8,
            parse_state: InfixParseletData::FnCall
        }
    }
}

/// Get Infix Parselet
///
/// Looks up an infix parselet, if one exists, for a given token.
fn get_infix_parselet(token: Option<&Token>) -> Option<InfixParselet> {
    match token {
        Some(&Token::Operator(op)) => Some(parser_for_op(op)),
        Some(&Token::OpenBracket) => Some(InfixParselet::fn_call()),
        _ => None,
    }
}

/// Get Parser for Operator
///
/// Given an operator returns an infix parselet instance capable
/// of parsing that operator.
fn parser_for_op(op: Operator) -> InfixParselet
{
    match op {
        Operator::Assign => InfixParselet::bin_op(1, op),
        Operator::Add | Operator::Sub => InfixParselet::bin_op(2, op),
        Operator::Mul | Operator::Div => InfixParselet::bin_op(3, op),
    }
}

/// Parser
///
/// Represents the state required when parsing an expression
pub struct Parser {
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

    fn expect(&mut self, expected: Token) -> Result<Token> {
        let expecting = format!("{:?}", expected);
        self.ts.next()
            .map_or(
                Err(Error::Unexpected {
                    expecting: expecting.to_string(),
                    found: None
                }),
                |t| {
                    if t == expected {
                        Ok(t)
                    } else {
                        Err(Error::Unexpected{
                            expecting: expecting.to_string(),
                            found: Some(t),
                        })
                    }
                })
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
                    try!(self.expect(Token::CloseBracket));
                    Ok(expr)
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
