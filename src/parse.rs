//! Contains the `Tokeniser` and `Parser` structs. These are
//! responsible for parsing a buffer into an expression tree.

use std::iter::Peekable;

use super::{Token, Expression, PrefixOp, InfixOp};

pub use self::error::{Error, Result};

impl Expression {

    /// Parse expression from string. Takes a reference to an
    /// expression and returns a result containing the parsed
    /// expression, or an error if none could be parsed.
    pub fn parse_str(s: &str) -> Result<Expression> {
        let t = Tokeniser::new_from_str(s);
        let mut p = Parser::new(t);
        p.expression(0)
    }
}


/// Tokeniser
///
/// An object which can run a regex state machine over an input
/// buffer, producing tokens when each new lexeme is matched.
struct Tokeniser {
    buff: String,
    idx: usize,
}

impl Tokeniser {
    /// Creates a new tokeniser from the given string slice.
    pub fn new_from_str(source: &str) -> Tokeniser {
        Tokeniser {
            buff: source.to_string(),
            idx: 0,
        }
    }

    fn next_raw(&mut self) -> Option<Token> {

        let ts = self.idx;
        let mut te = ts;
        let mut chars = self.buff[ts..].chars();
        let tok = chars.next().and_then(|c| {
            te += 1;
            match c {
                '=' => Some(Token::Equals),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Star),
                '/' => Some(Token::Slash),
                '(' => Some(Token::OpenBracket),
                ')' => Some(Token::CloseBracket),
                ',' => Some(Token::Comma),
                '0'...'9' => {
                    te += chars.take_while(|c| *c >= '0' && *c <= '9').count();
                    let token_str = &self.buff[ts..te];
                    // we have cheked that it's a valid numeric literal,
                    // so unwrap is fine here.
                    Some(Token::Literal(token_str.parse::<i64>().unwrap()))
                }
                c if c.is_alphabetic() || c == '_' => {
                    te += chars.take_while(|c| c.is_alphanumeric() || *c == '_')
                               .count();
                    Some(Token::Word(self.buff[ts..te].to_string()))
                }
                c if c.is_whitespace() => {
                    te += chars.take_while(|c| c.is_whitespace())
                               .count();
                    Some(Token::Whitespace(self.buff[ts..te].to_string()))
                }
                _ => None,
            }
        });
        self.idx = te;
        tok
    }
}

/// Tokeniser Iterator implementation.
///
/// This allows iterator adapters to be used with the token
/// stream. The next method filters out whitespace tokens from the
/// tokeniser too. This means the `Tokeniser` doesn't have to worry
/// about skipping certain lexemes in the grammar.
impl Iterator for Tokeniser {
    type Item = Token;

    /// Iterator next method. This method returns the next
    /// non-whitespace token in the `Tokeniser`'s stream of `Token`s.
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(tok) = self.next_raw() {
            if let Token::Whitespace(_) = tok {
            } else {
                return Some(tok);
            }
        }
        None
    }
}

/// Expression parser. Given a stream of tokens this will produce
/// an expression tree, or a parse error.
struct Parser {
    lexer: Peekable<Tokeniser>,
}

impl Parser {
    pub fn new(t: Tokeniser) -> Parser {
        Parser { lexer: t.peekable() }
    }

    /// Moves the token stream on by a single token
    fn advance(&mut self) -> Result<()> {
        match self.lexer.peek() {
            Some(_) => {
                self.lexer.next();
                Ok(())
            }
            _ => Err(Error::Incomplete),
        }
    }

    /// Moves the token stream on by a single token, if the
    /// token's lexeme is of the given type.
    pub fn advance_if(&mut self, expected: Token) -> Result<()> {
        if !self.next_is(expected) {
            return Err(Error::Unexpected);
        }
        self.advance()
    }

    /// Checks that the next token is of the given type
    pub fn next_is(&mut self, expected: Token) -> bool {
        self.lexer.peek() == Some(&expected)
    }

    /// Attempt to parse a single expression
    pub fn expression(&mut self, rbp: u32) -> Result<Expression> {
        let mut left = try!(self.parse_nud());
        while self.next_binds_tighter_than(rbp) {
            left = try!(self.parse_led(left));
        }
        Ok(left)
    }

    /// Returns true if the next token's lbp is > the given rbp
    fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        match self.lexer.peek() {
            Some(ref token) => token.lbp() > rbp,
            None => false,
        }
    }

    /// Attempt to parse a single left denotation
    fn parse_led(&mut self, lhs: Expression) -> Result<Expression> {
        match self.lexer.next() {
            Some(token) => token.led(self, lhs),
            None => Err(Error::Incomplete),
        }
    }

    /// Attempt to parse a single null denotation
    fn parse_nud(&mut self) -> Result<Expression> {
        match self.lexer.next() {
            Some(token) => token.nud(self),
            None => Err(Error::Incomplete),
        }
    }
}

impl Token {

    /// Left binding power. This controls the precedence of
    /// the symbol when being parsed as an infix operator.
    ///
    /// Returns the associativity, or binding power, for the given
    /// token. This is used when deciding if to parse the `led()`
    /// of this token.
    fn lbp(&self) -> u32 {
        match *self {
            Token::Equals => 10,

            Token::Plus | Token::Minus => 50,

            Token::Star | Token::Slash => 60,

            Token::OpenBracket => 80,

            _ => 0,
        }
    }

    /// Null denotation. This is the parse of the symbol when it
    /// doesn't have any expression to the left hand side of it.
    ///
    /// This is responsible for parsing literals and variable
    /// references into expressions, as well as parsing prefix
    /// expressions
    fn nud(&self, parser: &mut Parser) -> Result<Expression> {
        match *self {
            Token::Word(ref word) => Ok(Expression::Identifier(word.clone())),
            Token::Literal(i) => Ok(Expression::Literal(i)),
            Token::Plus => parser.expression(100),
            Token::Minus => {
                let rhs = try!(parser.expression(100));
                Ok(Expression::Prefix(PrefixOp::Negate, Box::new(rhs)))
            }
            _ => Err(Error::Unexpected),
        }
    }

    /// Left denotation. This is the parse of the symbol when it
    /// has an expression to the left hand side of it.
    ///
    /// This is responsible for parsing infix operators and function
    /// calls.
    fn led(&self, parser: &mut Parser, lhs: Expression) -> Result<Expression> {
        use Expression::*;
        match *self {

            // Binary infix operator
            Token::Equals |
            Token::Plus |
            Token::Minus |
            Token::Star |
            Token::Slash => {
                let rhs = try!(parser.expression(self.lbp()));
                let op = InfixOp::for_token(self).unwrap();
                Ok(Infix(Box::new(lhs), op, Box::new(rhs)))
            }

            // Function call
            Token::OpenBracket => {
                let mut params = Vec::new();
                while !parser.next_is(Token::CloseBracket) {
                    let param = try!(parser.expression(0));
                    params.push(param);
                    if !parser.next_is(Token::CloseBracket) {
                        try!(parser.advance_if(Token::Comma));
                    }
                }
                try!(parser.advance_if(Token::CloseBracket));
                Ok(Expression::Call(Box::new(lhs), params))
            }

            _ => Err(Error::Unexpected),
        }
    }
}

/// Parse error module. Contains the Result and Error types for the
/// `parse` module.
pub mod error {

    /// Parser result type. Returned from parsing functions when
    /// success can't be guaranteed.
    pub type Result<T> = ::std::result::Result<T, Error>;

    /// Parser error type. This distinguishes between the differen
    /// kinds of errors that the `Parser` can encounter.
    #[derive(Debug,PartialEq)]
    pub enum Error {
        /// Unexpected token.
        Unexpected,

        /// Incomplete data
        Incomplete,
    }

}

#[cfg(test)]
mod test {
    use super::{Tokeniser, Parser};
    use super::super::*;
    use Expression::*;

    macro_rules! check_parse {
        ($src:expr, $expected:expr) => {
            let src:&str = $src;
            let tokeniser = Tokeniser::new_from_str(src);
            let mut parser = Parser::new(tokeniser);
            assert_eq!(Ok($expected), parser.expression(0));
        }
    }

    #[test]
    fn create_tokeniser_from_str_returns_tokeniser() {
        let src = "hello = world";
        Tokeniser::new_from_str(src);
    }

    #[test]
    fn parse_simple_string() {
        check_parse!("hello + 123",
                     Infix(Box::new(Identifier("hello".to_string())),
                           InfixOp::Add,
                           Box::new(Literal(123))));
    }

    #[test]
    fn parse_with_precedence() {
        check_parse!("1 + 2 * 3",
                     Infix(Box::new(Literal(1)),
                           InfixOp::Add,
                           Box::new(Infix(Box::new(Literal(2)),
                                          InfixOp::Mul,
                                          Box::new(Literal(3))))));
    }

    #[test]
    fn parse_prefix_expressions() {
        check_parse!("+1 * -2 + +3",
                     Infix(Box::new(Infix(Box::new(Literal(1)),
                                          InfixOp::Mul,
                                          Box::new(Prefix(PrefixOp::Negate,
                                                          Box::new(Literal(2)))))),
                           InfixOp::Add,
                           Box::new(Literal(3))));
    }

    #[test]
    fn parse_simple_call() {
        check_parse!("foo()",
                     Call(Box::new(Identifier("foo".to_string())), Vec::new()));
    }

    #[test]
    fn parse_complex_call() {
        check_parse!("hello(1, 1 + 23, -world)",
                     Call(Box::new(Identifier("hello".to_string())),
                          vec![Literal(1),
                               Infix(Box::new(Literal(1)), InfixOp::Add, Box::new(Literal(23))),
                               Prefix(PrefixOp::Negate,
                                      Box::new(Identifier("world".to_string())))]));

    }
}
