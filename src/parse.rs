//! Contains the `Tokeniser` and `Parser` structs. These are
//! responsible for parsing a buffer into an expression tree.

use std::iter::Peekable;

use super::{Expression, TypeReference, PrefixOp, InfixOp};

pub use self::error::{Error, Result};

impl Expression {

    /// Parse expression from string. Takes a reference to an
    /// expression and returns a result containing the parsed
    /// expression, or an error if none could be parsed.
    pub fn parse_str(s: &str) -> Result<Vec<Expression>> {
        let t = Tokeniser::new_from_str(s);
        let mut p = Parser::new(t);
        p.expressions()
    }
}

/// This structure represents a single token from the input source
/// buffer.
#[derive(Debug,PartialEq)]
pub enum Token<'a> {
    /// Represents a string of alphabetic characters. This could be a
    /// language keyword or a variable or type identifier.
    Word(&'a str),

    /// Whitespace trivia
    Whitespace(&'a str),

    /// Constant numerical value.
    Literal(i64),

    /// The `=` character
    Equals,

    /// The `+` character
    Plus,

    /// The `-` character
    Minus,

    /// The `*` character
    Star,

    /// The `/` character
    Slash,

    /// The `(` character
    OpenBracket,

    /// The `)` character
    CloseBracket,

    /// The `[` character
    OpenSqBracket,

    /// The `]` character
    CloseSqBracket,

    /// The `,` character
    Comma,

    /// The `:` character
    Colon,

    /// An unrecognised token
    Unknown(char),
}

impl InfixOp {
    /// Get infix operator from token
    fn for_token(tok: &Token) -> Option<Self> {
        use InfixOp::*;
        use self::Token::*;
        match *tok {
            Equals => Some(Assign),
            Star => Some(Mul),
            Slash => Some(Div),
            Plus => Some(Add),
            Minus => Some(Sub),
            _ => None,
        }
    }
}

/// Tokeniser
///
/// An object which can run a regex state machine over an input
/// buffer, producing tokens when each new lexeme is matched.
struct Tokeniser<'a> {
    buff: &'a str,
    idx: usize,
}

impl<'a> Tokeniser<'a> {

    /// Creates a new tokeniser from the given string slice.
    pub fn new_from_str(source: &'a str) -> Tokeniser {
        Tokeniser {
            buff: source,
            idx: 0,
        }
    }

    /// Retrieve the next 'raw' token. This is the next lexical match
    /// in the buffer, and may include whitespace and other trivia
    /// tokens.
    fn next_raw(&mut self) -> Option<Token<'a>> {

        let ts = self.idx;
        let mut te = ts;
        let mut chars = self.buff[ts..].chars();
        let tok = chars.next().and_then(|c| {
            te += c.len_utf8();
            match c {
                '=' => Some(Token::Equals),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Star),
                '/' => Some(Token::Slash),
                '(' => Some(Token::OpenBracket),
                ')' => Some(Token::CloseBracket),
                '[' => Some(Token::OpenSqBracket),
                ']' => Some(Token::CloseSqBracket),
                ',' => Some(Token::Comma),
                ':' => Some(Token::Colon),
                '#' => {
                    te += chars.take_while(|c| *c != '\n')
                        .fold(0, |l, c| l + c.len_utf8());
                    Some(Token::Whitespace(&self.buff[ts..te]))
                }
                '0'...'9' => {
                    te += chars.take_while(|c| *c >= '0' && *c <= '9').count();
                    let token_str = &self.buff[ts..te];
                    // we have cheked that it's a valid numeric literal,
                    // so unwrap is fine here.
                    Some(Token::Literal(token_str.parse::<i64>().unwrap()))
                }
                c if c.is_alphabetic() || c == '_' => {
                    te += chars.take_while(|c| c.is_alphanumeric() || *c == '_')
                        .fold(0, |l, c| l + c.len_utf8());
                    Some(Token::Word(&self.buff[ts..te]))
                }
                c if c.is_whitespace() => {
                    te += chars.take_while(|c| c.is_whitespace())
                        .fold(0, |l, c| l + c.len_utf8());
                    Some(Token::Whitespace(&self.buff[ts..te]))
                }
                _ => Some(Token::Unknown(c)),
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
impl<'a> Iterator for Tokeniser<'a> {
    type Item = Token<'a>;

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
struct Parser<'a> {
    lexer: Peekable<Tokeniser<'a>>,
}

impl<'a> Parser<'a> {

    /// Create a new Parser from a given token stream.
    pub fn new(t: Tokeniser<'a>) -> Self {
        Parser { lexer: t.peekable() }
    }

    /// Moves the token stream on by a single token
    pub fn advance(&mut self) -> Result<()> {
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
    pub fn expect(&mut self, expected: Token) -> Result<()> {
        if !self.next_is(expected) {
            return Err(Error::Unexpected);
        }
        self.advance()
    }

    /// Checks that the next token is of the given type
    pub fn next_is(&mut self, expected: Token) -> bool {
        self.lexer.peek().map_or(false, |token| {
            token == &expected
        })
    }

    /// Attempt to parse a single expression
    pub fn expression(&mut self, rbp: u32) -> Result<Expression> {
        let mut left = try!(self.parse_nud());
        while self.next_binds_tighter_than(rbp) {
            left = try!(self.parse_led(left));
        }
        Ok(left)
    }

    /// Atttempt to parse a list of expressions
    pub fn expressions(&mut self) -> Result<Vec<Expression>> {
        let mut expressions = Vec::new();
        while self.lexer.peek().is_some() {
            expressions.push(try!(self.expression(0)));
        }
        Ok(expressions)
    }

    /// Attempt to parse a type reference, this is a single
    /// `:` followed by a type name.
    pub fn type_ref(&mut self) -> Result<TypeReference> {
        try!(self.expect(Token::Colon));
        match self.lexer.peek(){
            Some(&Token::Word(t)) => {
                try!(self.advance());
                Ok(TypeReference(t.to_string()))
            }
            _ => Err(Error::Unexpected),
        }
    }

    /// Attempt to parse a block of expressions
    pub fn block(&mut self) -> Result<Vec<Expression>> {
        let mut expressions = Vec::new();
        while self.lexer.peek().is_some() &&
            !self.next_is(Token::Word("end")) {
            expressions.push(try!(self.expression(0)));
        }
        Ok(expressions)
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

impl<'a> Token<'a> {
    /// Left binding power. This controls the precedence of
    /// the symbol when being parsed as an infix operator.
    ///
    /// Returns the associativity, or binding power, for the given
    /// token. This is used when deciding if to parse the `led()`
    /// of this token.
    fn lbp(&self) -> u32 {
        match *self {
            Token::Equals => 10,

            // ternary if
            Token::Word("if") | Token::Word("unless") => 20,

            Token::Plus | Token::Minus => 50,

            Token::Star | Token::Slash => 60,

            Token::OpenBracket | Token::OpenSqBracket => 80,

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
            Token::Word("fn") => {
                let identifier = try!(parser.expression(100));
                try!(parser.expect(Token::OpenBracket));
                try!(parser.expect(Token::CloseBracket));
                let typ = try!(parser.type_ref());
                let body = try!(parser.block());
                try!(parser.expect(Token::Word("end")));
                Ok(Expression::Function(
                    Box::new(identifier),
                    typ,
                    Vec::new(),
                    body))
            }
            Token::Word(word) => Ok(Expression::Identifier(word.to_string())),
            Token::Literal(i) => Ok(Expression::Literal(i)),
            Token::Plus => parser.expression(100),
            Token::Minus => {
                let rhs = try!(parser.expression(100));
                Ok(Expression::Prefix(PrefixOp::Negate, Box::new(rhs)))
            }
            Token::OpenBracket => {
                let expr = try!(parser.expression(0));
                try!(parser.expect(Token::CloseBracket));
                Ok(expr)
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

            // array indexing
            Token::OpenSqBracket => {
                let index = try!(parser.expression(0));
                try!(parser.expect(Token::CloseSqBracket));
                Ok(Index(Box::new(lhs), Box::new(index)))
            }

            // Function call
            Token::OpenBracket => {
                let mut params = Vec::new();
                while !parser.next_is(Token::CloseBracket) {
                    let param = try!(parser.expression(0));
                    params.push(param);
                    if !parser.next_is(Token::CloseBracket) {
                        try!(parser.expect(Token::Comma));
                    }
                }
                try!(parser.expect(Token::CloseBracket));
                Ok(Expression::Call(Box::new(lhs), params))
            }

            // Ternay statement:
            // <x> if <y> else <z>
            Token::Word("if") => {
                let condition = try!(parser.expression(0));
                try!(parser.expect(Token::Word("else")));
                let fallback = try!(parser.expression(0));
                Ok(Expression::Ternary(Box::new(lhs), Box::new(condition), Box::new(fallback)))
            }

            // Ternay statement:
            // <x> unless <y> else <z>
            Token::Word("unless") => {
                let condition = try!(parser.expression(0));
                try!(parser.expect(Token::Word("else")));
                let fallback = try!(parser.expression(0));
                Ok(Expression::Ternary(Box::new(fallback), Box::new(condition), Box::new(lhs)))
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


    #[test]
    fn parse_groups_with_parens() {
        check_parse!("(1 + 2) * 3",
                     Infix(Box::new(Infix(Box::new(Literal(1)),
                                          InfixOp::Add,
                                          Box::new(Literal(2)))),
                           InfixOp::Mul,
                           Box::new(Literal(3))));
    }

    #[test]
    fn parse_indexing() {
        check_parse!("hello[world](1, 2[3])",
                     Call(Box::new(Index(Box::new(Identifier("hello".to_string())),
                                         Box::new(Identifier("world".to_string())))),
                          vec![Literal(1), Index(Box::new(Literal(2)), Box::new(Literal(3)))]));
    }

    #[test]
    fn parse_ternary_if() {
        check_parse!("1 if 2 else 3",
                     Ternary(Box::new(Literal(1)),
                             Box::new(Literal(2)),
                             Box::new(Literal(3))));
        check_parse!("hello(1) if foo[23] else world[1 if foo else 2]",
        Ternary(
            Box::new(Call(
                Box::new(Identifier("hello".to_string())),
                vec![Literal(1)])),
            Box::new(Index(
                Box::new(Identifier("foo".to_string())),
                Box::new(Literal(23)))),
            Box::new(Index(
                Box::new(Identifier("world".to_string())),
                Box::new(Ternary(Box::new(Literal(1)),
                        Box::new(Identifier("foo".to_string())),
                        Box::new(Literal(2))))))));
        check_parse!("0 unless 1 else 2",
                     Ternary(Box::new(Literal(2)),
                             Box::new(Literal(1)),
                             Box::new(Literal(0))));
    }

    #[test]
    fn parse_unicode_identifiers() {
        check_parse!("  übåℝ * ßeåk  ",
                     Infix(Box::new(Identifier("übåℝ".to_string())),
                           InfixOp::Mul,
                           Box::new(Identifier("ßeåk".to_string()))));
    }

    #[test]
    fn parse_function_def() {
        check_parse!("fn test() :Num 100 end",
                     Function(Box::new(Identifier("test".to_string())),
                              TypeReference("Num".to_string()),
                              Vec::new(),
                              vec![Literal(100)]));

        check_parse!("fn ünécød3() :Num
                0 if 74 else 888
             end",
                     Function(Box::new(Identifier("ünécød3".to_string())),
                              TypeReference("Num".to_string()),
                              Vec::new(),
                              vec![Ternary(Box::new(Literal(0)),
                                           Box::new(Literal(74)),
                                           Box::new(Literal(888)))]));
    }
}
