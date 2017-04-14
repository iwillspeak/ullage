//! Contains the `Tokeniser` and `Parser` structs. These are
//! responsible for parsing a buffer into an expression tree.

use std::iter::Peekable;

use super::ast::prelude::*;
pub use self::error::{Error, Result};

/// Parse expression from string. Takes a reference to an
/// expression and returns a result containing the parsed
/// expression, or an error if none could be parsed.
pub fn parse_str(s: &str) -> Result<Vec<Expression>> {
    let t = Tokeniser::new_from_str(s);
    let mut p = Parser::new(t);
    p.expressions()
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

    /// The `==` operator
    DoubleEquals,

    /// The `!` character
    Bang,

    /// The `!=` operator
    BangEquals,

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

    /// The `<` character
    LessThan,

    /// The `>` character
    MoreThan,

    /// An unrecognised token
    Unknown(char),
}

impl InfixOp {
    /// Get infix operator from token
    fn for_token(tok: &Token) -> Option<Self> {
        use super::ast::operators::InfixOp::*;
        use self::Token::*;
        match *tok {
            DoubleEquals => Some(Eq),
            Equals => Some(Assign),
            BangEquals => Some(NotEq),
            LessThan => Some(Lt),
            MoreThan => Some(Gt),
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
                '=' => {
                    match chars.next() {
                        Some('=') => {
                            te += 1;
                            Some(Token::DoubleEquals)
                        }
                        _ => Some(Token::Equals),
                    }
                }
                '!' => {
                    match chars.next() {
                        Some('=') => {
                            te += 1;
                            Some(Token::BangEquals)
                        }
                        _ => Some(Token::Bang),
                    }
                }
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
                '<' => Some(Token::LessThan),
                '>' => Some(Token::MoreThan),
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
        match self.lexer.peek() {
                Some(token) if token == &expected => Ok(()),
                Some(_) => Err(Error::Unexpected),
                None => Err(Error::Incomplete),
            }
            .map(|ok| {
                self.lexer.next();
                ok
            })
    }

    /// Checks that the next token is of the given type
    pub fn next_is(&mut self, expected: Token) -> bool {
        self.lexer.peek().map_or(false, |token| token == &expected)
    }

    /// Attempt to parse an identifier
    pub fn identifier(&mut self) -> Result<String> {
        match try!(self.expression(100)) {
            Expression::Identifier(string) => Ok(string),
            _ => Err(Error::Unexpected),
        }
    }

    /// Attempt to parse a single expressiond
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

    /// Parse Type Reference
    ///
    /// Attempt to parse a type reference, this is a single
    /// `:` followed by a type name.
    pub fn type_ref(&mut self) -> Result<TypeRef> {
        try!(self.expect(Token::Colon));
        self.ty()
    }

    /// Parse Type
    ///
    /// Attempt to parse a type. This could be a simple type name, or
    /// it could be a more complex one such as an array type or tuple.
    pub fn ty(&mut self) -> Result<TypeRef> {
        match self.lexer.peek() {
            Some(&Token::Word(t)) => {
                try!(self.advance());
                Ok(TypeRef::simple(t))
            }
            Some(&Token::OpenSqBracket) => {
                try!(self.advance());
                let inner = try!(self.ty());
                try!(self.expect(Token::CloseSqBracket));
                Ok(TypeRef::array(inner))
            }
            Some(&Token::OpenBracket) => {
                try!(self.advance());
                let mut types = Vec::new();
                if !self.next_is(Token::CloseBracket) {
                    types.push(try!(self.ty()));
                }
                while !self.next_is(Token::CloseBracket) {
                    try!(self.expect(Token::Comma));
                    types.push(try!(self.ty()));
                }
                try!(self.expect(Token::CloseBracket));
                Ok(TypeRef::tuple(types))
            }
            _ => Err(Error::Unexpected),
        }
    }

    /// Parse an optional type reference
    ///
    /// If there is a type refernece then parse it and return it. If
    /// there is no type we may have to infer it later.
    pub fn optional_type_ref(&mut self) -> Option<TypeRef> {
        if self.next_is(Token::Colon) {
            self.type_ref().ok()
        } else {
            None
        }
    }

    /// Parse an identifier, with an optional type
    pub fn typed_id(&mut self) -> Result<TypedId> {
        let id = try!(self.identifier());
        let typ = self.optional_type_ref();
        Ok(TypedId { id: id, typ: typ })
    }

    /// Attempt to parse a local declaration
    ///
    /// Parses the body of a local variable delcaration (`let` or
    /// `var`).
    pub fn declaration(&mut self, is_mut: bool) -> Result<Expression> {
        let id = try!(self.identifier());
        let typ = self.optional_type_ref();
        try!(self.expect(Token::Equals));
        let rhs = try!(self.expression(0));
        Ok(Expression::declaration(TypedId::from_parts(id.clone(), typ), is_mut, rhs))
    }

    /// Attempt to parse a block of expressions
    pub fn block(&mut self) -> Result<Vec<Expression>> {
        let mut expressions = Vec::new();
        while self.lexer.peek().is_some() && !self.next_is(Token::Word("end")) {
            expressions.push(try!(self.expression(0)));
        }
        Ok(expressions)
    }

    /// Returns true if the next token's lbp is > the given rbp
    fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.lexer.peek().map_or(false, |t| t.lbp() > rbp)
    }

    /// Attempt to parse a single left denotation
    fn parse_led(&mut self, lhs: Expression) -> Result<Expression> {
        self.lexer.next().map_or(Err(Error::Incomplete), |t| t.led(self, lhs))
    }

    /// Attempt to parse a single null denotation
    fn parse_nud(&mut self) -> Result<Expression> {
        self.lexer.next().map_or(Err(Error::Incomplete), |t| t.nud(self))
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
            Token::Word("if") |
            Token::Word("unless") => 20,

            // boolean conditional operators
            Token::DoubleEquals | Token::BangEquals | Token::LessThan | Token::MoreThan => 40,

            // Arithmetic operators
            Token::Plus | Token::Minus => 50,

            Token::Star | Token::Slash => 60,

            // Grouping operators
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
                let identifier = try!(parser.identifier());
                let mut res = Expression::function(identifier);
                try!(parser.expect(Token::OpenBracket));
                if !parser.next_is(Token::CloseBracket) {
                    res = res.with_arg(try!(parser.typed_id()));
                }
                while !parser.next_is(Token::CloseBracket) {
                    try!(parser.expect(Token::Comma));
                    res = res.with_arg(try!(parser.typed_id()));
                }
                try!(parser.expect(Token::CloseBracket));
                res = res.with_return_type(try!(parser.type_ref()))
                    .with_body(try!(parser.block()));
                try!(parser.expect(Token::Word("end")));
                Ok(Expression::from(res))
            }
            Token::Word("while") => {
                let condition = try!(parser.expression(0));
                let block = try!(parser.block());
                try!(parser.expect(Token::Word("end")));
                Ok(Expression::loop_while(condition, block))
            }
            Token::Word("let") => parser.declaration(false),
            Token::Word("var") => parser.declaration(true),
            Token::Word("print") => {
                let to_print = try!(parser.expression(0));
                Ok(Expression::print(to_print))
            }
            Token::Word("true") => Ok(Expression::constant_bool(true)),
            Token::Word("false") => Ok(Expression::constant_bool(false)),
            Token::Word(word) => Ok(Expression::identifier(String::from(word))),
            Token::Literal(i) => Ok(Expression::constant_num(i)),
            Token::Plus => parser.expression(100),
            Token::Minus => prefix_op(parser, PrefixOp::Negate),
            Token::Bang => prefix_op(parser, PrefixOp::Not),
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
        match *self {

            // Binary infix operator
            Token::DoubleEquals | Token::BangEquals | Token::LessThan | Token::MoreThan |
            Token::Equals | Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                let rhs = try!(parser.expression(self.lbp()));
                let op = InfixOp::for_token(self).unwrap();
                Ok(Expression::infix(lhs, op, rhs))
            }

            // array indexing
            Token::OpenSqBracket => {
                let index = try!(parser.expression(0));
                try!(parser.expect(Token::CloseSqBracket));
                Ok(Expression::index(lhs, index))
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
                Ok(Expression::call(lhs, params))
            }

            // Ternay statement:
            // <x> if <y> else <z>
            Token::Word("if") => {
                let (condition, fallback) = try!(ternary_body(parser));
                Ok(Expression::if_then_else(condition, lhs, fallback))
            }

            // Ternay statement:
            // <x> unless <y> else <z>
            Token::Word("unless") => {
                let (condition, fallback) = try!(ternary_body(parser));
                Ok(Expression::if_then_else(condition, fallback, lhs))
            }

            _ => Err(Error::Incomplete),
        }
    }
}

/// Prefix Operator
///
/// Parses the trailing expression for a prefix operator.
fn prefix_op(parser: &mut Parser, op: PrefixOp) -> Result<Expression> {
    let rhs = try!(parser.expression(100));
    Ok(Expression::prefix(op, rhs))
}

/// Ternay Body
///
/// The condition and fallback part of a ternary expression.
fn ternary_body(parser: &mut Parser) -> Result<(Expression, Expression)> {
    let condition = try!(parser.expression(0));
    try!(parser.expect(Token::Word("else")));
    let fallback = try!(parser.expression(0));
    Ok((condition, fallback))
}

/// Parse error module. Contains the Result and Error types for the
/// `parse` module.
pub mod error {

    /// Parser result type. Returned from parsing functions when
    /// success can't be guaranteed.
    pub type Result<T> = ::std::result::Result<T, Error>;

    /// Parser error type. This distinguishes between the different
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
    use super::super::ast::prelude::*;
    use super::super::ast::expression::Expression::*;

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
                     Expression::infix(Expression::identifier("hello".to_string()),
                                       InfixOp::Add,
                                       Expression::constant_num(123)));
    }

    #[test]
    fn parse_operators() {
        check_parse!("a = b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Assign,
                                       Expression::identifier("b".to_string())));
        check_parse!("a + b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Add,
                                       Expression::identifier("b".to_string())));
        check_parse!("a - b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Sub,
                                       Expression::identifier("b".to_string())));
        check_parse!("a * b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Mul,
                                       Expression::identifier("b".to_string())));
        check_parse!("a / b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Div,
                                       Expression::identifier("b".to_string())));
        check_parse!("a == b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Eq,
                                       Expression::identifier("b".to_string())));
        check_parse!("a != b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::NotEq,
                                       Expression::identifier("b".to_string())));
        check_parse!("a < b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Lt,
                                       Expression::identifier("b".to_string())));
        check_parse!("a > b",
                     Expression::infix(Expression::identifier("a".to_string()),
                                       InfixOp::Gt,
                                       Expression::identifier("b".to_string())));
    }

    #[test]
    fn parse_with_precedence() {
        check_parse!("1 + 2 * 3",
                     Expression::infix(Expression::constant_num(1),
                                       InfixOp::Add,
                                       Expression::infix(Expression::constant_num(2),
                                                         InfixOp::Mul,
                                                         Expression::constant_num(3))));
    }

    #[test]
    fn parse_prefix_expressions() {
        check_parse!("+1 * -2 + +3",
                     Expression::infix(Expression::infix(Expression::constant_num(1),
                                          InfixOp::Mul,
                                          Expression::prefix(PrefixOp::Negate,
                                                                      Expression::constant_num(2))),
                           InfixOp::Add,
                                       Expression::constant_num(3)));
        check_parse!("!a",
                     Expression::prefix(PrefixOp::Not, Expression::identifier("a".to_string())));
        check_parse!("!a != !b",
                     Expression::infix(
                         Expression::prefix(
                             PrefixOp::Not,
                             Expression::identifier("a".to_string())),
                         InfixOp::NotEq,
                         Expression::prefix(
                             PrefixOp::Not,
                             Expression::identifier("b".to_string()))));
    }

    #[test]
    fn parse_simple_call() {
        check_parse!("foo()",
                     Expression::call(Expression::identifier("foo".to_string()), Vec::new()));
    }

    #[test]
    fn parse_complex_call() {
        check_parse!("hello(1, 1 + 23, -world)",
                     Expression::call(Expression::identifier("hello".to_string()),
                                      vec![Expression::constant_num(1),
                                           Expression::infix(Expression::constant_num(1),
                                                             InfixOp::Add,
                                                             Expression::constant_num(23)),
                                           Expression::prefix(PrefixOp::Negate,
                                                              Expression::identifier("world"
                                                                  .to_string()))]));
    }


    #[test]
    fn parse_groups_with_parens() {
        check_parse!("(1 + 2) * 3",
                     Infix(Box::new(Infix(Box::new(Expression::constant_num(1)),
                                          InfixOp::Add,
                                          Box::new(Expression::constant_num(2)))),
                           InfixOp::Mul,
                           Box::new(Expression::constant_num(3))));
    }

    #[test]
    fn parse_indexing() {
        check_parse!("hello[world](1, 2[3])",
                     Expression::call(Expression::index(Expression::identifier("hello"
                                                            .to_string()),
                                                        Expression::identifier("world"
                                                            .to_string())),
                                      vec![Expression::constant_num(1),
                                           Expression::index(Expression::constant_num(2),
                                                             Expression::constant_num(3))]));
    }

    #[test]
    fn parse_ternary_if() {
        check_parse!("1 if 2 else 3",
                     Expression::if_then_else(Expression::constant_num(2),
                                              Expression::constant_num(1),
                                              Expression::constant_num(3)));
        check_parse!("hello(1) if foo[23] else world[1 if foo else 2]",
                     Expression::if_then_else(
                         Expression::index(
                             Expression::identifier("foo".to_string()),
                             Expression::constant_num(23)),
                         Expression::call(
                             Expression::identifier("hello".to_string()),
                             vec![Expression::constant_num(1)]),
                         Expression::index(
                             Expression::identifier("world".to_string()),
                             Expression::if_then_else(
                                 Expression::identifier("foo".to_string()),
                                 Expression::constant_num(1),
                                 Expression::constant_num(2)))));
        check_parse!("0 unless 1 else 2",
                     Expression::if_then_else(Expression::constant_num(1),
                                              Expression::constant_num(2),
                                              Expression::constant_num(0)));
    }

    #[test]
    fn parse_unicode_identifiers() {
        check_parse!("  übåℝ * ßeåk  ",
                     Expression::infix(Expression::identifier("übåℝ".to_string()),
                                       InfixOp::Mul,
                                       Expression::identifier("ßeåk".to_string())));
    }

    #[test]
    fn parse_function_def() {
        check_parse!("fn test() :Num 100 end",
                     Expression::function("test".to_string())
                         .with_return_type(TypeRef::simple("Num"))
                         .with_body(vec![Expression::constant_num(100)])
                         .into());
        check_parse!("fn ünécød3() :Num
                0 if 74 else 888
             end",
                     Expression::function("ünécød3".to_string())
                         .with_return_type(TypeRef::simple("Num"))
                         .with_body(vec![Expression::if_then_else(Expression::constant_num(74),
                                                                  Expression::constant_num(0),
                                                                  Expression::constant_num(888))])
                         .into());
    }

    #[test]
    fn parse_while_loop() {
        check_parse!("while 1 end",
                     Expression::loop_while(Expression::constant_num(1), Vec::new()));
        check_parse!("while 0 44 234 end",
                     Expression::loop_while(Expression::constant_num(0),
                                            vec![Expression::constant_num(44),
                                                 Expression::constant_num(234)]));
    }

    #[test]
    fn parse_function_with_args() {
        check_parse!("fn neg(i: Num): Num - i end",
                     Expression::function("neg".to_string())
                         .with_arg(TypedId::new("i".to_string(), TypeRef::simple("Num")))
                         .with_return_type(TypeRef::simple("Num"))
                         .with_body(vec![
                         Expression::prefix(
                             PrefixOp::Negate,
                             Expression::identifier("i".to_string()))])
                         .into());

        check_parse!("fn test(i: Num, j, k: String): String i + j + k end",
                     Expression::function("test".to_string())
                         .with_arg(TypedId::new("i".to_string(), TypeRef::simple("Num")))
                         .with_arg(TypedId::new_without_type("j".to_string()))
                         .with_arg(TypedId::new("k".to_string(), TypeRef::simple("String")))
                         .with_return_type(TypeRef::simple("String"))
                         .with_body(vec![
                         Expression::infix(
                             Expression::infix(
                                 Expression::identifier("i".to_string()),
                                 InfixOp::Add,
                                 Expression::identifier("j".to_string())),
                             InfixOp::Add,
                             Expression::identifier("k".to_string()))])
                         .into());
    }

    #[test]
    fn parse_simple_array_type() {
        check_parse!("let f: [Num] = 100",
                         Expression::declaration(
                             TypedId::from_parts(String::from("f"),
                                                 Some(TypeRef::array(TypeRef::simple("Num")))),
                             false,
                             Expression::constant_num(100)
                         ));
    }

    #[test]
    fn parse_simple_let() {
        check_parse!("let foo = 100",
                     Expression::declaration(TypedId::from_parts("foo".to_string(), None),
                                             false,
                                             Expression::constant_num(100)));
    }

    #[test]
    fn parse_simple_tuple() {
        check_parse!("let f: (Num) = 100",
                     Expression::declaration(
                         TypedId::from_parts(
                                 String::from("f"),
                             Some(TypeRef::tuple(vec![TypeRef::simple("Num")]))),
                         false,
                         Expression::constant_num(100)
                     ));
        check_parse!("let f: (Num, [String]) = 100",
                     Expression::declaration(TypedId::from_parts(String::from("f"),
                                                                 Some(TypeRef::tuple(vec![
                                     TypeRef::simple("Num"),
                                     TypeRef::array(TypeRef::simple("String"))
                                 ]))),
                                             false,
                                             Expression::constant_num(100)));
    }

    #[test]
    fn parse_variable_decl() {
        check_parse!("var foo = 93",
                     Expression::declaration(TypedId::from_parts(String::from("foo"), None),
                                             true,
                                             Expression::constant_num(93)));
        check_parse!("var foo_bar: Number = -99999",
                     Expression::declaration(TypedId::from_parts(String::from("foo_bar"),
                                                                 Some(TypeRef::simple("Number"))),
                                             true,
                                             Expression::prefix(PrefixOp::Negate,
                                                                Expression::constant_num(99999))));

    }

    #[test]
    fn parse_print_operator() {
        check_parse!("print 1334",
                     Expression::print(Expression::constant_num(1334)));
    }

    #[test]
    fn parse_bool_literal() {
        check_parse!("true", Expression::constant_bool(true));
        check_parse!("false", Expression::constant_bool(false));
    }
}
