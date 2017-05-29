//! Contains the `TokenIterator` and `Parser` structs. These are
//! responsible for parsing a buffer into an expression tree.

pub mod error;
pub mod token_iterator;
mod string_source;

use std::iter::Peekable;

use super::ast::prelude::*;
use self::error::{Error, Result};
use self::token_iterator::TokenIterator;
use self::string_source::StringSource;

/// A Source of Input Characters
///
/// This trait abstracts over different types of input and provides a
/// cosnistent way for the token iterator to pull data from them.
pub trait Source<'a> {
    /// Source Position Type
    ///
    /// This type allows source implementations to abstract over
    /// indexes into their internal state.
    type Pos: Copy + Default;

    /// Take a Char from the Source
    ///
    /// Attempts to read a single character from the source at a given
    /// position. If there is a caracter at that position it is
    /// returned along with the position of the next character.
    fn take(&mut self, pos: Self::Pos) -> Option<(char, Self::Pos)>;

    /// Get a String Slice from the Source
    ///
    /// Covnerts a pair of source positions into a string slice of the
    /// characters between them. Character positions behave like
    /// cursors: the slice will contain all characters after the `s`
    /// position and before the `e` position.
    fn slice(&self, s: Self::Pos, e: Self::Pos) -> &'a str;
}

/// Parse expression from string. Takes a reference to an
/// expression and returns a result containing the parsed
/// expression, or an error if none could be parsed.
pub fn parse_str(s: &str) -> Result<Vec<Expression>> {
    let token_source = StringSource::new(s);
    let t = TokenIterator::new(token_source);
    let mut p = Parser::new(t);
    p.expressions()
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

/// Expression parser. Given a stream of tokens this will produce
/// an expression tree, or a parse error.
struct Parser<'a, T>
    where T: Iterator<Item = Token<'a>>
{
    lexer: Peekable<T>,
}

impl<'a, T> Parser<'a, T>
    where T: Iterator<Item = Token<'a>>
{
    /// Create a new Parser from a given token stream.
    pub fn new(t: T) -> Self {
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
    fn nud<T>(&self, parser: &mut Parser<'a, T>) -> Result<Expression>
        where T: Iterator<Item = Token<'a>>
    {
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
    fn led<T>(&self, parser: &mut Parser<'a, T>, lhs: Expression) -> Result<Expression>
        where T: Iterator<Item = Token<'a>>
    {
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
fn prefix_op<'a, T>(parser: &mut Parser<'a, T>, op: PrefixOp) -> Result<Expression>
    where T: Iterator<Item = Token<'a>>
{
    let rhs = try!(parser.expression(100));
    Ok(Expression::prefix(op, rhs))
}

/// Ternay Body
///
/// The condition and fallback part of a ternary expression.
fn ternary_body<'a, T>(parser: &mut Parser<'a, T>) -> Result<(Expression, Expression)>
    where T: Iterator<Item = Token<'a>>
{
    let condition = try!(parser.expression(0));
    try!(parser.expect(Token::Word("else")));
    let fallback = try!(parser.expression(0));
    Ok((condition, fallback))
}

#[cfg(test)]
mod test {

    use super::*;
    use super::super::ast::expression::Expression::*;

    macro_rules! check_parse {
        ($src:expr, $expected:expr) => {
            let src = $src;
            assert_eq!(Ok(vec!{$expected}), parse_str(&src));
        }
    }

    #[test]
    fn create_token_iter_from_str_returns_token_iter() {
        let src = StringSource::new("hello = world");
        TokenIterator::new(src);
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
