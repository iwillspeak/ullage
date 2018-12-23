//! Contains the `Tokeniser` and `Parser` structs. These are
//! responsible for parsing a buffer into an expression tree.

pub mod error;
mod token;
mod tokeniser;
mod trivia_filter;

pub use self::error::{ParseError, ParseResult};
use self::token::{Literal, Token};
use self::tokeniser::Tokeniser;
use self::trivia_filter::*;

use super::text::SourceText;
use super::{Expression, TypeRef, TypedId};
use super::{InfixOp, PrefixOp};
use std::iter::Peekable;

/// Parse an Expression Tree from the Source
///
/// Runs the tokeniser and parser over the given input stirng. If the
/// parse was successful then a sequence expression containing all the
/// expressions in the source is returned. If any error is encountered
/// then that is surfaced instead.
pub fn parse_tree<S: Into<String>>(source: S) -> ParseResult<Expression> {
    let source = SourceText::new(source);
    let mut p = Parser::new(&source);
    Ok(Expression::sequence(p.expressions()?))
}

/// Parse a Single Expression
///
/// Runs the tokeniser and parser of the given input string, returning
/// the first expression parsed.
pub fn parse_single<S: Into<String>>(source: S) -> ParseResult<Expression> {
    let source = SourceText::new(source);
    let mut p = Parser::new(&source);
    p.single_expression()
}

/// Expression parser. Given a stream of tokens this will produce an
/// expression tree, or a parse error.
struct Parser<'a> {
    lexer: Peekable<TriviaFilter<Tokeniser<'a>>>,
}

impl<'a> Parser<'a> {
    /// Create a new Parser from a given source text.
    pub fn new(source: &'a SourceText) -> Self {
        let lexer = Tokeniser::new(source);
        Parser {
            lexer: lexer
                .without_trivia()
                .peekable(),
        }
    }

    /// Moves the token stream on by a single token
    pub fn advance(&mut self) -> ParseResult<()> {
        match self.lexer.peek() {
            Some(_) => {
                self.lexer.next();
                Ok(())
            }
            _ => Err(ParseError::Incomplete),
        }
    }

    /// Moves the token stream on by a single token, if the
    /// token's lexeme is of the given type.
    pub fn expect(&mut self, expected: Token<'_>) -> ParseResult<()> {
        match self.lexer.peek() {
            Some(token) if token == &expected => Ok(()),
            Some(_) => Err(ParseError::Unexpected),
            None => Err(ParseError::Incomplete),
        }
        .map(|ok| {
            self.lexer.next();
            ok
        })
    }

    /// Checks that the next token is of the given type
    pub fn next_is(&mut self, expected: Token<'_>) -> bool {
        self.lexer.peek().map_or(false, |token| token == &expected)
    }

    /// Attempt to parse an identifier
    pub fn identifier(&mut self) -> ParseResult<String> {
        match self.expression(100)? {
            Expression::Identifier(string) => Ok(string),
            _ => Err(ParseError::Unexpected),
        }
    }

    /// Attempt to parse a single expression
    ///
    /// Parses a expresison with the given precedence. To parse a
    /// single expression use `single_expression`.
    pub fn expression(&mut self, rbp: u32) -> ParseResult<Expression> {
        let mut left = self.parse_nud()?;
        while self.next_binds_tighter_than(rbp) {
            left = self.parse_led(left)?;
        }
        Ok(left)
    }

    /// Atttempt to parse a list of expressions
    pub fn expressions(&mut self) -> ParseResult<Vec<Expression>> {
        let mut expressions = Vec::new();
        while self.lexer.peek().is_some() {
            expressions.push(self.single_expression()?);
        }
        Ok(expressions)
    }

    /// Attempt to Parse a Single Expression
    ///
    /// Used to parse 'top-level' expressions.
    pub fn single_expression(&mut self) -> ParseResult<Expression> {
        self.expression(0)
    }

    /// Parse Type Reference
    ///
    /// Attempt to parse a type reference, this is a single
    /// `:` followed by a type name.
    pub fn type_ref(&mut self) -> ParseResult<TypeRef> {
        self.expect(Token::Colon)?;
        self.ty()
    }

    /// Parse Type
    ///
    /// Attempt to parse a type. This could be a simple type name, or
    /// it could be a more complex one such as an array type or tuple.
    pub fn ty(&mut self) -> ParseResult<TypeRef> {
        match self.lexer.peek() {
            Some(&Token::Word(t)) => {
                self.advance()?;
                Ok(TypeRef::simple(t))
            }
            Some(&Token::OpenSqBracket) => {
                self.advance()?;
                let inner = self.ty()?;
                self.expect(Token::CloseSqBracket)?;
                Ok(TypeRef::array(inner))
            }
            Some(&Token::OpenBracket) => {
                self.advance()?;
                let mut types = Vec::new();
                if !self.next_is(Token::CloseBracket) {
                    types.push(self.ty()?);
                }
                while !self.next_is(Token::CloseBracket) {
                    self.expect(Token::Comma)?;
                    types.push(self.ty()?);
                }
                self.expect(Token::CloseBracket)?;
                Ok(TypeRef::tuple(types))
            }
            _ => Err(ParseError::Unexpected),
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
    pub fn typed_id(&mut self) -> ParseResult<TypedId> {
        let id = self.identifier()?;
        let typ = self.optional_type_ref();
        Ok(TypedId { id: id, typ: typ })
    }

    /// Attempt to parse a local declaration
    ///
    /// Parses the body of a local variable delcaration (`let` or
    /// `var`).
    pub fn declaration(&mut self, is_mut: bool) -> ParseResult<Expression> {
        let id = self.identifier()?;
        let typ = self.optional_type_ref();
        self.expect(Token::Equals)?;
        let rhs = self.single_expression()?;
        Ok(Expression::declaration(
            TypedId::from_parts(id.clone(), typ),
            is_mut,
            rhs,
        ))
    }

    /// Attempt to parse a block of expressions
    pub fn block(&mut self) -> ParseResult<Vec<Expression>> {
        let mut expressions = Vec::new();
        while self.lexer.peek().is_some() && !self.next_is(Token::Word("end")) {
            expressions.push(self.single_expression()?);
        }
        Ok(expressions)
    }

    /// Returns true if the next token's lbp is > the given rbp
    fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.lexer.peek().map_or(false, |t| t.lbp() > rbp)
    }

    /// Prefix Operator
    ///
    /// Parses the trailing expression for a prefix operator.
    fn prefix_op(&mut self, op: PrefixOp) -> ParseResult<Expression> {
        let rhs = self.expression(100)?;
        Ok(Expression::prefix(op, rhs))
    }

    /// Ternay Body
    ///
    /// The condition and fallback part of a ternary expression.
    fn ternary_body(&mut self) -> ParseResult<(Expression, Expression)> {
        let condition = self.single_expression()?;
        self.expect(Token::Word("else"))?;
        let fallback = self.single_expression()?;
        Ok((condition, fallback))
    }

    /// Parse Left Denonation
    ///
    /// This is the parse of the symbol when it has an expression to
    /// the left hand side of it. This is responsible for parsing
    /// infix operators and function calls.
    fn parse_led(&mut self, lhs: Expression) -> ParseResult<Expression> {
        let token = self.lexer.next().ok_or(ParseError::Incomplete)?;

        match token {
            // Binary infix operator
            Token::DoubleEquals => self.infix(lhs, &token, InfixOp::Eq),
            Token::BangEquals => self.infix(lhs, &token, InfixOp::NotEq),
            Token::LessThan => self.infix(lhs, &token, InfixOp::Lt),
            Token::MoreThan => self.infix(lhs, &token, InfixOp::Gt),
            Token::Equals => self.infix(lhs, &token, InfixOp::Assign),
            Token::Plus => self.infix(lhs, &token, InfixOp::Add),
            Token::Minus => self.infix(lhs, &token, InfixOp::Sub),
            Token::Star => self.infix(lhs, &token, InfixOp::Mul),
            Token::Slash => self.infix(lhs, &token, InfixOp::Div),

            // array indexing
            Token::OpenSqBracket => {
                let index = self.single_expression()?;
                self.expect(Token::CloseSqBracket)?;
                Ok(Expression::index(lhs, index))
            }

            // Function call
            Token::OpenBracket => {
                let mut params = Vec::new();
                while !self.next_is(Token::CloseBracket) {
                    let param = self.single_expression()?;
                    params.push(param);
                    if !self.next_is(Token::CloseBracket) {
                        self.expect(Token::Comma)?;
                    }
                }
                self.expect(Token::CloseBracket)?;
                Ok(Expression::call(lhs, params))
            }

            // Ternay statement:
            // <x> if <y> else <z>
            Token::Word("if") => {
                let (condition, fallback) = self.ternary_body()?;
                Ok(Expression::if_then_else(condition, lhs, fallback))
            }

            // Ternay statement:
            // <x> unless <y> else <z>
            Token::Word("unless") => {
                let (condition, fallback) = self.ternary_body()?;
                Ok(Expression::if_then_else(condition, fallback, lhs))
            }

            _ => Err(ParseError::Incomplete),
        }
    }

    /// Parse Null Denotation
    ///
    /// This is the parse of the symbol when it doesn't have any
    /// expression to the left hand side of it. This is responsible
    /// for parsing literals and variable references into expressions,
    /// as well as parsing prefix expressions.
    fn parse_nud(&mut self) -> ParseResult<Expression> {
        let token = self.lexer.next().ok_or(ParseError::Incomplete)?;

        match token {
            Token::Word("fn") => {
                let identifier = self.identifier()?;
                let mut res = Expression::function(identifier);
                self.expect(Token::OpenBracket)?;
                if !self.next_is(Token::CloseBracket) {
                    res = res.with_arg(self.typed_id()?);
                }
                while !self.next_is(Token::CloseBracket) {
                    self.expect(Token::Comma)?;
                    res = res.with_arg(self.typed_id()?);
                }
                self.expect(Token::CloseBracket)?;
                res = res
                    .with_return_type(self.type_ref()?)
                    .with_body(self.block()?);
                self.expect(Token::Word("end"))?;
                Ok(Expression::from(res))
            }
            Token::Word("until") | Token::Word("while") => {
                let mut condition = self.single_expression()?;
                // TODO: It would be nice if we could do this in
                // lowering rather than hacking it up like this in
                // parsing
                if token == Token::Word("until") {
                    condition = Expression::Prefix(PrefixOp::Not, Box::new(condition));
                }
                let block = self.block()?;
                self.expect(Token::Word("end"))?;
                Ok(Expression::loop_while(condition, block))
            }
            Token::Word("let") => self.declaration(false),
            Token::Word("var") => self.declaration(true),
            Token::Word("print") => {
                let to_print = self.single_expression()?;
                Ok(Expression::print(to_print))
            }
            Token::Word("true") => Ok(Expression::constant_bool(true)),
            Token::Word("false") => Ok(Expression::constant_bool(false)),
            Token::Word(word) => Ok(Expression::identifier(String::from(word))),
            Token::Literal(ref l) => match l {
                &Literal::Number(i) => Ok(Expression::constant_num(i)),
                &Literal::RawString(ref s) => Ok(Expression::constant_string(s.clone())),
            },
            Token::Plus => self.expression(100),
            Token::Minus => self.prefix_op(PrefixOp::Negate),
            Token::Bang => self.prefix_op(PrefixOp::Not),
            Token::OpenBracket => {
                let expr = self.single_expression()?;
                self.expect(Token::CloseBracket)?;
                Ok(expr)
            }
            // This covers things which can't start expressions, like
            // whitespace and non-prefix operator tokens
            _ => Err(ParseError::Unexpected),
        }
    }

    /// Attempt to Parse an Infix Expression
    ///
    /// Given a parsed left hand expression and infix operator parse
    /// the
    fn infix(&mut self, lhs: Expression, token: &Token, op: InfixOp) -> ParseResult<Expression> {
        let rhs = self.expression(token.lbp())?;
        Ok(Expression::infix(lhs, op, rhs))
    }
}

#[cfg(test)]
mod test {

    use super::super::Expression::*;
    use super::super::*;
    use super::parse_single;

    macro_rules! check_parse {
        ($src:expr, $expected:expr) => {
            let src: &str = $src;
            let actual = parse_single(src);
            assert_eq!(Ok($expected), actual);
        };
    }

    #[test]
    fn parse_simple_string() {
        check_parse!(
            "hello + 123",
            Expression::infix(
                Expression::identifier("hello".to_string()),
                InfixOp::Add,
                Expression::constant_num(123),
            )
        );
    }

    #[test]
    fn parse_operators() {
        check_parse!(
            "a = b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Assign,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a + b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Add,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a - b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Sub,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a * b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Mul,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a / b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Div,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a == b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Eq,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a != b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::NotEq,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a < b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Lt,
                Expression::identifier("b".to_string()),
            )
        );
        check_parse!(
            "a > b",
            Expression::infix(
                Expression::identifier("a".to_string()),
                InfixOp::Gt,
                Expression::identifier("b".to_string()),
            )
        );
    }

    #[test]
    fn parse_with_precedence() {
        check_parse!(
            "1 + 2 * 3",
            Expression::infix(
                Expression::constant_num(1),
                InfixOp::Add,
                Expression::infix(
                    Expression::constant_num(2),
                    InfixOp::Mul,
                    Expression::constant_num(3),
                ),
            )
        );
    }

    #[test]
    fn parse_prefix_expressions() {
        check_parse!(
            "+1 * -2 + +3",
            Expression::infix(
                Expression::infix(
                    Expression::constant_num(1),
                    InfixOp::Mul,
                    Expression::prefix(PrefixOp::Negate, Expression::constant_num(2)),
                ),
                InfixOp::Add,
                Expression::constant_num(3),
            )
        );
        check_parse!(
            "!a",
            Expression::prefix(PrefixOp::Not, Expression::identifier("a".to_string()))
        );
        check_parse!(
            "!a != !b",
            Expression::infix(
                Expression::prefix(PrefixOp::Not, Expression::identifier("a".to_string())),
                InfixOp::NotEq,
                Expression::prefix(PrefixOp::Not, Expression::identifier("b".to_string())),
            )
        );
    }

    #[test]
    fn parse_simple_call() {
        check_parse!(
            "foo()",
            Expression::call(Expression::identifier("foo".to_string()), Vec::new())
        );
    }

    #[test]
    fn parse_complex_call() {
        check_parse!(
            "hello(1, 1 + 23, -world)",
            Expression::call(
                Expression::identifier("hello".to_string()),
                vec![
                    Expression::constant_num(1),
                    Expression::infix(
                        Expression::constant_num(1),
                        InfixOp::Add,
                        Expression::constant_num(23),
                    ),
                    Expression::prefix(
                        PrefixOp::Negate,
                        Expression::identifier("world".to_string()),
                    ),
                ],
            )
        );
    }

    #[test]
    fn parse_groups_with_parens() {
        check_parse!(
            "(1 + 2) * 3",
            Infix(
                Box::new(Infix(
                    Box::new(Expression::constant_num(1)),
                    InfixOp::Add,
                    Box::new(Expression::constant_num(2)),
                )),
                InfixOp::Mul,
                Box::new(Expression::constant_num(3)),
            )
        );
    }

    #[test]
    fn parse_indexing() {
        check_parse!(
            "hello[world](1, 2[3])",
            Expression::call(
                Expression::index(
                    Expression::identifier("hello".to_string()),
                    Expression::identifier("world".to_string()),
                ),
                vec![
                    Expression::constant_num(1),
                    Expression::index(Expression::constant_num(2), Expression::constant_num(3)),
                ],
            )
        );
    }

    #[test]
    fn parse_ternary_if() {
        check_parse!(
            "1 if 2 else 3",
            Expression::if_then_else(
                Expression::constant_num(2),
                Expression::constant_num(1),
                Expression::constant_num(3),
            )
        );
        check_parse!(
            "hello(1) if foo[23] else world[1 if foo else 2]",
            Expression::if_then_else(
                Expression::index(
                    Expression::identifier("foo".to_string()),
                    Expression::constant_num(23),
                ),
                Expression::call(
                    Expression::identifier("hello".to_string()),
                    vec![Expression::constant_num(1)],
                ),
                Expression::index(
                    Expression::identifier("world".to_string()),
                    Expression::if_then_else(
                        Expression::identifier("foo".to_string()),
                        Expression::constant_num(1),
                        Expression::constant_num(2),
                    ),
                ),
            )
        );
        check_parse!(
            "0 unless 1 else 2",
            Expression::if_then_else(
                Expression::constant_num(1),
                Expression::constant_num(2),
                Expression::constant_num(0),
            )
        );
    }

    #[test]
    fn parse_unicode_identifiers() {
        check_parse!(
            "  übåℝ * ßeåk  ",
            Expression::infix(
                Expression::identifier("übåℝ".to_string()),
                InfixOp::Mul,
                Expression::identifier("ßeåk".to_string()),
            )
        );
    }

    #[test]
    fn parse_function_def() {
        check_parse!(
            "fn test() :Num 100 end",
            Expression::function("test".to_string())
                .with_return_type(TypeRef::simple("Num"))
                .with_body(vec![Expression::constant_num(100)])
                .into()
        );
        check_parse!(
            "fn ünécød3() :Num
                0 if 74 else 888
             end",
            Expression::function("ünécød3".to_string())
                .with_return_type(TypeRef::simple("Num"))
                .with_body(vec![Expression::if_then_else(
                    Expression::constant_num(74),
                    Expression::constant_num(0),
                    Expression::constant_num(888),
                )])
                .into()
        );
    }

    #[test]
    fn parse_while_loop() {
        check_parse!(
            "while 1 end",
            Expression::loop_while(Expression::constant_num(1), Vec::new())
        );
        check_parse!(
            "while 0 44 234 end",
            Expression::loop_while(
                Expression::constant_num(0),
                vec![Expression::constant_num(44), Expression::constant_num(234)],
            )
        );
    }

    #[test]
    fn parse_function_with_args() {
        check_parse!(
            "fn neg(i: Num): Num - i end",
            Expression::function("neg".to_string())
                .with_arg(TypedId::new("i".to_string(), TypeRef::simple("Num")))
                .with_return_type(TypeRef::simple("Num"))
                .with_body(vec![Expression::prefix(
                    PrefixOp::Negate,
                    Expression::identifier("i".to_string()),
                )])
                .into()
        );

        check_parse!(
            "fn test(i: Num, j, k: String): String i + j + k end",
            Expression::function("test".to_string())
                .with_arg(TypedId::new("i".to_string(), TypeRef::simple("Num")))
                .with_arg(TypedId::new_without_type("j".to_string()))
                .with_arg(TypedId::new("k".to_string(), TypeRef::simple("String")))
                .with_return_type(TypeRef::simple("String"))
                .with_body(vec![Expression::infix(
                    Expression::infix(
                        Expression::identifier("i".to_string()),
                        InfixOp::Add,
                        Expression::identifier("j".to_string()),
                    ),
                    InfixOp::Add,
                    Expression::identifier("k".to_string()),
                )])
                .into()
        );
    }

    #[test]
    fn parse_simple_array_type() {
        check_parse!(
            "let f: [Num] = 100",
            Expression::declaration(
                TypedId::from_parts(
                    String::from("f"),
                    Some(TypeRef::array(TypeRef::simple("Num"))),
                ),
                false,
                Expression::constant_num(100),
            )
        );
    }

    #[test]
    fn parse_simple_let() {
        check_parse!(
            "let foo = 100",
            Expression::declaration(
                TypedId::from_parts("foo".to_string(), None),
                false,
                Expression::constant_num(100),
            )
        );
    }

    #[test]
    fn parse_simple_tuple() {
        check_parse!(
            "let f: (Num) = 100",
            Expression::declaration(
                TypedId::from_parts(
                    String::from("f"),
                    Some(TypeRef::tuple(vec![TypeRef::simple("Num")])),
                ),
                false,
                Expression::constant_num(100),
            )
        );
        check_parse!(
            "let f: (Num, [String]) = 100",
            Expression::declaration(
                TypedId::from_parts(
                    String::from("f"),
                    Some(TypeRef::tuple(vec![
                        TypeRef::simple("Num"),
                        TypeRef::array(TypeRef::simple("String")),
                    ])),
                ),
                false,
                Expression::constant_num(100),
            )
        );
    }

    #[test]
    fn parse_variable_decl() {
        check_parse!(
            "var foo = 93",
            Expression::declaration(
                TypedId::from_parts(String::from("foo"), None),
                true,
                Expression::constant_num(93),
            )
        );
        check_parse!(
            "var foo_bar: Number = -99999",
            Expression::declaration(
                TypedId::from_parts(String::from("foo_bar"), Some(TypeRef::simple("Number"))),
                true,
                Expression::prefix(PrefixOp::Negate, Expression::constant_num(99999)),
            )
        );
    }

    #[test]
    fn parse_print_operator() {
        check_parse!(
            "print 1334",
            Expression::print(Expression::constant_num(1334))
        );
    }

    #[test]
    fn parse_bool_literal() {
        check_parse!("true", Expression::constant_bool(true));
        check_parse!("false", Expression::constant_bool(false));
    }

    #[test]
    fn parse_string_literal() {
        check_parse!("'hello'", Expression::constant_string("hello"));
        check_parse!(
            "'über ∂elta'",
            Expression::constant_string("über ∂elta")
        );
    }
}
