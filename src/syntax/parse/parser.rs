//! Parser Module
//!
//! Tis file contains the language parser implementation. The parser
//! is a hand-written recursive descent / operator precedence parser.
//!
//! The input to the parser is a given `SourceText`. The parser
//! converts this into a stream of syntax `Token`s with a `Tokeniser`
//! and walks the resulting iterator to construct a parse tree.
//!
//! # Grammar
//!
//! The language grammer is relatively simple and should be possible
//! to parse with a single-token lookahead. This parser uses the
//! `Peekable` iterator to acchieve this.

use super::super::text::SourceText;
use super::super::{Expression, InfixOp, PrefixOp, TypeRef, TypedId};
use super::error::*;
use super::token::{Literal, Token};
use super::tokeniser::Tokeniser;
use super::trivia_filter::*;

use std::iter::Peekable;

/// Expression parser. Given a stream of tokens this will produce an
/// expression tree, or a parse error.
pub struct Parser<'a> {
    lexer: Peekable<TriviaFilter<Tokeniser<'a>>>,
}

impl<'a> Parser<'a> {
    /// Create a new Parser from a given source text.
    pub fn new(source: &'a SourceText) -> Self {
        let lexer = Tokeniser::new(source);
        Parser {
            lexer: lexer.without_trivia().peekable(),
        }
    }

    /// Moves the token stream on by a single token
    fn advance(&mut self) -> ParseResult<()> {
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
    fn expect(&mut self, expected: Token<'_>) -> ParseResult<()> {
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
    fn next_is(&mut self, expected: Token<'_>) -> bool {
        self.lexer.peek().map_or(false, |token| token == &expected)
    }

    /// Attempt to parse an identifier
    fn identifier(&mut self) -> ParseResult<String> {
        match self.expression(100)? {
            Expression::Identifier(string) => Ok(string),
            _ => Err(ParseError::Unexpected),
        }
    }

    /// Attempt to parse a single expression
    ///
    /// Parses a expresison with the given precedence. To parse a
    /// single expression use `single_expression`.
    fn expression(&mut self, rbp: u32) -> ParseResult<Expression> {
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
    fn type_ref(&mut self) -> ParseResult<TypeRef> {
        self.expect(Token::Colon)?;
        self.ty()
    }

    /// Parse Type
    ///
    /// Attempt to parse a type. This could be a simple type name, or
    /// it could be a more complex one such as an array type or tuple.
    fn ty(&mut self) -> ParseResult<TypeRef> {
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
    fn optional_type_ref(&mut self) -> Option<TypeRef> {
        if self.next_is(Token::Colon) {
            self.type_ref().ok()
        } else {
            None
        }
    }

    /// Parse an identifier, with an optional type
    fn typed_id(&mut self) -> ParseResult<TypedId> {
        let id = self.identifier()?;
        let typ = self.optional_type_ref();
        Ok(TypedId { id: id, typ: typ })
    }

    /// Attempt to parse a local declaration
    ///
    /// Parses the body of a local variable delcaration (`let` or
    /// `var`).
    fn declaration(&mut self, is_mut: bool) -> ParseResult<Expression> {
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
    fn block(&mut self) -> ParseResult<Vec<Expression>> {
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
