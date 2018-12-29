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
use super::super::tree::{Token, Literal, TokenKind};
use super::raw_tokeniser::{RawTokeniser, TokenGrouper};

use std::iter::Peekable;

/// Expression parser. Given a stream of tokens this will produce an
/// expression tree, or a parse error.
pub struct Parser<'a> {
    lexer: Peekable<TokenGrouper<'a>>,
    diagnostics: Vec<String>,
}

impl<'a> Parser<'a> {
    /// Create a new Parser from a given source text.
    pub fn new(source: &'a SourceText) -> Self {
        let lexer = RawTokeniser::new(source);
        Parser {
            lexer: lexer.group_trivia().peekable(),
            diagnostics: Vec::new(),
        }
    }

    /// Moves the token stream on by a single token
    fn advance(&mut self) {
        self.lexer.next();
    }

    /// Moves the token stream on by a single token, if the
    /// token's lexeme is of the given type.
    fn expect(&mut self, expected: TokenKind) -> ParseResult<()> {
        let maybe_token = self.lexer.peek();
        match maybe_token {
            Some(token) if token.kind == expected => {
                self.advance();
                Ok(())
            }
            Some(other) => {
                self.diagnostics.push(format!(
                    "Unexpected token: {:?}, expecting: {:?}",
                    other, expected
                ));
                Err(ParseError::Unexpected)
            }
            None => {
                self.diagnostics
                    .push(format!("Expected {:?} but found end of file", expected));
                Err(ParseError::Incomplete)
            }
        }
    }

    /// Checks that the next token is of the given type
    fn next_is(&mut self, expected: TokenKind) -> bool {
        self.lexer.peek().map_or(false, |token| token.kind == expected)
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
        self.expect(TokenKind::Colon)?;
        self.ty()
    }

    /// Parse Type
    ///
    /// Attempt to parse a type. This could be a simple type name, or
    /// it could be a more complex one such as an array type or tuple.
    fn ty(&mut self) -> ParseResult<TypeRef> {
        match self.lexer.peek() {
            Some(&Token{ kind: TokenKind::Word(ref t), ..}) => {
                let t = t.clone();
                self.advance();
                Ok(TypeRef::simple(t))
            }
            Some(&Token{ kind: TokenKind::OpenSqBracket, ..}) => {
                self.advance();
                let inner = self.ty()?;
                self.expect(TokenKind::CloseSqBracket)?;
                Ok(TypeRef::array(inner))
            }
            Some(&Token{ kind: TokenKind::OpenBracket, ..}) => {
                self.advance();
                let mut types = Vec::new();
                if !self.next_is(TokenKind::CloseBracket) {
                    types.push(self.ty()?);
                }
                while !self.next_is(TokenKind::CloseBracket) {
                    self.expect(TokenKind::Comma)?;
                    types.push(self.ty()?);
                }
                self.expect(TokenKind::CloseBracket)?;
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
        if self.next_is(TokenKind::Colon) {
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
        self.expect(TokenKind::Equals)?;
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
        while self.lexer.peek().is_some() && !self.next_is(TokenKind::Word("end".into())) {
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
        self.expect(TokenKind::Word("else".into()))?;
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

        match token.kind {
            // Binary infix operator
            TokenKind::DoubleEquals => self.infix(lhs, &token, InfixOp::Eq),
            TokenKind::BangEquals => self.infix(lhs, &token, InfixOp::NotEq),
            TokenKind::LessThan => self.infix(lhs, &token, InfixOp::Lt),
            TokenKind::MoreThan => self.infix(lhs, &token, InfixOp::Gt),
            TokenKind::Equals => self.infix(lhs, &token, InfixOp::Assign),
            TokenKind::Plus => self.infix(lhs, &token, InfixOp::Add),
            TokenKind::Minus => self.infix(lhs, &token, InfixOp::Sub),
            TokenKind::Star => self.infix(lhs, &token, InfixOp::Mul),
            TokenKind::Slash => self.infix(lhs, &token, InfixOp::Div),

            // array indexing
            TokenKind::OpenSqBracket => {
                let index = self.single_expression()?;
                self.expect(TokenKind::CloseSqBracket)?;
                Ok(Expression::index(lhs, index))
            }

            // Function call
            TokenKind::OpenBracket => {
                let mut params = Vec::new();
                while !self.next_is(TokenKind::CloseBracket) {
                    let param = self.single_expression()?;
                    params.push(param);
                    if !self.next_is(TokenKind::CloseBracket) {
                        self.expect(TokenKind::Comma)?;
                    }
                }
                self.expect(TokenKind::CloseBracket)?;
                Ok(Expression::call(lhs, params))
            }

            // Ternay statement:
            // <x> if <y> else <z>
            TokenKind::Word(ref w) if w == "if"  => {
                let (condition, fallback) = self.ternary_body()?;
                Ok(Expression::if_then_else(condition, lhs, fallback))
            }

            // Ternay statement:
            // <x> unless <y> else <z>
            TokenKind::Word(ref w) if w == "unless" => {
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

        match token.kind {
            TokenKind::Word(ref w) if w == "fn" => {
                let identifier = self.identifier()?;
                let mut res = Expression::function(identifier);
                self.expect(TokenKind::OpenBracket)?;
                if !self.next_is(TokenKind::CloseBracket) {
                    res = res.with_arg(self.typed_id()?);
                }
                while !self.next_is(TokenKind::CloseBracket) {
                    self.expect(TokenKind::Comma)?;
                    res = res.with_arg(self.typed_id()?);
                }
                self.expect(TokenKind::CloseBracket)?;
                res = res
                    .with_return_type(self.type_ref()?)
                    .with_body(self.block()?);
                self.expect(TokenKind::Word("end".into()))?;
                Ok(Expression::from(res))
            }
            TokenKind::Word(ref w) if w == "while" || w == "until" => {
                let mut condition = self.single_expression()?;
                // TODO: It would be nice if we could do this in
                // lowering rather than hacking it up like this in
                // parsing
                if w == "until" {
                    condition = Expression::Prefix(PrefixOp::Not, Box::new(condition));
                }
                let block = self.block()?;
                self.expect(TokenKind::Word("end".into()))?;
                Ok(Expression::loop_while(condition, block))
            }
            TokenKind::Word(ref w) if w == "let" => self.declaration(false),
            TokenKind::Word(ref w) if w == "var" => self.declaration(true),
            TokenKind::Word(ref w) if w == "print" => {
                let to_print = self.single_expression()?;
                Ok(Expression::print(to_print))
            }
            TokenKind::Word(ref w) if w == "true" => Ok(Expression::constant_bool(true)),
            TokenKind::Word(ref w) if w == "false" => Ok(Expression::constant_bool(false)),
            TokenKind::Word(word) => Ok(Expression::identifier(String::from(word))),
            TokenKind::Literal(ref l) => match l {
                &Literal::Number(i) => Ok(Expression::constant_num(i)),
                &Literal::RawString(ref s) => Ok(Expression::constant_string(s.clone())),
            },
            TokenKind::Plus => self.expression(100),
            TokenKind::Minus => self.prefix_op(PrefixOp::Negate),
            TokenKind::Bang => self.prefix_op(PrefixOp::Not),
            TokenKind::OpenBracket => {
                let expr = self.single_expression()?;
                self.expect(TokenKind::CloseBracket)?;
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
