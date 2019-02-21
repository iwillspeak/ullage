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
//! to parse with a single-token lookahead. For a full description of
//! the syntax take a look at the Syntax documentation in the `docs/`
//! folder.

use super::super::text::{Ident, SourceText};
use super::super::tree::{Literal, Token, TokenKind};
use super::super::{
    BlockBody, DelimItem, Expression, InfixOp, PrefixOp, TypeRef, TypedId, VarStyle,
};
use super::error::*;
use super::tokeniser::Tokeniser;

/// Expression parser. Given a stream of tokens this will produce an
/// expression tree, or a parse error.
pub struct Parser<'a> {
    source: &'a SourceText,
    lexer: Tokeniser<'a>,
    diagnostics: Vec<String>,
    #[allow(clippy::option_option)]
    current: Option<Option<Token>>,
}

impl<'a> Parser<'a> {
    /// Create a new Parser from a given source text.
    pub fn new(source: &'a SourceText) -> Self {
        Parser {
            source,
            lexer: Tokeniser::new(source),
            diagnostics: Vec::new(),
            current: None,
        }
    }

    /// Collect the Parser and Lexer Diagnostics
    fn collect_diagnostics(&mut self) -> Vec<String> {
        let mut diagnostics = Vec::new();
        diagnostics.append(&mut self.diagnostics);
        diagnostics.append(self.lexer.diagnostics_mut());
        diagnostics
    }

    /// Moves the token stream on by a single token. The curren token
    /// is returned.
    #[must_use]
    fn advance(&mut self) -> Option<Token> {
        match self.current.take() {
            Some(maybe_token) => maybe_token,
            None => self.lexer.next(),
        }
    }

    /// Peek at the current token
    ///
    /// Buffers a token if one is not buffered and returns a reference
    /// to it. This performs the one-token lookahead this parser
    /// requires.
    fn current(&mut self) -> Option<&Token> {
        if self.current.is_none() {
            self.current = Some(self.lexer.next());
        }
        match self.current {
            Some(Some(ref token)) => Some(token),
            _ => None,
        }
    }

    /// Moves the token stream on by a single token, if the
    /// token's lexeme is of the given type.
    #[must_use]
    fn expect(&mut self, expected: &TokenKind) -> Token {
        match self.current() {
            Some(token) if token.kind == *expected => self.advance().unwrap(),
            Some(other) => {
                let diagnostic = format!("expecting: {:?}, found: {:?}", other, expected);
                self.diagnostics.push(diagnostic.clone());
                Token::new(expected.clone())
            }
            None => {
                self.diagnostics
                    .push(format!("expected {} but found end of file", expected));
                Token::new(expected.clone())
            }
        }
    }

    /// Checks that the next token is of the given type
    fn next_is(&mut self, expected: &TokenKind) -> bool {
        self.current()
            .map_or(false, |token| token.kind == *expected)
    }

    /// Attempt to parse an identifier
    fn identifier(&mut self) -> ParseResult<Ident> {
        match self.expression(100)? {
            Expression::Identifier(id) => Ok(id.ident),
            other => Err(ParseError::Unexpected(format!(
                "expected identifier, found: {:?}",
                other
            ))),
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
        while self.current().is_some() {
            expressions.push(self.single_expression()?);
        }
        let errors = self.collect_diagnostics();
        if !errors.is_empty() {
            Err(ParseError::Diagnostics(errors.into()))
        } else {
            Ok(expressions)
        }
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
        // FIXME: need to store token introducing type
        let _fixme = self.expect(&TokenKind::Colon);
        self.ty()
    }

    /// Parse Type
    ///
    /// Attempt to parse a type. This could be a simple type name, or
    /// it could be a more complex one such as an array type or tuple.
    fn ty(&mut self) -> ParseResult<TypeRef> {
        let current = self.current().ok_or(ParseError::Incomplete)?;
        Ok(match &current.kind {
            TokenKind::Word(_) => TypeRef::simple(self.advance().unwrap()),
            TokenKind::OpenSqBracket => {
                // FIXME: proper token handling for array types
                let _open = self.advance();
                let inner = self.ty()?;
                let _close = self.expect(&TokenKind::CloseSqBracket);
                TypeRef::array(inner)
            }
            TokenKind::OpenBracket => {
                // FIXME: Proper token handling for tuple types
                let _open = self.advance();
                let mut types = Vec::new();
                if !self.next_is(&TokenKind::CloseBracket) {
                    types.push(self.ty()?);
                }
                while !self.next_is(&TokenKind::CloseBracket) {
                    // FIXME: Separated lists
                    let _delim = self.expect(&TokenKind::Comma);
                    types.push(self.ty()?);
                }
                let _close = self.expect(&TokenKind::CloseBracket);
                TypeRef::tuple(types)
            }
            t => {
                return Err(ParseError::Unexpected(format!(
                    "expected type, found: {:?}",
                    t
                )));
            }
        })
    }

    /// Parse an optional type reference
    ///
    /// If there is a type refernece then parse it and return it. If
    /// there is no type we may have to infer it later.
    fn optional_type_ref(&mut self) -> Option<TypeRef> {
        if self.next_is(&TokenKind::Colon) {
            self.type_ref().ok()
        } else {
            None
        }
    }

    /// Parse an identifier, with an optional type
    fn typed_id(&mut self) -> ParseResult<TypedId> {
        let id = self.identifier()?;
        let typ = self.optional_type_ref();
        Ok(TypedId { id, typ })
    }

    /// Attempt to parse a local declaration
    ///
    /// Parses the body of a local variable delcaration (`let` or
    /// `var`).
    fn declaration(&mut self, var_tok: Token) -> ParseResult<Expression> {
        let id = self.identifier()?;
        let typ = self.optional_type_ref();
        let assign_tok = self.expect(&TokenKind::Equals);
        let rhs = self.single_expression()?;
        let style = if let TokenKind::Word(Ident::Var) = var_tok.kind {
            VarStyle::Mutable
        } else {
            VarStyle::Immutable
        };
        Ok(Expression::declaration(
            var_tok,
            TypedId::from_parts(id, typ),
            style,
            assign_tok,
            rhs,
        ))
    }

    /// Attempt to parse a block of expressions
    fn block(&mut self) -> ParseResult<BlockBody> {
        let mut expressions = Vec::new();
        while self.current().is_some() && !self.next_is(&TokenKind::Word(Ident::End)) {
            expressions.push(self.single_expression()?);
        }
        Ok(BlockBody {
            contents: Box::new(Expression::sequence(expressions)),
            close: Box::new(self.expect(&TokenKind::Word(Ident::End))),
        })
    }

    /// Returns true if the next token's lbp is > the given rbp
    fn next_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.current().map_or(false, |t| t.lbp() > rbp)
    }

    /// Prefix Operator
    ///
    /// Parses the trailing expression for a prefix operator.
    fn prefix_op(&mut self, op_token: Token, op: PrefixOp) -> ParseResult<Expression> {
        let rhs = self.expression(100)?;
        Ok(Expression::prefix(op_token, op, rhs))
    }

    /// Ternay Body
    ///
    /// The condition and fallback part of a ternary expression.
    fn ternary_body(&mut self) -> ParseResult<(Expression, Token, Expression)> {
        let condition = self.single_expression()?;
        let else_tok = self.expect(&TokenKind::Word(Ident::Else));
        let fallback = self.single_expression()?;
        Ok((condition, else_tok, fallback))
    }

    /// Parse Left Denonation
    ///
    /// This is the parse of the symbol when it has an expression to
    /// the left hand side of it. This is responsible for parsing
    /// infix operators and function calls.
    fn parse_led(&mut self, lhs: Expression) -> ParseResult<Expression> {
        let token = self.advance().ok_or(ParseError::Incomplete)?;

        match token.kind {
            // Binary infix operator
            TokenKind::DoubleEquals => self.infix(lhs, token, InfixOp::Eq),
            TokenKind::BangEquals => self.infix(lhs, token, InfixOp::NotEq),
            TokenKind::LessThan => self.infix(lhs, token, InfixOp::Lt),
            TokenKind::LessThanEqual => self.infix(lhs, token, InfixOp::LtEq),
            TokenKind::MoreThan => self.infix(lhs, token, InfixOp::Gt),
            TokenKind::MoreThanEqual => self.infix(lhs, token, InfixOp::GtEq),
            TokenKind::Equals => self.infix(lhs, token, InfixOp::Assign),
            TokenKind::Plus => self.infix(lhs, token, InfixOp::Add),
            TokenKind::Minus => self.infix(lhs, token, InfixOp::Sub),
            TokenKind::Star => self.infix(lhs, token, InfixOp::Mul),
            TokenKind::Slash => self.infix(lhs, token, InfixOp::Div),

            // array indexing
            TokenKind::OpenSqBracket => {
                let open = token;
                let index = self.single_expression()?;
                let close = self.expect(&TokenKind::CloseSqBracket);
                Ok(Expression::index(lhs, open, index, close))
            }

            // Function call
            TokenKind::OpenBracket => {
                let open = token;
                let mut params = Vec::new();
                while !self.next_is(&TokenKind::CloseBracket) {
                    let param = self.single_expression()?;
                    params.push(param);
                    if !self.next_is(&TokenKind::CloseBracket) {
                        // FIXME: Separated lists. Ties in with tuples
                        let _delim = self.expect(&TokenKind::Comma);
                    }
                }
                let close = self.expect(&TokenKind::CloseBracket);
                Ok(Expression::call(lhs, open, params, close))
            }

            // Ternay statement:
            // <x> if <y> else <z>
            TokenKind::Word(Ident::If) => {
                let if_tok = token;
                let (condition, else_tok, fallback) = self.ternary_body()?;
                Ok(Expression::if_then_else(
                    if_tok, condition, lhs, else_tok, fallback,
                ))
            }

            // Ternay statement:
            // <x> unless <y> else <z>
            TokenKind::Word(Ident::Unless) => {
                let if_tok = token;
                let (condition, else_tok, fallback) = self.ternary_body()?;
                Ok(Expression::if_then_else(
                    if_tok, condition, fallback, else_tok, lhs,
                ))
            }

            _ => unreachable!("`parse_led` should only be called if looking at a token with a left binding power."),
        }
    }

    /// Parse a Delimited List of Items
    ///
    /// Returns a list of zero or more elemnets delimited by the given
    /// tokens. Used to parse the parameter list for a function and
    /// the argument list for a call site.
    fn delimited(
        &mut self,
        delimiter: TokenKind,
        close: TokenKind,
    ) -> ParseResult<Vec<DelimItem<TypedId>>> {
        let mut res = Vec::new();
        if !self.next_is(&close) {
            res.push(DelimItem::First(self.typed_id()?));
        }
        while !self.next_is(&close) {
            let delim = self.expect(&delimiter);
            res.push(DelimItem::Follow(delim, self.typed_id()?));
        }
        Ok(res)
    }

    /// Parse Null Denotation
    ///
    /// This is the parse of the symbol when it doesn't have any
    /// expression to the left hand side of it. This is responsible
    /// for parsing literals and variable references into expressions,
    /// as well as parsing prefix expressions.
    fn parse_nud(&mut self) -> ParseResult<Expression> {
        let token = self.advance().ok_or(ParseError::Incomplete)?;

        match token.kind {
            TokenKind::Word(Ident::Fn) => {
                let fn_kw = token;
                let identifier = self.identifier()?;
                let params_open = self.expect(&TokenKind::OpenBracket);
                let params = self.delimited(TokenKind::Comma, TokenKind::CloseBracket)?;
                let params_close = self.expect(&TokenKind::CloseBracket);
                let return_type = self.type_ref()?;
                let body = self.block()?;
                Ok(Expression::function(
                    fn_kw,
                    identifier,
                    params_open,
                    params,
                    params_close,
                    return_type,
                    body,
                ))
            }
            TokenKind::Word(Ident::While) | TokenKind::Word(Ident::Until) => {
                let condition = self.single_expression()?;
                let block = self.block()?;
                Ok(Expression::loop_while(token, condition, block))
            }
            TokenKind::Word(Ident::Let) | TokenKind::Word(Ident::Var) => self.declaration(token),
            TokenKind::Word(Ident::Print) => {
                let to_print = self.single_expression()?;
                Ok(Expression::print(token, to_print))
            }
            TokenKind::Word(Ident::True) => Ok(Expression::constant_bool(token, true)),
            TokenKind::Word(Ident::False) => Ok(Expression::constant_bool(token, false)),
            TokenKind::Word(word) => Ok(Expression::identifier(token, word)),
            TokenKind::Literal(ref l) => match *l {
                Literal::Number(i) => Ok(Expression::constant_num(token, i)),
                Literal::RawString(ref s) => {
                    let string_value = s.clone();
                    Ok(Expression::constant_string(token, string_value))
                }
            },
            TokenKind::Plus => self.expression(100),
            TokenKind::Minus => self.prefix_op(token, PrefixOp::Negate),
            TokenKind::Bang => self.prefix_op(token, PrefixOp::Not),
            TokenKind::OpenBracket => {
                let expr = self.single_expression()?;
                let closing = self.expect(&TokenKind::CloseBracket);
                Ok(Expression::grouping(token, expr, closing))
            }
            // This covers things which can't start expressions, like
            // whitespace and non-prefix operator tokens
            _ => {
                let pos = self.source.line_pos(token.span().start);
                Err(ParseError::Unexpected(format!(
                    "{} at {}:{}",
                    token.kind, pos.0, pos.1
                )))
            }
        }
    }

    /// Attempt to Parse an Infix Expression
    ///
    /// Given a parsed left hand expression and infix operator parse
    /// the
    fn infix(&mut self, lhs: Expression, token: Token, op: InfixOp) -> ParseResult<Expression> {
        let rhs = self.expression(token.lbp())?;
        Ok(Expression::infix(lhs, token, op, rhs))
    }
}
