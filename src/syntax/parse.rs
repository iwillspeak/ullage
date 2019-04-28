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

mod tokeniser;

#[cfg(test)]
mod checkparse_tests;

use super::text::{Ident, SourceText, DUMMY_SPAN};
use super::tree::{Literal, SyntaxTree, Token, TokenKind};
use super::{
    BlockBody, DelimItem, Expression, InfixOp, PrefixOp, TypeAnno, TypeRef, TypedId, VarStyle,
};
use crate::diag::Diagnostic;
use std::iter::Iterator;
use tokeniser::{TokenStream, Tokeniser};

/// Parser state structure
///
/// The parser object holds on to the source text and token stream
/// while parsing takes place. It's used to buffer up the current
/// token that the parser is looking at. Internally the parser also
/// buffers up the diagnostics which will be emitted at the end of a
/// parse.
pub(crate) struct Parser<'a> {
    source: &'a SourceText,
    lexer: Tokeniser<'a>,
    diagnostics: Vec<Diagnostic>,
    current: Option<Token>,
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
    ///
    /// This transfers the ownership of the buffered diagnostics from
    /// the parser and lexer to a new `Vec`. After calling this the
    /// parser and lexer diagnostics will be empty.
    fn collect_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        diagnostics.append(&mut self.diagnostics);
        diagnostics.append(self.lexer.diagnostics_mut());
        diagnostics
    }

    /// Peek at the current token
    ///
    /// Buffers a token if one is not buffered and returns a reference
    /// to it. This performs the one-token lookahead this parser
    /// requires.
    fn current(&mut self) -> &Token {
        let current = &mut self.current;
        let lexer = &mut self.lexer;
        current.get_or_insert_with(|| lexer.next_token())
    }

    /// Check the type of the current token
    ///
    /// Buffers a token with `current` and inspects the kind.
    fn current_is(&mut self, expected: &TokenKind) -> bool {
        self.current().kind == *expected
    }

    /// Check if the current token is one of many kinds of tokens.
    ///
    /// Buffers a token with `current` and inspects the kind. Returns
    /// true if it matches any of the input token kinds.
    fn current_is_any<'i, T>(&mut self, expected: T) -> bool
    where
        T: IntoIterator<Item = &'i TokenKind>,
    {
        let current_kind = &self.current().kind;
        expected.into_iter().any(|e| e == current_kind)
    }

    /// Check the binding power of the current token
    ///
    /// Returns true if the current token's lbp is greater than the
    /// given rbp.
    fn current_binds_tighter_than(&mut self, rbp: u32) -> bool {
        self.current().lbp() > rbp
    }

    /// Advance the token stream
    ///
    /// Moves the token stream on by a single token. The curren token
    /// is returned. This will always return _some_ token. Reads past
    /// the end of the lexer's underlyint tokens will retrurn
    /// syntasized `TokenKind::End` tokens.
    #[must_use]
    fn advance(&mut self) -> Token {
        match self.current.take() {
            Some(maybe_token) => maybe_token,
            None => self.lexer.next_token(),
        }
    }

    /// Check for expected current token kind
    ///
    /// Moves the token stream on by a single token, if the token's
    /// kind matches the expected. This will stub out a syntasized
    /// token and buffer a diagnostic if the wrong kind of token was
    /// encountered.
    ///
    /// To check the kind of a token and conditionally parse
    /// `current_is` should be used rather than calling expect.
    #[must_use]
    fn expect(&mut self, expected: &TokenKind) -> Token {
        match self.current() {
            token if token.kind == *expected => self.advance(),
            other => {
                let diagnostic = Diagnostic::new(
                    format!("expecting: {}, found: {}", expected, other.kind),
                    other.span(),
                );
                self.diagnostics.push(diagnostic);
                Token::new(expected.clone())
            }
        }
    }

    /// Parse a syntax tree from the given source text
    ///
    /// This is the root production of the grammar. Each compilation
    /// unit consists of a series of expressions. This loops until it
    /// finds the end of file producing a list of the expressions it
    /// parsed.
    pub fn parse(&mut self) -> SyntaxTree {
        let mut expressions = Vec::new();
        while !self.current_is(&TokenKind::End) {
            expressions.push(self.top_level_expression());
        }
        let end = self.expect(&TokenKind::End);
        let errors = self.collect_diagnostics();
        SyntaxTree::new(Expression::sequence(expressions), errors, end)
    }

    /// Parse a single expression into a tree
    ///
    /// Used to parse 'top-level' expressions. This can be a root
    /// production in the grammar if attempting to parse a single
    /// item. e.g. for scripting or testing purposes.
    pub fn parse_single(&mut self) -> SyntaxTree {
        let expression = self.top_level_expression();
        let end = self.expect(&TokenKind::End);
        let errors = self.collect_diagnostics();
        SyntaxTree::new(expression, errors, end)
    }

    /// Attempt to parse a single expression
    ///
    /// Parses a expresison with the given precedence. This is really
    /// the main production in the grammar. It's called by the
    /// higher-level grammar entry points `expression` and
    /// `expressions`. This will be called in three different variations:
    ///
    ///  * With `Token::MAX_LBP` - to parse the expression for a right
    ///    associative operator.
    ///  * With `Token::MIN_LPB` - To parse a root leve expression.
    ///  * With the binding power taken from a token to parse the
    ///    right hand side of an infix expression.
    fn expression_with_rbp(&mut self, rbp: u32) -> Expression {
        let mut left = self.parse_nud();
        while self.current_binds_tighter_than(rbp) {
            left = self.parse_led(left);
        }
        left
    }

    /// Top level expression helper
    ///
    /// Parses a single expression with the binding power set to
    /// `Token::MIN_LBP`.
    fn top_level_expression(&mut self) -> Expression {
        self.expression_with_rbp(Token::MIN_LBP)
    }

    /// Attempt to parse an identifier
    fn identifier(&mut self) -> (Token, Ident) {
        let current = self.current();
        match &current.kind {
            TokenKind::Word(id) => {
                let id = *id;
                (self.advance(), id)
            }
            kind => {
                let err = Diagnostic::new(
                    format!("expected identifier, found: {:}", kind),
                    current.span(),
                );
                self.diagnostics.push(err);
                // by starting this with an invalid character we make
                // sure we don't clash with a real identifier.
                let stub_id = self.source.intern("0invalid_ident0");
                (Token::new(TokenKind::Word(stub_id)), stub_id)
            }
        }
    }

    /// Parse Type Annotation
    ///
    /// Attempt to parse a type reference, this is a single
    /// `:` followed by a type name.
    fn type_anno(&mut self) -> TypeAnno {
        let anno_tok = self.expect(&TokenKind::Colon);
        TypeAnno::new(anno_tok, self.ty())
    }

    /// Parse type reference
    ///
    /// Attempt to parse a type. This could be a simple type name, or
    /// it could be a more complex one such as an array type or tuple.
    ///
    /// If no type reference can be found at the current point in the
    /// token stream then a `TypeRef::Missing` is returned.
    fn ty(&mut self) -> TypeRef {
        let current = self.current();
        match &current.kind {
            TokenKind::Word(_) => TypeRef::simple(self.advance()),
            TokenKind::OpenSqBracket => TypeRef::array(
                self.advance(),
                self.ty(),
                self.expect(&TokenKind::CloseSqBracket),
            ),
            TokenKind::OpenBracket => {
                let open = self.advance();
                let mut types = Vec::new();
                if !self.current_is(&TokenKind::CloseBracket) {
                    types.push(self.ty());
                }
                while !self.current_is(&TokenKind::CloseBracket) {
                    // FIXME: This needs to be a delimited list
                    // currently this comma token is getting lost in
                    // the parse tree.
                    let _delim = self.expect(&TokenKind::Comma);
                    types.push(self.ty());
                }
                let close = self.expect(&TokenKind::CloseBracket);
                TypeRef::tuple(open, types, close)
            }
            t => {
                let err = Diagnostic::new(format!("expected type, found: {:?}", t), current.span());
                self.diagnostics.push(err);
                TypeRef::missing()
            }
        }
    }

    /// Parse an optional type annotation
    ///
    /// If there is a type refernece then parse it and return it. If
    /// there is no type we may have to infer it later.
    fn optional_type_anno(&mut self) -> Option<TypeAnno> {
        if self.current_is(&TokenKind::Colon) {
            Some(self.type_anno())
        } else {
            None
        }
    }

    /// Parse an identifier, with an optional type
    fn typed_id(&mut self) -> TypedId {
        // FIXME: This discards the token for the identifier.
        let (_fixme, id) = self.identifier();
        let typ = self.optional_type_anno();
        TypedId::from_parts(id, typ)
    }

    /// Attempt to parse a local declaration
    ///
    /// Parses the body of a local variable delcaration (`let` or
    /// `var`).
    fn declaration(&mut self, var_tok: Token) -> Expression {
        let (_fixme, id) = self.identifier();
        let typ = self.optional_type_anno();
        let assign_tok = self.expect(&TokenKind::Equals);
        let rhs = self.top_level_expression();
        let style = if let TokenKind::Word(Ident::Var) = var_tok.kind {
            VarStyle::Mutable
        } else {
            VarStyle::Immutable
        };
        Expression::declaration(
            var_tok,
            TypedId::from_parts(id, typ),
            style,
            assign_tok,
            rhs,
        )
    }

    /// Parse the contents of a block expression.
    ///
    /// Block expressions are the bodies of functions and loops. They
    /// consist of a seuqence of expressions followed by a closing
    /// `end` token.
    fn block(&mut self) -> BlockBody {
        let mut expressions = Vec::new();
        while !self.current_is_any(&[TokenKind::Word(Ident::End), TokenKind::End]) {
            expressions.push(self.top_level_expression());
        }
        BlockBody {
            contents: Box::new(Expression::sequence(expressions)),
            close: Box::new(self.expect(&TokenKind::Word(Ident::End))),
        }
    }

    /// Prefix Operator
    ///
    /// Parses the trailing expression for a prefix operator.
    fn prefix_op(&mut self, op_token: Token, op: PrefixOp) -> Expression {
        let rhs = self.expression_with_rbp(Token::MAX_LBP);
        Expression::prefix(op_token, op, rhs)
    }

    /// Ternay Body
    ///
    /// The condition and fallback part of a ternary expression.
    fn ternary_body(&mut self) -> (Expression, Token, Expression) {
        let condition = self.top_level_expression();
        let else_tok = self.expect(&TokenKind::Word(Ident::Else));
        let fallback = self.top_level_expression();
        (condition, else_tok, fallback)
    }

    /// Parse Left Denonation
    ///
    /// This is the parse of the symbol when it has an expression to
    /// the left hand side of it. This is responsible for parsing
    /// infix operators and function calls.
    fn parse_led(&mut self, lhs: Expression) -> Expression {
        let token = self.advance();

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
                let index = self.top_level_expression();
                let close = self.expect(&TokenKind::CloseSqBracket);
                Expression::index(lhs, open, index, close)
            }

            // Function call
            TokenKind::OpenBracket => {
                let open = token;
                let mut params = Vec::new();
                while !self.current_is(&TokenKind::CloseBracket) {
                    let param = self.top_level_expression();
                    params.push(param);
                    if !self.current_is(&TokenKind::CloseBracket) {
                        // FIXME: Delimited lists. Should fit in with
                        // tuple parsing. Currently this delimiter
                        // token is getting lost.
                        let _delim = self.expect(&TokenKind::Comma);
                    }
                }
                let close = self.expect(&TokenKind::CloseBracket);
                Expression::call(lhs, open, params, close)
            }

            // Ternay statement:
            // <x> if <y> else <z>
            TokenKind::Word(Ident::If) => {
                let if_tok = token;
                let (condition, else_tok, fallback) = self.ternary_body();
                Expression::if_then_else(
                    if_tok, condition, lhs, else_tok, fallback,
                )
            }

            // Ternay statement:
            // <x> unless <y> else <z>
            TokenKind::Word(Ident::Unless) => {
                let if_tok = token;
                let (condition, else_tok, fallback) = self.ternary_body();
                Expression::if_then_else(
                    if_tok, condition, fallback, else_tok, lhs,
                )
            }

            _ => unreachable!("`parse_led` should only be called if looking at a token with a left binding power."),
        }
    }

    /// Parse a Delimited List of Items
    ///
    /// Returns a list of zero or more elemnets delimited by the given
    /// tokens. Used to parse the parameter list for a function and
    /// the argument list for a call site.
    fn delimited(&mut self, delimiter: TokenKind, close: TokenKind) -> Vec<DelimItem<TypedId>> {
        let mut res = Vec::new();
        if !self.current_is(&close) {
            res.push(DelimItem::First(self.typed_id()));
        }
        while !self.current_is(&close) {
            let delim = self.expect(&delimiter);
            res.push(DelimItem::Follow(delim, self.typed_id()));
        }
        res
    }

    /// Parse Null Denotation
    ///
    /// This is the parse of the symbol when it doesn't have any
    /// expression to the left hand side of it. This is responsible
    /// for parsing literals and variable references into expressions,
    /// as well as parsing prefix expressions.
    fn parse_nud(&mut self) -> Expression {
        let token = self.advance();

        match token.kind {
            TokenKind::Word(Ident::Fn) => {
                let fn_kw = token;
                let (_fixme, identifier) = self.identifier();
                let params_open = self.expect(&TokenKind::OpenBracket);
                let params = self.delimited(TokenKind::Comma, TokenKind::CloseBracket);
                let params_close = self.expect(&TokenKind::CloseBracket);
                let return_type = self.type_anno();
                let body = self.block();
                Expression::function(
                    fn_kw,
                    identifier,
                    params_open,
                    params,
                    params_close,
                    return_type,
                    body,
                )
            }
            TokenKind::Word(Ident::While) | TokenKind::Word(Ident::Until) => {
                let condition = self.top_level_expression();
                let block = self.block();
                Expression::loop_while(token, condition, block)
            }
            TokenKind::Word(Ident::Let) | TokenKind::Word(Ident::Var) => self.declaration(token),
            TokenKind::Word(Ident::Print) => {
                let to_print = self.top_level_expression();
                Expression::print(token, to_print)
            }
            TokenKind::Word(Ident::True) => Expression::constant_bool(token, true),
            TokenKind::Word(Ident::False) => Expression::constant_bool(token, false),
            TokenKind::Word(word) => Expression::identifier(token, word),
            TokenKind::Literal(ref l) => match *l {
                Literal::Number(i) => Expression::constant_num(token, i),
                Literal::RawString(ref s) => {
                    let string_value = s.clone();
                    Expression::constant_string(token, string_value)
                }
            },
            TokenKind::Plus => self.prefix_op(token, PrefixOp::Identity),
            TokenKind::Minus => self.prefix_op(token, PrefixOp::Negate),
            TokenKind::Bang => self.prefix_op(token, PrefixOp::Not),
            TokenKind::OpenBracket => {
                let expr = self.top_level_expression();
                let closing = self.expect(&TokenKind::CloseBracket);
                Expression::grouping(token, expr, closing)
            }
            // This covers things which can't start expressions, like
            // whitespace and non-prefix operator tokens
            _ => {
                let span = token.span();
                if span != DUMMY_SPAN {
                    let err = if token.kind == TokenKind::End {
                        Diagnostic::new("Expected expression but found end of file", span)
                    } else {
                        Diagnostic::new(
                            format!(
                                "unexpected token: expected expression but found {}",
                                token.kind
                            ),
                            span,
                        )
                    };
                    self.diagnostics.push(err);
                }

                // TODO: Unify this with ID stubbing in identifier.
                let stub_id = self.source.intern("0invalid_ident0");
                Expression::identifier(Token::new(TokenKind::Word(stub_id)), stub_id)
            }
        }
    }

    /// Attempt to Parse an Infix Expression
    ///
    /// Given a parsed left hand expression and infix operator parse
    /// the right hand side of that expression. Returns the compound
    /// infix expression.
    fn infix(&mut self, lhs: Expression, token: Token, op: InfixOp) -> Expression {
        let rhs = self.expression_with_rbp(token.lbp());
        Expression::infix(lhs, token, op, rhs)
    }
}
