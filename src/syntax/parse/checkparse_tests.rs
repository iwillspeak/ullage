//! Parse Tree Check Tests
//!
//! Tests for the parser which check that a given input matches an
//! exprexpected parse tree.

use super::super::parse::Parser;
use super::super::text::{Ident, SourceText};
use super::super::tree::{Literal, Token, TokenKind};
use super::super::*;

macro_rules! check_parse {
    ($src:expr, |$source:ident| $expected:expr) => {
        let src: &str = $src;
        let $source = SourceText::new(src);
        let mut parser = Parser::new(&$source);
        let tree = parser.parse_single();
        assert_eq!(false, tree.has_diagnostics());
        assert_eq!(&$expected, tree.root());
    };
    ($src:expr, $expected:expr) => {
        check_parse!($src, |t| $expected);
    };
}

/// Create a Simple TypeRef
///
/// Takes the given string, interns it and creates a type reference to
/// that simple type.
///
/// FIXME: Replace with a proper builder API for trees
fn mk_simple_ty(source: &SourceText, simple_name: &str) -> TypeRef {
    TypeRef::simple(syntax_builder::word(source.intern(simple_name)))
}

/// Stub a Type Annotation
///
/// Creates a an annotation with a stubbed `:` token and a fabricated
/// simple type.
fn mk_simple_ty_anno(source: &SourceText, simple_name: &str) -> TypeAnno {
    TypeAnno::new(
        Token::new(TokenKind::Colon),
        mk_simple_ty(source, simple_name),
    )
}

/// Turns a vector of expressions into a dummy block body by pasting a
/// stubbed `Ident::End` on the end.
fn blockify(contents: Vec<Expression>) -> BlockBody {
    BlockBody {
        contents: Box::new(Expression::Sequence(contents)),
        close: Box::new(syntax_builder::word(Ident::End)),
    }
}

#[test]
fn parse_number_literals() {
    check_parse!("0", syntax_builder::const_num(0));
    check_parse!("1", syntax_builder::const_num(1));
    check_parse!("12356", syntax_builder::const_num(12356));
    check_parse!("65536", syntax_builder::const_num(65536));
    check_parse!("4294967296", syntax_builder::const_num(4294967296));
    check_parse!(
        "9223372036854775807",
        syntax_builder::const_num(9223372036854775807)
    );
}

#[test]
fn parse_simple_string() {
    check_parse!("hello + 123", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("hello")),
        Token::new(TokenKind::Plus),
        InfixOp::Add,
        syntax_builder::const_num(123),
    ));
}

#[test]
fn parse_operators() {
    check_parse!("a = b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::Equals),
        InfixOp::Assign,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a + b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::Plus),
        InfixOp::Add,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a - b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::Minus),
        InfixOp::Sub,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a * b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::Star),
        InfixOp::Mul,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a / b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::Slash),
        InfixOp::Div,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a == b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::DoubleEquals),
        InfixOp::Eq,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a != b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::BangEquals),
        InfixOp::NotEq,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a < b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::LessThan),
        InfixOp::Lt,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a <= b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::LessThanEqual),
        InfixOp::LtEq,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a > b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::MoreThan),
        InfixOp::Gt,
        syntax_builder::ident_expr(s.intern("b")),
    ));
    check_parse!("a >= b", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("a")),
        Token::new(TokenKind::MoreThanEqual),
        InfixOp::GtEq,
        syntax_builder::ident_expr(s.intern("b")),
    ));
}

#[test]
fn parse_with_precedence() {
    check_parse!(
        "1 + 2 * 3",
        Expression::infix(
            syntax_builder::const_num(1),
            Token::new(TokenKind::Plus),
            InfixOp::Add,
            Expression::infix(
                syntax_builder::const_num(2),
                Token::new(TokenKind::Star),
                InfixOp::Mul,
                syntax_builder::const_num(3),
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
                Expression::prefix(
                    Token::new(TokenKind::Plus),
                    PrefixOp::Identity,
                    syntax_builder::const_num(1)
                ),
                Token::new(TokenKind::Star),
                InfixOp::Mul,
                Expression::prefix(
                    Token::new(TokenKind::Minus),
                    PrefixOp::Negate,
                    syntax_builder::const_num(2)
                ),
            ),
            Token::new(TokenKind::Plus),
            InfixOp::Add,
            Expression::prefix(
                Token::new(TokenKind::Plus),
                PrefixOp::Identity,
                syntax_builder::const_num(3)
            )
        )
    );
    check_parse!("!a", |s| Expression::prefix(
        Token::new(TokenKind::Bang),
        PrefixOp::Not,
        syntax_builder::ident_expr(s.intern("a"))
    ));
    check_parse!("!a != !b", |s| Expression::infix(
        Expression::prefix(
            Token::new(TokenKind::Bang),
            PrefixOp::Not,
            syntax_builder::ident_expr(s.intern("a"))
        ),
        Token::new(TokenKind::BangEquals),
        InfixOp::NotEq,
        Expression::prefix(
            Token::new(TokenKind::Bang),
            PrefixOp::Not,
            syntax_builder::ident_expr(s.intern("b"))
        ),
    ));
}

#[test]
fn parse_simple_call() {
    check_parse!("foo()", |s| Expression::call(
        syntax_builder::ident_expr(s.intern("foo")),
        Token::new(TokenKind::OpenBracket),
        SepList::empty(),
        Token::new(TokenKind::CloseBracket)
    ));
}

#[test]
fn parse_complex_call() {
    check_parse!("hello(1, 1 + 23, -world)", |s| Expression::call(
        syntax_builder::ident_expr(s.intern("hello")),
        Token::new(TokenKind::OpenBracket),
        SepList::builder()
            .push_item(syntax_builder::const_num(1))
            .push_sep(Token::new(TokenKind::Comma))
            .push_item(Expression::infix(
                syntax_builder::const_num(1),
                Token::new(TokenKind::Plus),
                InfixOp::Add,
                syntax_builder::const_num(23),
            ))
            .push_sep(Token::new(TokenKind::Comma))
            .push_item(Expression::prefix(
                Token::new(TokenKind::Minus),
                PrefixOp::Negate,
                syntax_builder::ident_expr(s.intern("world")),
            ))
            .build(),
        Token::new(TokenKind::CloseBracket),
    ));
}

#[test]
fn parse_groups_with_parens() {
    check_parse!(
        "(1 + 2) * 3",
        Expression::infix(
            Expression::grouping(
                Token::new(TokenKind::OpenBracket),
                Expression::infix(
                    syntax_builder::const_num(1),
                    Token::new(TokenKind::Plus),
                    InfixOp::Add,
                    syntax_builder::const_num(2),
                ),
                Token::new(TokenKind::CloseBracket),
            ),
            Token::new(TokenKind::Star),
            InfixOp::Mul,
            syntax_builder::const_num(3)
        )
    );
}

#[test]
fn parse_indexing() {
    check_parse!("hello[world](1, 2[3])", |s| Expression::call(
        Expression::index(
            syntax_builder::ident_expr(s.intern("hello")),
            Token::new(TokenKind::OpenSqBracket),
            syntax_builder::ident_expr(s.intern("world")),
            Token::new(TokenKind::CloseSqBracket)
        ),
        Token::new(TokenKind::OpenBracket),
        SepList::builder()
            .push_item(syntax_builder::const_num(1))
            .push_sep(Token::new(TokenKind::Comma))
            .push_item(Expression::index(
                syntax_builder::const_num(2),
                Token::new(TokenKind::OpenSqBracket),
                syntax_builder::const_num(3),
                Token::new(TokenKind::CloseSqBracket)
            ))
            .build(),
        Token::new(TokenKind::CloseBracket),
    ));
}

#[test]
fn parse_ternary_if() {
    check_parse!("1 if 2 else 3", |s| Expression::if_then_else(
        syntax_builder::word(s.intern("if")),
        syntax_builder::const_num(2),
        syntax_builder::const_num(1),
        syntax_builder::word(s.intern("else")),
        syntax_builder::const_num(3),
    ));
    check_parse!("hello(1) if foo[23] else world[1 if foo else 2]", |s| {
        Expression::if_then_else(
            syntax_builder::word(s.intern("if")),
            Expression::index(
                syntax_builder::ident_expr(s.intern("foo")),
                Token::new(TokenKind::OpenSqBracket),
                syntax_builder::const_num(23),
                Token::new(TokenKind::CloseSqBracket),
            ),
            Expression::call(
                syntax_builder::ident_expr(s.intern("hello")),
                Token::new(TokenKind::OpenBracket),
                SepList::builder()
                    .push_item(syntax_builder::const_num(1))
                    .build(),
                Token::new(TokenKind::CloseBracket),
            ),
            syntax_builder::word(s.intern("else")),
            Expression::index(
                syntax_builder::ident_expr(s.intern("world")),
                Token::new(TokenKind::OpenSqBracket),
                Expression::if_then_else(
                    syntax_builder::word(s.intern("if")),
                    syntax_builder::ident_expr(s.intern("foo")),
                    syntax_builder::const_num(1),
                    syntax_builder::word(s.intern("else")),
                    syntax_builder::const_num(2),
                ),
                Token::new(TokenKind::CloseSqBracket),
            ),
        )
    });
    check_parse!(
        "0 unless 1 else 2",
        Expression::if_then_else(
            syntax_builder::word(s.intern("unless")),
            syntax_builder::const_num(1),
            syntax_builder::const_num(2),
            syntax_builder::word(s.intern("else")),
            syntax_builder::const_num(0),
        )
    );
}

#[test]
fn parse_unicode_identifiers() {
    check_parse!("  übåℝ * ßeåk  ", |s| Expression::infix(
        syntax_builder::ident_expr(s.intern("übåℝ")),
        Token::new(TokenKind::Star),
        InfixOp::Mul,
        syntax_builder::ident_expr(s.intern("ßeåk")),
    ));
}

#[test]
fn parse_function_def() {
    check_parse!("fn test() :Num 100 end", |s| Expression::function(
        syntax_builder::word(s.intern("fn")),
        s.intern("test"),
        Token::new(TokenKind::OpenBracket),
        SepList::empty(),
        Token::new(TokenKind::CloseBracket),
        mk_simple_ty_anno(&s, "Num"),
        blockify(vec![syntax_builder::const_num(100)])
    ));
    check_parse!(
        "fn ünécød3() :Num
                0 if 74 else 888
             end",
        |s| Expression::function(
            syntax_builder::word(s.intern("fn")),
            s.intern("ünécød3"),
            Token::new(TokenKind::OpenBracket),
            SepList::empty(),
            Token::new(TokenKind::CloseBracket),
            mk_simple_ty_anno(&s, "Num"),
            blockify(vec![Expression::if_then_else(
                syntax_builder::word(s.intern("if")),
                syntax_builder::const_num(74),
                syntax_builder::const_num(0),
                syntax_builder::word(s.intern("else")),
                syntax_builder::const_num(888),
            )])
        )
    );
}

#[test]
fn parse_while_loop() {
    check_parse!("while 1 end", |s| Expression::loop_while(
        syntax_builder::word(s.intern("while")),
        syntax_builder::const_num(1),
        blockify(Vec::new())
    ));
    check_parse!("while 0 44 234 end", |s| Expression::loop_while(
        syntax_builder::word(s.intern("while")),
        syntax_builder::const_num(0),
        blockify(vec![
            syntax_builder::const_num(44),
            syntax_builder::const_num(234)
        ]),
    ));
}

#[test]
fn parse_function_with_args() {
    check_parse!("fn neg(i: Num): Num - i end", |s| Expression::function(
        syntax_builder::word(s.intern("fn")),
        s.intern("neg"),
        Token::new(TokenKind::OpenBracket),
        SepList::builder()
            .push_item(TypedId::new(
                syntax_builder::word(s.intern("i")),
                mk_simple_ty_anno(&s, "Num")
            ))
            .build(),
        Token::new(TokenKind::CloseBracket),
        mk_simple_ty_anno(&s, "Num"),
        blockify(vec![Expression::prefix(
            Token::new(TokenKind::Minus),
            PrefixOp::Negate,
            syntax_builder::ident_expr(s.intern("i")),
        )])
    ));

    check_parse!("fn test(i: Num, j, k: String): String i + j + k end", |s| {
        Expression::function(
            syntax_builder::word(s.intern("fn")),
            s.intern("test"),
            Token::new(TokenKind::OpenBracket),
            SepList::builder()
                .push_item(TypedId::new(
                    syntax_builder::word(s.intern("i")),
                    mk_simple_ty_anno(&s, "Num"),
                ))
                .push_sep(Token::new(TokenKind::Comma))
                .push_item(TypedId::new_without_type(syntax_builder::word(
                    s.intern("j"),
                )))
                .push_sep(Token::new(TokenKind::Comma))
                .push_item(TypedId::new(
                    syntax_builder::word(s.intern("k")),
                    mk_simple_ty_anno(&s, "String"),
                ))
                .build(),
            Token::new(TokenKind::CloseBracket),
            mk_simple_ty_anno(&s, "String"),
            blockify(vec![Expression::infix(
                Expression::infix(
                    syntax_builder::ident_expr(s.intern("i")),
                    Token::new(TokenKind::Plus),
                    InfixOp::Add,
                    syntax_builder::ident_expr(s.intern("j")),
                ),
                Token::new(TokenKind::Plus),
                InfixOp::Add,
                syntax_builder::ident_expr(s.intern("k")),
            )]),
        )
    });
}

#[test]
fn parse_simple_array_type() {
    check_parse!("let f: [Num] = 100", |s| Expression::declaration(
        syntax_builder::word(s.intern("let")),
        TypedId::from_parts(
            syntax_builder::word(s.intern("f")),
            Some(TypeAnno::new(
                Token::new(TokenKind::Colon),
                TypeRef::array(
                    Token::new(TokenKind::OpenSqBracket),
                    mk_simple_ty(&s, "Num"),
                    Token::new(TokenKind::CloseSqBracket)
                )
            )),
        ),
        VarStyle::Immutable,
        Token::new(TokenKind::Equals),
        syntax_builder::const_num(100),
    ));
}

#[test]
fn parse_simple_let() {
    check_parse!("let foo = 100", |s| Expression::declaration(
        syntax_builder::word(s.intern("let")),
        TypedId::from_parts(syntax_builder::word(s.intern("foo")), None),
        VarStyle::Immutable,
        Token::new(TokenKind::Equals),
        syntax_builder::const_num(100),
    ));
}

#[test]
fn parse_simple_tuple() {
    check_parse!("let f: (Num) = 100", |s| Expression::declaration(
        syntax_builder::word(s.intern("let")),
        TypedId::from_parts(
            syntax_builder::word(s.intern("f")),
            Some(TypeAnno::new(
                Token::new(TokenKind::Colon),
                TypeRef::tuple(
                    Token::new(TokenKind::OpenBracket),
                    SepList::builder()
                        .push_item(mk_simple_ty(&s, "Num"))
                        .build(),
                    Token::new(TokenKind::CloseBracket)
                )
            )),
        ),
        VarStyle::Immutable,
        Token::new(TokenKind::Equals),
        syntax_builder::const_num(100),
    ));
    check_parse!("let f: (Num, [String]) = 100", |s| Expression::declaration(
        syntax_builder::word(s.intern("let")),
        TypedId::from_parts(
            syntax_builder::word(s.intern("f")),
            Some(TypeAnno::new(
                Token::new(TokenKind::Colon),
                TypeRef::tuple(
                    Token::new(TokenKind::OpenBracket),
                    SepList::builder()
                        .push_item(mk_simple_ty(&s, "Num"))
                        .push_sep(Token::new(TokenKind::Comma))
                        .push_item(TypeRef::array(
                            Token::new(TokenKind::OpenSqBracket),
                            mk_simple_ty(&s, "String"),
                            Token::new(TokenKind::CloseSqBracket)
                        ))
                        .build(),
                    Token::new(TokenKind::CloseBracket)
                )
            )),
        ),
        VarStyle::Immutable,
        Token::new(TokenKind::Equals),
        syntax_builder::const_num(100),
    ));
}

#[test]
fn parse_variable_decl() {
    check_parse!("var foo = 93", |s| Expression::declaration(
        syntax_builder::word(s.intern("var")),
        TypedId::from_parts(syntax_builder::word(s.intern("foo")), None),
        VarStyle::Mutable,
        Token::new(TokenKind::Equals),
        syntax_builder::const_num(93),
    ));
    check_parse!("var foo_bar: Number = -99999", |s| Expression::declaration(
        syntax_builder::word(s.intern("var")),
        TypedId::from_parts(
            syntax_builder::word(s.intern("foo_bar")),
            Some(mk_simple_ty_anno(&s, "Number"))
        ),
        VarStyle::Mutable,
        Token::new(TokenKind::Equals),
        Expression::prefix(
            Token::new(TokenKind::Minus),
            PrefixOp::Negate,
            syntax_builder::const_num(99999)
        ),
    ));
}

#[test]
fn parse_print_operator() {
    check_parse!("print 1334", |s| Expression::print(
        syntax_builder::word(s.intern("print")),
        syntax_builder::const_num(1334)
    ));
}

#[test]
fn parse_bool_literal() {
    check_parse!("true", |s| Expression::constant_bool(
        syntax_builder::word(s.intern("true")),
        true
    ));
    check_parse!("false", |s| Expression::constant_bool(
        syntax_builder::word(s.intern("false")),
        false
    ));
    check_parse!("true", syntax_builder::const_bool(true));
    check_parse!("false", syntax_builder::const_bool(false));
}

#[test]
fn parse_string_literal() {
    check_parse!(
        "'hello'",
        Expression::constant_string(
            Token::new(TokenKind::Literal(Literal::RawString("hello".into()))),
            "hello"
        )
    );
    check_parse!(
        "'über ∂elta'",
        Expression::constant_string(
            Token::new(TokenKind::Literal(Literal::RawString("über ∂elta".into()))),
            "über ∂elta"
        )
    );
}

#[test]
fn parse_string_literal_syntax_builder() {
    check_parse!("'foobar'", syntax_builder::raw_string("foobar"));
    check_parse!("'münchen'", syntax_builder::raw_string("münchen"));
}
