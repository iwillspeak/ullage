//! Parse Tree Check Tests
//!
//! Tests for the parser which check that a given input matches an
//! exprexpected parse tree.

use super::super::text::SourceText;
use super::super::tree::{Token, TokenKind};
use super::super::Expression::*;
use super::super::*;
use super::parse_single;

macro_rules! check_parse {
    ($src:expr, |$source:ident| $expected:expr) => {
        let src: &str = $src;
        let $source = SourceText::new(src);
        let actual = parse_single(&$source);
        assert_eq!(Ok($expected), actual);
    };
    ($src:expr, $expected:expr) => {
        check_parse!($src, |t| $expected);
    };
}

#[test]
fn parse_simple_string() {
    check_parse!("hello + 123", |s| Expression::infix(
        Expression::identifier(s.intern("hello")),
        InfixOp::Add,
        Expression::constant_num(123),
    ));
}

#[test]
fn parse_operators() {
    check_parse!("a = b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Assign,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a + b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Add,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a - b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Sub,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a * b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Mul,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a / b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Div,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a == b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Eq,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a != b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::NotEq,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a < b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Lt,
        Expression::identifier(s.intern("b")),
    ));
    check_parse!("a > b", |s| Expression::infix(
        Expression::identifier(s.intern("a")),
        InfixOp::Gt,
        Expression::identifier(s.intern("b")),
    ));
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
    check_parse!("!a", |s| Expression::prefix(
        PrefixOp::Not,
        Expression::identifier(s.intern("a"))
    ));
    check_parse!("!a != !b", |s| Expression::infix(
        Expression::prefix(PrefixOp::Not, Expression::identifier(s.intern("a"))),
        InfixOp::NotEq,
        Expression::prefix(PrefixOp::Not, Expression::identifier(s.intern("b"))),
    ));
}

#[test]
fn parse_simple_call() {
    check_parse!("foo()", |s| Expression::call(
        Expression::identifier(s.intern("foo")),
        Vec::new()
    ));
}

#[test]
fn parse_complex_call() {
    check_parse!("hello(1, 1 + 23, -world)", |s| Expression::call(
        Expression::identifier(s.intern("hello")),
        vec![
            Expression::constant_num(1),
            Expression::infix(
                Expression::constant_num(1),
                InfixOp::Add,
                Expression::constant_num(23),
            ),
            Expression::prefix(PrefixOp::Negate, Expression::identifier(s.intern("world")),),
        ],
    ));
}

#[test]
fn parse_groups_with_parens() {
    check_parse!(
        "(1 + 2) * 3",
        Infix(
            Box::new(Grouping(
                Box::new(Token::new(TokenKind::OpenBracket)),
                Box::new(Infix(
                    Box::new(Expression::constant_num(1)),
                    InfixOp::Add,
                    Box::new(Expression::constant_num(2)),
                )),
                Box::new(Token::new(TokenKind::CloseBracket)),
            )),
            InfixOp::Mul,
            Box::new(Expression::constant_num(3)),
        )
    );
}

#[test]
fn parse_indexing() {
    check_parse!("hello[world](1, 2[3])", |s| Expression::call(
        Expression::index(
            Expression::identifier(s.intern("hello")),
            Expression::identifier(s.intern("world")),
        ),
        vec![
            Expression::constant_num(1),
            Expression::index(Expression::constant_num(2), Expression::constant_num(3)),
        ],
    ));
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
    check_parse!("hello(1) if foo[23] else world[1 if foo else 2]", |s| {
        Expression::if_then_else(
            Expression::index(
                Expression::identifier(s.intern("foo")),
                Expression::constant_num(23),
            ),
            Expression::call(
                Expression::identifier(s.intern("hello")),
                vec![Expression::constant_num(1)],
            ),
            Expression::index(
                Expression::identifier(s.intern("world")),
                Expression::if_then_else(
                    Expression::identifier(s.intern("foo")),
                    Expression::constant_num(1),
                    Expression::constant_num(2),
                ),
            ),
        )
    });
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
    check_parse!("  übåℝ * ßeåk  ", |s| Expression::infix(
        Expression::identifier(s.intern("übåℝ")),
        InfixOp::Mul,
        Expression::identifier(s.intern("ßeåk")),
    ));
}

#[test]
fn parse_function_def() {
    check_parse!("fn test() :Num 100 end", |s| Expression::function(
        s.intern("test")
    )
    .with_return_type(TypeRef::simple("Num"))
    .with_body(vec![Expression::constant_num(100)])
    .into());
    check_parse!(
        "fn ünécød3() :Num
                0 if 74 else 888
             end",
        |s| Expression::function(s.intern("ünécød3"))
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
    check_parse!("fn neg(i: Num): Num - i end", |s| Expression::function(
        s.intern("neg")
    )
    .with_arg(TypedId::new(s.intern("i"), TypeRef::simple("Num")))
    .with_return_type(TypeRef::simple("Num"))
    .with_body(vec![Expression::prefix(
        PrefixOp::Negate,
        Expression::identifier(s.intern("i")),
    )])
    .into());

    check_parse!("fn test(i: Num, j, k: String): String i + j + k end", |s| {
        Expression::function(s.intern("test"))
            .with_arg(TypedId::new(s.intern("i"), TypeRef::simple("Num")))
            .with_arg(TypedId::new_without_type(s.intern("j")))
            .with_arg(TypedId::new(s.intern("k"), TypeRef::simple("String")))
            .with_return_type(TypeRef::simple("String"))
            .with_body(vec![Expression::infix(
                Expression::infix(
                    Expression::identifier(s.intern("i")),
                    InfixOp::Add,
                    Expression::identifier(s.intern("j")),
                ),
                InfixOp::Add,
                Expression::identifier(s.intern("k")),
            )])
            .into()
    });
}

#[test]
fn parse_simple_array_type() {
    check_parse!("let f: [Num] = 100", |s| Expression::declaration(
        TypedId::from_parts(s.intern("f"), Some(TypeRef::array(TypeRef::simple("Num"))),),
        false,
        Expression::constant_num(100),
    ));
}

#[test]
fn parse_simple_let() {
    check_parse!("let foo = 100", |s| Expression::declaration(
        TypedId::from_parts(s.intern("foo"), None),
        false,
        Expression::constant_num(100),
    ));
}

#[test]
fn parse_simple_tuple() {
    check_parse!("let f: (Num) = 100", |s| Expression::declaration(
        TypedId::from_parts(
            s.intern("f"),
            Some(TypeRef::tuple(vec![TypeRef::simple("Num")])),
        ),
        false,
        Expression::constant_num(100),
    ));
    check_parse!("let f: (Num, [String]) = 100", |s| Expression::declaration(
        TypedId::from_parts(
            s.intern("f"),
            Some(TypeRef::tuple(vec![
                TypeRef::simple("Num"),
                TypeRef::array(TypeRef::simple("String")),
            ])),
        ),
        false,
        Expression::constant_num(100),
    ));
}

#[test]
fn parse_variable_decl() {
    check_parse!("var foo = 93", |s| Expression::declaration(
        TypedId::from_parts(s.intern("foo"), None),
        true,
        Expression::constant_num(93),
    ));
    check_parse!("var foo_bar: Number = -99999", |s| Expression::declaration(
        TypedId::from_parts(s.intern("foo_bar"), Some(TypeRef::simple("Number"))),
        true,
        Expression::prefix(PrefixOp::Negate, Expression::constant_num(99999)),
    ));
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
