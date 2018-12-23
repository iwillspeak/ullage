//! Parse Tree Check Tests
//!
//! Tests for the parser which check that a given input matches an
//! exprexpected parse tree.

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
