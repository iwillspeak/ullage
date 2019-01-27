//! Parse Tree Check Tests
//!
//! Tests for the parser which check that a given input matches an
//! exprexpected parse tree.

use super::super::text::{Ident, SourceText};
use super::super::tree::{Literal, Token, TokenKind};
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

/// Creates an Identifier Expression
///
/// This funciton handles interning the indentifier and creating a
/// mock token for the idnetifier expression to use.
fn mk_ident(source: &SourceText, id: &str) -> Expression {
    let id = source.intern(id);
    Expression::identifier(Token::new(TokenKind::Word(id)), id)
}

/// Turns a vector of expressions into a dummy block body by pasting a
/// stubbed `Ident::End` on the end.
fn blockify(contents: Vec<Expression>) -> BlockBody {
    BlockBody {
        contents: Box::new(Expression::Sequence(contents)),
        close: Box::new(Token::new(TokenKind::Word(Ident::End))),
    }
}

#[test]
fn parse_simple_string() {
    check_parse!("hello + 123", |s| Expression::infix(
        mk_ident(&s, "hello"),
        Token::new(TokenKind::Plus),
        InfixOp::Add,
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(123))), 123),
    ));
}

#[test]
fn parse_operators() {
    check_parse!("a = b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::Equals),
        InfixOp::Assign,
        mk_ident(&s, "b"),
    ));
    check_parse!("a + b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::Plus),
        InfixOp::Add,
        mk_ident(&s, "b"),
    ));
    check_parse!("a - b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::Minus),
        InfixOp::Sub,
        mk_ident(&s, "b"),
    ));
    check_parse!("a * b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::Star),
        InfixOp::Mul,
        mk_ident(&s, "b"),
    ));
    check_parse!("a / b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::Slash),
        InfixOp::Div,
        mk_ident(&s, "b"),
    ));
    check_parse!("a == b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::DoubleEquals),
        InfixOp::Eq,
        mk_ident(&s, "b"),
    ));
    check_parse!("a != b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::BangEquals),
        InfixOp::NotEq,
        mk_ident(&s, "b"),
    ));
    check_parse!("a < b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::LessThan),
        InfixOp::Lt,
        mk_ident(&s, "b"),
    ));
    check_parse!("a <= b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::LessThanEqual),
        InfixOp::LtEq,
        mk_ident(&s, "b"),
    ));
    check_parse!("a > b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::MoreThan),
        InfixOp::Gt,
        mk_ident(&s, "b"),
    ));
    check_parse!("a >= b", |s| Expression::infix(
        mk_ident(&s, "a"),
        Token::new(TokenKind::MoreThanEqual),
        InfixOp::GtEq,
        mk_ident(&s, "b"),
    ));
}

#[test]
fn parse_with_precedence() {
    check_parse!(
        "1 + 2 * 3",
        Expression::infix(
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
            Token::new(TokenKind::Plus),
            InfixOp::Add,
            Expression::infix(
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2),
                Token::new(TokenKind::Star),
                InfixOp::Mul,
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(3))), 3),
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
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
                Token::new(TokenKind::Star),
                InfixOp::Mul,
                Expression::prefix(
                    Token::new(TokenKind::Minus),
                    PrefixOp::Negate,
                    Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2)
                ),
            ),
            Token::new(TokenKind::Plus),
            InfixOp::Add,
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(3))), 3),
        )
    );
    check_parse!("!a", |s| Expression::prefix(
        Token::new(TokenKind::Bang),
        PrefixOp::Not,
        mk_ident(&s, "a")
    ));
    check_parse!("!a != !b", |s| Expression::infix(
        Expression::prefix(
            Token::new(TokenKind::Bang),
            PrefixOp::Not,
            mk_ident(&s, "a")
        ),
        Token::new(TokenKind::BangEquals),
        InfixOp::NotEq,
        Expression::prefix(
            Token::new(TokenKind::Bang),
            PrefixOp::Not,
            mk_ident(&s, "b")
        ),
    ));
}

#[test]
fn parse_simple_call() {
    check_parse!("foo()", |s| Expression::call(
        mk_ident(&s, "foo"),
        Token::new(TokenKind::OpenBracket),
        Vec::new(),
        Token::new(TokenKind::CloseBracket)
    ));
}

#[test]
fn parse_complex_call() {
    check_parse!("hello(1, 1 + 23, -world)", |s| Expression::call(
        mk_ident(&s, "hello"),
        Token::new(TokenKind::OpenBracket),
        vec![
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
            Expression::infix(
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
                Token::new(TokenKind::Plus),
                InfixOp::Add,
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(23))), 23),
            ),
            Expression::prefix(
                Token::new(TokenKind::Minus),
                PrefixOp::Negate,
                mk_ident(&s, "world"),
            ),
        ],
        Token::new(TokenKind::CloseBracket),
    ));
}

#[test]
fn parse_groups_with_parens() {
    check_parse!(
        "(1 + 2) * 3",
        Expression::infix(
            Grouping(
                Box::new(Token::new(TokenKind::OpenBracket)),
                Box::new(Expression::infix(
                    Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
                    Token::new(TokenKind::Plus),
                    InfixOp::Add,
                    Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2),
                )),
                Box::new(Token::new(TokenKind::CloseBracket)),
            ),
            Token::new(TokenKind::Star),
            InfixOp::Mul,
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(3))), 3)
        )
    );
}

#[test]
fn parse_indexing() {
    check_parse!("hello[world](1, 2[3])", |s| Expression::call(
        Expression::index(
            mk_ident(&s, "hello"),
            Token::new(TokenKind::OpenSqBracket),
            mk_ident(&s, "world"),
            Token::new(TokenKind::CloseSqBracket)
        ),
        Token::new(TokenKind::OpenBracket),
        vec![
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
            Expression::index(
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2),
                Token::new(TokenKind::OpenSqBracket),
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(3))), 3),
                Token::new(TokenKind::CloseSqBracket)
            ),
        ],
        Token::new(TokenKind::CloseBracket),
    ));
}

#[test]
fn parse_ternary_if() {
    check_parse!(
        "1 if 2 else 3",
        Expression::if_then_else(
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2),
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(3))), 3),
        )
    );
    check_parse!("hello(1) if foo[23] else world[1 if foo else 2]", |s| {
        Expression::if_then_else(
            Expression::index(
                mk_ident(&s, "foo"),
                Token::new(TokenKind::OpenSqBracket),
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(23))), 23),
                Token::new(TokenKind::CloseSqBracket),
            ),
            Expression::call(
                mk_ident(&s, "hello"),
                Token::new(TokenKind::OpenBracket),
                vec![Expression::constant_num(
                    Token::new(TokenKind::Literal(Literal::Number(1))),
                    1,
                )],
                Token::new(TokenKind::CloseBracket),
            ),
            Expression::index(
                mk_ident(&s, "world"),
                Token::new(TokenKind::OpenSqBracket),
                Expression::if_then_else(
                    mk_ident(&s, "foo"),
                    Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
                    Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2),
                ),
                Token::new(TokenKind::CloseSqBracket),
            ),
        )
    });
    check_parse!(
        "0 unless 1 else 2",
        Expression::if_then_else(
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(2))), 2),
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(0))), 0),
        )
    );
}

#[test]
fn parse_unicode_identifiers() {
    check_parse!("  übåℝ * ßeåk  ", |s| Expression::infix(
        mk_ident(&s, "übåℝ"),
        Token::new(TokenKind::Star),
        InfixOp::Mul,
        mk_ident(&s, "ßeåk"),
    ));
}

#[test]
fn parse_function_def() {
    check_parse!("fn test() :Num 100 end", |s| Expression::function(
        s.intern("test")
    )
    .with_return_type(TypeRef::simple("Num"))
    .with_body(blockify(vec![Expression::constant_num(
        Token::new(TokenKind::Literal(Literal::Number(100))),
        100
    )]))
    .into());
    check_parse!(
        "fn ünécød3() :Num
                0 if 74 else 888
             end",
        |s| Expression::function(s.intern("ünécød3"))
            .with_return_type(TypeRef::simple("Num"))
            .with_body(blockify(vec![Expression::if_then_else(
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(74))), 74),
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(0))), 0),
                Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(888))), 888),
            )]))
            .into()
    );
}

#[test]
fn parse_while_loop() {
    check_parse!("while 1 end", |s| Expression::loop_while(
        Token::new(TokenKind::Word(s.intern("while"))),
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1))), 1),
        blockify(Vec::new())
    ));
    check_parse!("while 0 44 234 end", |s| Expression::loop_while(
        Token::new(TokenKind::Word(s.intern("while"))),
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(0))), 0),
        blockify(vec![
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(44))), 44),
            Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(234))), 234)
        ]),
    ));
}

#[test]
fn parse_function_with_args() {
    check_parse!("fn neg(i: Num): Num - i end", |s| Expression::function(
        s.intern("neg")
    )
    .with_arg(TypedId::new(s.intern("i"), TypeRef::simple("Num")))
    .with_return_type(TypeRef::simple("Num"))
    .with_body(blockify(vec![Expression::prefix(
        Token::new(TokenKind::Minus),
        PrefixOp::Negate,
        mk_ident(&s, "i"),
    )]))
    .into());

    check_parse!("fn test(i: Num, j, k: String): String i + j + k end", |s| {
        Expression::function(s.intern("test"))
            .with_arg(TypedId::new(s.intern("i"), TypeRef::simple("Num")))
            .with_arg(TypedId::new_without_type(s.intern("j")))
            .with_arg(TypedId::new(s.intern("k"), TypeRef::simple("String")))
            .with_return_type(TypeRef::simple("String"))
            .with_body(blockify(vec![Expression::infix(
                Expression::infix(
                    mk_ident(&s, "i"),
                    Token::new(TokenKind::Plus),
                    InfixOp::Add,
                    mk_ident(&s, "j"),
                ),
                Token::new(TokenKind::Plus),
                InfixOp::Add,
                mk_ident(&s, "k"),
            )]))
            .into()
    });
}

#[test]
fn parse_simple_array_type() {
    check_parse!("let f: [Num] = 100", |s| Expression::declaration(
        TypedId::from_parts(s.intern("f"), Some(TypeRef::array(TypeRef::simple("Num"))),),
        false,
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(100))), 100),
    ));
}

#[test]
fn parse_simple_let() {
    check_parse!("let foo = 100", |s| Expression::declaration(
        TypedId::from_parts(s.intern("foo"), None),
        false,
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(100))), 100),
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
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(100))), 100),
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
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(100))), 100),
    ));
}

#[test]
fn parse_variable_decl() {
    check_parse!("var foo = 93", |s| Expression::declaration(
        TypedId::from_parts(s.intern("foo"), None),
        true,
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(93))), 93),
    ));
    check_parse!("var foo_bar: Number = -99999", |s| Expression::declaration(
        TypedId::from_parts(s.intern("foo_bar"), Some(TypeRef::simple("Number"))),
        true,
        Expression::prefix(
            Token::new(TokenKind::Minus),
            PrefixOp::Negate,
            Expression::constant_num(
                Token::new(TokenKind::Literal(Literal::Number(99999))),
                99999
            )
        ),
    ));
}

#[test]
fn parse_print_operator() {
    check_parse!("print 1334", |s| Expression::print(
        Token::new(TokenKind::Word(s.intern("print"))),
        Expression::constant_num(Token::new(TokenKind::Literal(Literal::Number(1334))), 1334)
    ));
}

#[test]
fn parse_bool_literal() {
    check_parse!("true", |s| Expression::constant_bool(
        Token::new(TokenKind::Word(s.intern("true"))),
        true
    ));
    check_parse!("false", |s| Expression::constant_bool(
        Token::new(TokenKind::Word(s.intern("false"))),
        false
    ));
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
            Token::new(TokenKind::Literal(Literal::RawString(
                "über ∂elta".into()
            ))),
            "über ∂elta"
        )
    );
}
