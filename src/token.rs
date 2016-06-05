use super::{Parser,ParseResult,ParseError,Expression,Operator};

/// Token Class
///
/// Represents a terminal in the grammar.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    /// Identifier
    ///
    /// Represents a reference to a variable, type or other such word.
    Identifier(String),

    /// Number
    ///
    /// Represents a numerical constant
    Number(i64),

    /// Plus
    ///
    /// The `+` character
    Plus,

    /// Minus
    ///
    /// The `-` character
    Minus,

    /// Star
    ///
    /// The `*` character
    Star,

    /// Slash
    ///
    /// The `/` character
    Slash,

    /// Equals
    ///
    /// The `=` character
    Equals,

    /// Open Bracket
    ///
    /// Opening `(` bracket.
    OpenBracket,

    /// Close Bracket
    ///
    /// Closing `)` bracket
    CloseBracket,
}

impl Token {
    pub fn prefix(&self, parser: &mut Parser) -> ParseResult<Expression> {
        match *self {
            Token::Identifier(ref id) => Ok(Expression::from_ident(&id)),
            Token::Number(n) => Ok(Expression::from_value(n)),
            Token::Plus => {
                let suffix = try!(parser.parse(100));
                Ok(Expression::from_prefix_op(Operator::Add, suffix))
            }
            Token::OpenBracket => {
                let expr = try!(parser.parse(0));
                try!(parser.expect(Token::CloseBracket));
                Ok(expr)
            }
            ref t => {
                Err(ParseError::Unexpected {
                    expecting: "Identifier, number or '('".to_string(),
                    found: Some(t.clone()),
                })
            }
        }
    }
}
