use super::expression::Operator;

/// Token Class
///
/// Represents a terminal in the grammar.
#[derive(Debug, PartialEq)]
pub enum Token {
    /// Identifier
    ///
    /// Represents a reference to a variable.
    Identifier(String),

    /// Number
    ///
    /// Represents a numerical constant
    Number(i64),

    /// Operator
    ///
    /// Represents an operator
    Operator(Operator),

    /// Open Bracket
    ///
    /// Opening bracket.
    OpenBracket,

    /// Close Bracket
    ///
    /// Closing bracket
    CloseBracket,
}

/// Tokeniser
///
pub struct Tokeniser {
    buff: String,
    idx: usize,
}

/// Tokeniser Iterator Implementation
///
/// A tokeniser can be used as a iterator. This means that tokens can
/// be filtered, mapped and buffered to support `peek()`.
impl Iterator for Tokeniser {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }
}

impl Tokeniser {
    /// Create a Tokeniser
    ///
    /// Creates a new tokeniser instance for the given string. This
    /// tokeniser will then yield tokens lexed from the string.
    pub fn new(string: &str) -> Self {
        Tokeniser {
            buff: string.to_string(),
            idx: 0,
        }
    }

    fn next_token(&mut self) -> Option<Token> {

        // skip over any leading whitespace
        self.idx += self.buff[self.idx..]
                        .chars()
                        .take_while(|c| c.is_whitespace())
                        .count();

        let ts = self.idx;
        let mut te = ts;
        let mut chars = self.buff[ts..].chars();
        let token = chars.next().and_then(|c| {
            te += 1;
            match c {
                '+' => Some(Token::Operator(Operator::Add)),
                '-' => Some(Token::Operator(Operator::Sub)),
                '*' => Some(Token::Operator(Operator::Mul)),
                '/' => Some(Token::Operator(Operator::Div)),
                '(' => Some(Token::OpenBracket),
                ')' => Some(Token::CloseBracket),
                '0'...'9' => {
                    te += chars.take_while(|c| *c >= '0' && *c <= '9').count();
                    let token_str = &self.buff[ts..te];
                    // we have cheked that it's a valid numeric literal,
                    // so unwrap is fine here.
                    Some(Token::Number(token_str.parse::<i64>().unwrap()))
                }
                c if c.is_alphabetic() || c == '_' => {
                    te += chars.take_while(|c| c.is_alphanumeric() || *c == '_')
                               .count();
                    let token_str = &self.buff[ts..te];
                    Some(Token::Identifier(token_str.to_string()))
                }
                _ => None,
            }
        });
        self.idx = te;
        token
    }
}

#[cfg(test)]
mod test {

    use expression::Operator;
    use super::{Token, Tokeniser};

    fn create_tokeniser(str: &str) -> Tokeniser {
        Tokeniser::new(str)
    }

    #[test]
    pub fn test_empty_string_returns_none_token() {
        let mut ts = create_tokeniser("");
        assert_eq!(ts.next_token(), None);
    }

    #[test]
    pub fn test_operator_tokens() {
        let mut ts = create_tokeniser("+ - * /");
        let mut ops = Vec::new();
        while let Some(Token::Operator(op)) = ts.next_token() {
            ops.push(op);
        }
        assert_eq!(ops,
                   [Operator::Add, Operator::Sub, Operator::Mul, Operator::Div]);
    }

    #[test]
    pub fn test_brackets() {
        let ts = create_tokeniser("(a * (213 + (b - 99)) / 8)");
        let tokens: Vec<Token> = ts.collect();

        assert_eq!(tokens,
                   [Token::OpenBracket,
                    Token::Identifier("a".to_string()),
                    Token::Operator(Operator::Mul),
                    Token::OpenBracket,
                    Token::Number(213),
                    Token::Operator(Operator::Add),
                    Token::OpenBracket,
                    Token::Identifier("b".to_string()),
                    Token::Operator(Operator::Sub),
                    Token::Number(99),
                    Token::CloseBracket,
                    Token::CloseBracket,
                    Token::Operator(Operator::Div),
                    Token::Number(8),
                    Token::CloseBracket]);
    }

    #[test]
    pub fn test_number_tokenisation() {
        {
            let mut ts = create_tokeniser("1");
            assert_eq!(ts.next_token(), Some(Token::Number(1)));
        }
        {
            let mut ts = create_tokeniser("5");
            assert_eq!(ts.next_token(), Some(Token::Number(5)));
        }
        {
            let mut ts = create_tokeniser("0");
            assert_eq!(ts.next_token(), Some(Token::Number(0)));
        }
        {
            let mut ts = create_tokeniser("9");
            assert_eq!(ts.next_token(), Some(Token::Number(9)));
        }
        {
            let mut ts = create_tokeniser("123");
            assert_eq!(ts.next_token(), Some(Token::Number(123)));
        }
        {
            let mut ts = create_tokeniser("   2  2 3 45 ");
            let mut nums = Vec::new();
            while let Some(Token::Number(value)) = ts.next_token() {
                nums.push(value);
            }
            assert_eq!(nums, [2, 2, 3, 45]);
        }
    }

    #[test]
    pub fn test_simple_identifier_token() {
        let mut ts = create_tokeniser("a");
        assert_eq!(ts.next_token(), Some(Token::Identifier("a".to_string())));
    }

    #[test]
    pub fn test_identifiers() {
        {
            let s = "_hello_world";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next_token(), Some(Token::Identifier(s.to_string())));
        }
        {
            let s = "helloWorld";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next_token(), Some(Token::Identifier(s.to_string())));
        }
        {
            let s = "IDENTIFIER";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next_token(), Some(Token::Identifier(s.to_string())));
        }
        {
            let s = "u32";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next_token(), Some(Token::Identifier(s.to_string())));
        }
        {
            let tok = Token::Identifier("helloworld".to_string());
            if let Token::Identifier(ident) = tok {
                assert_eq!(ident, "helloworld");
            } else {
                panic!();
            }
        }
    }

    #[test]
    pub fn test_create_number_token_suceeds() {
        let tok = Token::Number(1234);
        if let Token::Number(num) = tok {
            assert_eq!(num, 1234);
        } else {
            panic!();
        }
    }

    #[test]
    pub fn test_create_operator_token_succeeds() {
        let tok = Token::Operator(Operator::Add);
        if let Token::Operator(op) = tok {
            assert_eq!(op, Operator::Add);
        } else {
            panic!();
        }
    }

    #[test]
    pub fn test_tokeniser_iterator() {
        let ts = create_tokeniser("hello world");
        let mut pts = ts.map(|t| {
                            match t {
                                Token::Identifier(id) => id,
                                _ => "fail".to_string(),
                            }
                        })
                        .peekable();
        assert_eq!(pts.peek().unwrap(), "hello");
        assert_eq!(pts.next().unwrap(), "hello");
        assert_eq!(pts.peek().unwrap(), "world");
    }
}
