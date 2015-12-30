mod expression;

use expression::*;

/// Token Class
///
/// Represents a terminal in the grammar.
#[derive(Debug, PartialEq)]
enum Token {
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
struct Tokeniser {
    buff: String,
    idx: usize,
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

    pub fn next(&mut self) -> Option<Token> {

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
        assert_eq!(ts.next(), None);
    }

    #[test]
    pub fn test_operator_tokens() {
        let mut ts = create_tokeniser("+ - * /");
        let mut ops = Vec::new();
        while let Some(Token::Operator(op)) = ts.next() {
            ops.push(op);
        }
        assert_eq!(ops, vec![
            Operator::Add,
            Operator::Sub,
            Operator::Mul,
            Operator::Div]);
    }

    #[test]
    pub fn test_brackets() {
        let mut ts = create_tokeniser("(a * (213 + (b - 99)) / 8)");
        let mut tokens = Vec::new();
        while let Some(token) = ts.next() {
            tokens.push(token);
        }
        assert_eq!(tokens, vec![
            Token::OpenBracket,
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
    pub fn test_siple_number_token() {
        {
            let mut ts = create_tokeniser("1");
            assert_eq!(ts.next(), Some(Token::Number(1)));
        }
        {
            let mut ts = create_tokeniser("5");
            assert_eq!(ts.next(), Some(Token::Number(5)));
        }
        {
            let mut ts = create_tokeniser("0");
            assert_eq!(ts.next(), Some(Token::Number(0)));
        }
        {
            let mut ts = create_tokeniser("9");
            assert_eq!(ts.next(), Some(Token::Number(9)));
        }
    }

    #[test]
    pub fn test_mult_char_numbers() {
        let mut ts = create_tokeniser("123");
        assert_eq!(ts.next(), Some(Token::Number(123)));
    }

    #[test]
    pub fn test_whitespace() {
        let mut ts = create_tokeniser("   2  2 3 45 ");
        let mut nums = Vec::<i64>::new();
        while let Some(Token::Number(value)) = ts.next() {
            nums.push(value);
        }
        assert_eq!(nums, vec![2, 2, 3, 45]);
    }

    #[test]
    pub fn test_simple_identifier_token() {
        let mut ts = create_tokeniser("a");
        assert_eq!(ts.next(), Some(Token::Identifier("a".to_string())));
    }

    #[test]
    pub fn test_identifiers() {
        {
            let s = "_hello_world";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next(), Some(Token::Identifier(s.to_string())));
        }
        {
            let s = "helloWorld";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next(), Some(Token::Identifier(s.to_string())));
        }
        {
            let s = "IDENTIFIER";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next(), Some(Token::Identifier(s.to_string())));
        }
        {
            let s = "u32";
            let mut ts = create_tokeniser(&s);
            assert_eq!(ts.next(), Some(Token::Identifier(s.to_string())));
        }
    }

    #[test]
    pub fn test_create_identifier_token_suceeds() {
        let tok = Token::Identifier("helloworld".to_string());
        if let Token::Identifier(ident) = tok {
            assert_eq!(ident, "helloworld");
        } else {
            assert!(false);
        }
    }

    #[test]
    pub fn test_create_number_token_suceeds() {
        let tok = Token::Number(1234);
        if let Token::Number(num) = tok {
            assert_eq!(num, 1234);
        } else {
            assert!(false);
        }
    }

    #[test]
    pub fn test_create_operator_token_succeeds() {
        let tok = Token::Operator(Operator::Add);
        if let Token::Operator(op) = tok {
            assert_eq!(op, Operator::Add);
        } else {
            assert!(false);
        }
    }

}

fn main() {
    use std::io;
    use std::io::prelude::*;

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let mut ts = Tokeniser::new(&line.unwrap());
        let mut tokens = Vec::new();
        while let Some(token) = ts.next() {
            tokens.push(token);
        }
        println!("{:?}", tokens);
    }
}
