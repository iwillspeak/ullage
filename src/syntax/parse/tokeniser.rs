//! Tokeniser
//!
//! The tokeniser runs over an input `SourceText` and converts it into
//! a series of `Token`s.

use super::super::text::SourceText;
use super::token::*;

/// Tokeniser
///
/// An object which can run a regex state machine over an input
/// buffer, producing tokens when each new lexeme is matched.
pub(super) struct Tokeniser<'a> {
    buff: &'a str,
    idx: usize,
}

impl<'a> Tokeniser<'a> {
    /// Create a Tokeniser
    ///
    /// Prepares a new token iterator which yields tokens that
    /// reference the given source text.
    pub fn new(source: &'a SourceText) -> Tokeniser<'_> {
        Tokeniser {
            buff: &source.text(),
            idx: 0,
        }
    }

    /// Retrieve the next 'raw' token. This is the next lexical match
    /// in the buffer, and may include whitespace and other trivia
    /// tokens.
    fn next_raw(&mut self) -> Option<Token<'a>> {
        let ts = self.idx;
        let mut te = ts;
        let mut chars = self.buff[ts..].chars();
        let tok = chars.next().map(|c| {
            te += c.len_utf8();
            match c {
                '=' => match chars.next() {
                    Some('=') => {
                        te += 1;
                        Token::DoubleEquals
                    }
                    _ => Token::Equals,
                },
                '!' => match chars.next() {
                    Some('=') => {
                        te += 1;
                        Token::BangEquals
                    }
                    _ => Token::Bang,
                },
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '(' => Token::OpenBracket,
                ')' => Token::CloseBracket,
                '[' => Token::OpenSqBracket,
                ']' => Token::CloseSqBracket,
                ',' => Token::Comma,
                ':' => Token::Colon,
                '<' => Token::LessThan,
                '>' => Token::MoreThan,
                '#' => {
                    te += chars
                        .take_while(|c| *c != '\n')
                        .fold(0, |l, c| l + c.len_utf8());
                    Token::Whitespace(&self.buff[ts..te])
                }
                '0'..='9' => {
                    te += chars.take_while(|c| *c >= '0' && *c <= '9').count();
                    let token_str = &self.buff[ts..te];
                    // we have cheked that it's a valid numeric literal,
                    // so unwrap is fine here.
                    Token::Literal(Literal::Number(token_str.parse::<i64>().unwrap()))
                }
                '\'' => {
                    te += chars
                        .take_while(|c| *c != '\'')
                        .fold(0, |l, c| l + c.len_utf8())
                        + 1;
                    Token::Literal(Literal::RawString(String::from(&self.buff[ts + 1..te - 1])))
                }
                c if c.is_alphabetic() || c == '_' => {
                    te += chars
                        .take_while(|c| c.is_alphanumeric() || *c == '_')
                        .fold(0, |l, c| l + c.len_utf8());
                    Token::Word(&self.buff[ts..te])
                }
                c if c.is_whitespace() => {
                    te += chars
                        .take_while(|c| c.is_whitespace())
                        .fold(0, |l, c| l + c.len_utf8());
                    Token::Whitespace(&self.buff[ts..te])
                }
                _ => Token::Unknown(c),
            }
        });
        self.idx = te;
        tok
    }
}

/// Tokeniser Iterator implementation.
///
/// This allows iterator adapters to be used with the token
/// stream. The next method filters out whitespace tokens from the
/// tokeniser too. This means the `Tokeniser` doesn't have to worry
/// about skipping certain lexemes in the grammar.
impl<'a> Iterator for Tokeniser<'a> {
    type Item = Token<'a>;

    /// Iterator next method. This method returns the next
    /// non-whitespace token in the `Tokeniser`'s stream of `Token`s.
    fn next(&mut self) -> Option<Self::Item> {
        self.next_raw()
    }
}

#[cfg(test)]
mod test {

    use super::super::super::text::SourceText;
    use super::*;

    #[test]
    fn create_tokeniser_from_str_returns_tokeniser() {
        let src = "hello = world";
        Tokeniser::new(&SourceText::new(src));
    }

    #[test]
    fn tokeniser_collect_tokens() {
        let src = SourceText::new("var foo = 'hello world'");
        let tokeniser = Tokeniser::new(&src);
        let tokens = tokeniser.collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::Word("var"),
                Token::Whitespace(" "),
                Token::Word("foo"),
                Token::Whitespace(" "),
                Token::Equals,
                Token::Whitespace(" "),
                Token::Literal(Literal::RawString("hello world".into()))
            ],
            tokens
        );
    }
}
