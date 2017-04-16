use syntax::ast::token::Token;
use syntax::parse::Source;

/// TokenIterator
///
/// An object which can run a regex state machine over an input
/// buffer, producing tokens when each new lexeme is matched.
pub struct TokenIterator<'a, S>
    where S: Source<'a>,
          S: 'a
{
    src: S,
    idx: S::Pos,
    phantom: ::std::marker::PhantomData<&'a S>,
}

impl<'a, S> TokenIterator<'a, S>
    where S: Source<'a>
{
    /// Create a new TokenIterator from a given Source
    pub fn new(src: S) -> Self {
        TokenIterator {
            src: src,
            idx: Default::default(),
            phantom: ::std::marker::PhantomData,
        }
    }

    /// Skip Past Matching Chars
    ///
    /// Searches through the source for the first source position that
    /// doesn't match the given predicate function. Used to find the
    /// end of longer tokens such as whitespace or identifiers.
    fn skip_src_past<P>(&mut self, mut pos: S::Pos, pred: P) -> S::Pos
        where P: Fn(char) -> bool
    {
        while let Some((ch, next)) = self.src.take(pos) {
            if !pred(ch) {
                break;
            }
            pos = next;
        }
        pos
    }

    /// Optional Two-Char Token
    ///
    /// Peeks at the next character. If it matches the expected second
    /// part of a two-char token then `two` is returned. Otherwise
    /// `one` is returned instead.
    fn opt_two_char(&mut self, second: char, two: Token<'a>, one: Token<'a>) -> Token<'a> {
        match self.src.take(self.idx) {
            Some((ch, p)) if ch == second => {
                self.idx = p;
                two
            }
            _ => one,
        }
    }

    /// Retrieve the next 'raw' token. This is the next lexical match
    /// in the buffer, and may include whitespace and other trivia
    /// tokens.
    fn next_raw(&mut self) -> Option<Token<'a>> {

        let ts = self.idx;
        self.src.take(ts).map(|(c, pos)| {
            self.idx = pos;
            match c {
                '=' => self.opt_two_char('=', Token::DoubleEquals, Token::Equals),
                '!' => self.opt_two_char('=', Token::BangEquals, Token::Bang),
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
                    self.idx = self.skip_src_past(pos, |c| c != '\n');
                    Token::Whitespace(self.src.slice(ts, self.idx))
                }
                '0'...'9' => {
                    self.idx = self.skip_src_past(pos, |c| c >= '0' && c <= '9');
                    let token_str = self.src.slice(ts, self.idx);
                    // we have cheked that it's a valid numeric literal,
                    // so unwrap is fine here.||||
                    Token::Literal(token_str.parse::<i64>().unwrap())
                }
                c if c.is_alphabetic() || c == '_' => {
                    self.idx = self.skip_src_past(pos, |c| c.is_alphanumeric() || c == '_');
                    Token::Word(&self.src.slice(ts, self.idx))
                }
                c if c.is_whitespace() => {
                    self.idx = self.skip_src_past(pos, |c| c.is_whitespace());
                    Token::Whitespace(&self.src.slice(ts, self.idx))
                }
                _ => Token::Unknown(c),
            }
        })
    }
}

/// TokenIterator Iterator implementation.
///
/// This allows iterator adapters to be used with the token
/// stream. The next method filters out whitespace tokens from the
/// TokenIterator too. This means the `TokenIterator` doesn't have to worry
/// about skipping certain lexemes in the grammar.
impl<'a, S> Iterator for TokenIterator<'a, S>
    where S: Source<'a>
{
    type Item = Token<'a>;

    /// Iterator next method. This method returns the next
    /// non-whitespace token in the `TokenIterator`'s stream of `Token`s.
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(tok) = self.next_raw() {
            if let Token::Whitespace(_) = tok {
            } else {
                return Some(tok);
            }
        }
        None
    }
}
