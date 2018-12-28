//! Raw Tokeniser
//!
//! This module defines the 'raw' tokeniser. The raw tokeniser emits a
//! flat list of all the tokens in a given source text. It provides
//! token kind information along with the span at which the token is
//! found.
//!
//! Raw tokens should be consumed by the main tokeniser to be turned
//! into structured syntax tokens by grouping trivia tokens to their
//! appropriate syntax token.

use super::super::text::{Pos, SourceText, Span};
use super::super::tree::{Literal, Token, TokenKind, TriviaTokenKind};

/// The Raw Token Structure
#[derive(Debug, PartialEq)]
pub struct RawToken {
    // The span of this token in the source text
    pub span: Span,
    /// The underlying token kind. This is either palin syntax token
    /// or a trivia token.
    pub kind: RawTokenKind,
}

/// Raw Token Kind
///
/// Used to abstract over palin and trivia tokens.
#[derive(Debug, PartialEq)]
pub enum RawTokenKind {
    /// A plain syntax token
    Plain(TokenKind),
    /// A trivia token
    Trivia(TriviaTokenKind),
}

/// The Raw Tokeniser
///
/// This walks a state machine over the underlying `SourceText` and
/// returns a sequence of tokens.
pub struct RawTokeniser<'t> {
    /// The undering source buffer
    source: &'t SourceText,
    /// The current position in the source text.
    pos: Pos,
}

impl From<TokenKind> for RawTokenKind {
    fn from(input: TokenKind) -> Self {
        RawTokenKind::Plain(input)
    }
}

impl From<TriviaTokenKind> for RawTokenKind {
    fn from(input: TriviaTokenKind) -> Self {
        RawTokenKind::Trivia(input)
    }
}

impl<'t> RawTokeniser<'t> {
    /// Create a Tokeniser
    pub fn new(source: &'t SourceText) -> Self {
        let pos = source.start();
        RawTokeniser { source, pos }
    }

    /// Group the Trivia Tokens
    ///
    /// This method consumes the inner iterator and returns a new
    /// iterable which transforms the stream of raw tokens by
    /// attaching the trivia to the 'closest' plain token.
    pub fn group_trivia(self) -> TokenGrouper<'t> {
        TokenGrouper::new(self)
    }

    /// Skip Over Characters
    ///
    /// Evaluates the predicate to find the end of a given token
    /// stream. Used to find the end of comments, whitespace and other
    /// variable-length tokens that we don't need to store values for.
    fn skip_over<P>(&mut self, chars: &mut Iterator<Item = (char, Pos)>, mut pred: P)
    where
        P: FnMut(char) -> bool,
    {
        let more = chars.take_while(|(c, _)| pred(*c)).last();
        if let Some((_, end)) = more {
            self.pos = end;
        }
    }

    /// One-Char Lookahead Token Choice
    ///
    /// Look at the next character in the stream. Based on the value
    /// of the character return either `single` or `double`.
    ///
    /// If the next character in `chars` is `maybe_next` then the
    /// character is consumed from the iterator and `double` is
    /// returned. Otherwise the position state is left as-is and
    /// `single` is returned.
    ///
    /// This is designed for recognising two-char tokens like `==` and
    /// `!=` where the first character could be a valid token when
    /// taken on its own.
    fn ch_choice<T, U>(
        &mut self,
        chars: &mut Iterator<Item = (char, Pos)>,
        maybe_next: char,
        single: T,
        double: U,
    ) -> RawTokenKind
    where
        T: Into<RawTokenKind>,
        U: Into<RawTokenKind>,
    {
        match chars.next() {
            Some((c, e)) if c == maybe_next => {
                self.pos = e;
                double.into()
            }
            _ => single.into(),
        }
    }
}

impl<'t> Iterator for RawTokeniser<'t> {
    type Item = RawToken;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.pos;
        let mut chars = self.source.walk_chars(start);
        let kind = chars.next().map(|(c, next_pos)| {
            self.pos = next_pos;
            match c {
                '=' => self.ch_choice(&mut chars, '=', TokenKind::Equals, TokenKind::DoubleEquals),
                '!' => self.ch_choice(&mut chars, '=', TokenKind::Bang, TokenKind::BangEquals),
                '+' => TokenKind::Plus.into(),
                '-' => TokenKind::Minus.into(),
                '*' => TokenKind::Star.into(),
                '/' => TokenKind::Slash.into(),
                '(' => TokenKind::OpenBracket.into(),
                ')' => TokenKind::CloseBracket.into(),
                '[' => TokenKind::OpenSqBracket.into(),
                ']' => TokenKind::CloseSqBracket.into(),
                ',' => TokenKind::Comma.into(),
                ':' => TokenKind::Colon.into(),
                '<' => TokenKind::LessThan.into(),
                '>' => TokenKind::MoreThan.into(),
                '#' => {
                    self.skip_over(&mut chars, |c| c != '\n');
                    TriviaTokenKind::Comment.into()
                }
                '\n' => TriviaTokenKind::Newline.into(),
                '\r' => self.ch_choice(
                    &mut chars,
                    '\n',
                    TriviaTokenKind::Newline,
                    TriviaTokenKind::Newline,
                ),
                '0'..='9' => {
                    self.skip_over(&mut chars, |c| c >= '0' && c <= '9');
                    let lex_val = self.source.slice(start, self.pos);
                    TokenKind::Literal(Literal::Number(lex_val.parse::<i64>().unwrap())).into()
                }
                '\'' => {
                    // So this is a bit hairy. Essentially we keep
                    // track of when we have seen the closing
                    // delimiter so we can consume that token. If we
                    // see an unterminated string literal then emit it
                    // as junk instead.
                    let mut seen_end = false;
                    self.skip_over(&mut chars, |c| {
                        if seen_end {
                            false
                        } else {
                            seen_end = c == '\'';
                            true
                        }
                    });
                    let lex_val = self.source.slice(next_pos, self.pos);
                    if seen_end {
                        TokenKind::Literal(Literal::RawString(lex_val[..lex_val.len() - 1].into()))
                            .into()
                    } else {
                        // TODO: To check the handlng of unterminated
                        // strings we could do with a failing
                        // compilation test. We don't want these junk
                        // tokens getting ignored.
                        TriviaTokenKind::Junk.into()
                    }
                }
                c if c.is_alphabetic() || c == '_' => {
                    self.skip_over(&mut chars, |c| c.is_alphanumeric() || c == '_');
                    let lex_val = self.source.slice(start, self.pos);
                    TokenKind::Word(lex_val.into()).into()
                }
                c if c.is_whitespace() => {
                    self.skip_over(&mut chars, |c| c.is_whitespace());
                    TriviaTokenKind::Whitespace.into()
                }
                _ => TriviaTokenKind::Junk.into(),
            }
        });
        kind.map(|k| RawToken {
            kind: k,
            span: Span::new(start, self.pos),
        })
    }
}

/// The Token Group Iterator
///
/// This iterator adapter groups trivia to transform a stream of raw
/// tokens into a stream of plain tokens with the trivia attached to
/// the 'closest' plain token.
///
/// For our case trivia at the start of a line is attached to the
/// first plain token. Trivia after a token up to the end of line or
/// next plain token is attached as trailing trivia.
pub struct TokenGrouper<'t> {
    inner: RawTokeniser<'t>,
}

impl<'t> TokenGrouper<'t> {
    /// Construct a new Token Grouper by wrapping the `inner`
    /// `RawTokeniser`.
    pub fn new(inner: RawTokeniser<'t>) -> Self {
        TokenGrouper { inner }
    }
}

impl<'t> Iterator for TokenGrouper<'t> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(raw) = self.inner.next() {
            match raw.kind {
                RawTokenKind::Plain(plain_kind) => {
                    return Some(Token::with_span(raw.span, plain_kind))
                }
                _ => (),
            }
        }
        None
    }
}

#[cfg(test)]
mod test {

    use super::*;

    macro_rules! check_lex {
        ($src:expr, $expected:expr) => {
            let src: &str = $src;
            let source = SourceText::new(src);
            let tokeniser = RawTokeniser::new(&source);
            let tokens = tokeniser.collect::<Vec<_>>();
            let start = source.start();
            let end = start + Pos::from(src.len());
            let span = Span::new(start, end);
            assert_eq!(
                vec![RawToken {
                    span,
                    kind: $expected
                }],
                tokens
            );
        };
    }

    #[test]
    pub fn check_single_token_lext() {
        // TODO: Do we wnat to introduce a `SyntaxFacts` which tells
        // us these known literals? Would be useful in testing at least.
        check_lex!("=", RawTokenKind::Plain(TokenKind::Equals));
        check_lex!("==", RawTokenKind::Plain(TokenKind::DoubleEquals));
        check_lex!("!", RawTokenKind::Plain(TokenKind::Bang));
        check_lex!("!=", RawTokenKind::Plain(TokenKind::BangEquals));
        check_lex!("+", RawTokenKind::Plain(TokenKind::Plus));
        check_lex!("-", RawTokenKind::Plain(TokenKind::Minus));
        check_lex!("*", RawTokenKind::Plain(TokenKind::Star));
        check_lex!("/", RawTokenKind::Plain(TokenKind::Slash));
        check_lex!("(", RawTokenKind::Plain(TokenKind::OpenBracket));
        check_lex!(")", RawTokenKind::Plain(TokenKind::CloseBracket));
        check_lex!("[", RawTokenKind::Plain(TokenKind::OpenSqBracket));
        check_lex!("]", RawTokenKind::Plain(TokenKind::CloseSqBracket));
        check_lex!(",", RawTokenKind::Plain(TokenKind::Comma));
        check_lex!(":", RawTokenKind::Plain(TokenKind::Colon));
        check_lex!("<", RawTokenKind::Plain(TokenKind::LessThan));
        check_lex!(">", RawTokenKind::Plain(TokenKind::MoreThan));

        // Whitespace
        check_lex!(" ", RawTokenKind::Trivia(TriviaTokenKind::Whitespace));
        check_lex!("\t", RawTokenKind::Trivia(TriviaTokenKind::Whitespace));
        check_lex!(
            "\u{2003}",
            RawTokenKind::Trivia(TriviaTokenKind::Whitespace)
        );
        check_lex!(" ", RawTokenKind::Trivia(TriviaTokenKind::Whitespace));
        check_lex!("  \t", RawTokenKind::Trivia(TriviaTokenKind::Whitespace));

        // Junk tokens, can't have multi-char ones yet
        check_lex!("¬", RawTokenKind::Trivia(TriviaTokenKind::Junk));
        check_lex!("%", RawTokenKind::Trivia(TriviaTokenKind::Junk));
        check_lex!("£", RawTokenKind::Trivia(TriviaTokenKind::Junk));

        // Comments just run to the end of the line, regardless of
        // what is in them.
        check_lex!("#", RawTokenKind::Trivia(TriviaTokenKind::Comment));
        check_lex!(
            "# longer comment",
            RawTokenKind::Trivia(TriviaTokenKind::Comment)
        );
        check_lex!(
            "# ∆¬∞€#",
            RawTokenKind::Trivia(TriviaTokenKind::Comment)
        );

        // We recognise _all_ denominations of newline
        check_lex!("\n", RawTokenKind::Trivia(TriviaTokenKind::Newline));
        check_lex!("\r", RawTokenKind::Trivia(TriviaTokenKind::Newline));
        check_lex!("\r\n", RawTokenKind::Trivia(TriviaTokenKind::Newline));
    }

    #[test]
    pub fn check_lex_numeric_values() {
        check_lex!(
            "123",
            RawTokenKind::Plain(TokenKind::Literal(Literal::Number(123)))
        );
        check_lex!(
            "0",
            RawTokenKind::Plain(TokenKind::Literal(Literal::Number(0)))
        );
        check_lex!(
            "9999",
            RawTokenKind::Plain(TokenKind::Literal(Literal::Number(9999)))
        );
    }

    #[test]
    pub fn check_lex_identifier_values() {
        check_lex!("a", RawTokenKind::Plain(TokenKind::Word("a".into())));
        check_lex!(
            "hello",
            RawTokenKind::Plain(TokenKind::Word("hello".into()))
        );
        check_lex!("a1", RawTokenKind::Plain(TokenKind::Word("a1".into())));
        check_lex!(
            "foo_bar",
            RawTokenKind::Plain(TokenKind::Word("foo_bar".into()))
        );
        check_lex!("_", RawTokenKind::Plain(TokenKind::Word("_".into())));
        check_lex!(
            "_private",
            RawTokenKind::Plain(TokenKind::Word("_private".into()))
        );
        check_lex!(
            "while",
            RawTokenKind::Plain(TokenKind::Word("while".into()))
        );
        check_lex!(
            "ünîçøδé",
            RawTokenKind::Plain(TokenKind::Word("ünîçøδé".into()))
        );
    }

    #[test]
    pub fn check_lex_string_values() {
        // raw string values ('foo' etc.) are all that is supported
        // right now.
        check_lex!(
            "''",
            RawTokenKind::Plain(TokenKind::Literal(Literal::RawString("".into())))
        );
        check_lex!(
            "'hello'",
            RawTokenKind::Plain(TokenKind::Literal(Literal::RawString("hello".into())))
        );
        check_lex!(
            "'føœ Bå®'",
            RawTokenKind::Plain(TokenKind::Literal(Literal::RawString("føœ Bå®".into())))
        );
    }

    #[test]
    fn newlines_are_not_attached_to_comments() {
        let src = SourceText::new("# comment\n#another comment\nliteral");
        let tokeniser = RawTokeniser::new(&src);
        let tokens = tokeniser.map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            vec![
                RawTokenKind::Trivia(TriviaTokenKind::Comment),
                RawTokenKind::Trivia(TriviaTokenKind::Newline),
                RawTokenKind::Trivia(TriviaTokenKind::Comment),
                RawTokenKind::Trivia(TriviaTokenKind::Newline),
                RawTokenKind::Plain(TokenKind::Word("literal".into())),
            ],
            tokens
        );
    }

    #[test]
    fn raw_tokenier_collect_returns_expected_tokens() {
        let src = SourceText::new("var foo = 'hello world' + 1");
        let tokeniser = RawTokeniser::new(&src);
        let tokens = tokeniser.map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            vec![
                RawTokenKind::Plain(TokenKind::Word("var".into())),
                RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                RawTokenKind::Plain(TokenKind::Word("foo".into())),
                RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                RawTokenKind::Plain(TokenKind::Equals),
                RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                RawTokenKind::Plain(TokenKind::Literal(Literal::RawString("hello world".into()))),
                RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                RawTokenKind::Plain(TokenKind::Plus),
                RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                RawTokenKind::Plain(TokenKind::Literal(Literal::Number(1))),
            ],
            tokens
        );
    }

    #[test]
    fn raw_tokeniser_group_trivia_returns_expected_grouping() {
        let src = SourceText::new("(hello + world)");
        let tokeniser = RawTokeniser::new(&src);
        let tokens = tokeniser.group_trivia().collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::with_span(
                    Span::new(Pos::from(0), Pos::from(1)),
                    TokenKind::OpenBracket
                ),
                Token::with_span(
                    Span::new(Pos::from(1), Pos::from(6)),
                    TokenKind::Word("hello".into())
                ),
                Token::with_span(Span::new(Pos::from(7), Pos::from(8)), TokenKind::Plus),
                Token::with_span(
                    Span::new(Pos::from(9), Pos::from(14)),
                    TokenKind::Word("world".into())
                ),
                Token::with_span(
                    Span::new(Pos::from(14), Pos::from(15)),
                    TokenKind::CloseBracket
                ),
            ],
            tokens
        );
    }
}
