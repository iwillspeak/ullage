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

use super::super::text::{Location, Pos, SourceText, Span};
use super::super::tree::{Literal, Token, TokenKind, TriviaToken, TriviaTokenKind};
use std::iter::Peekable;

/// Token Stream Trait
///
/// Token streams are iterators of tokesn that never end.
pub trait TokenStream {
    /// Next Token
    ///
    /// Advances the lexer to get the next underlying token.
    fn next_token(&mut self) -> Token;
}

/// The Raw Token Structure
///
/// Raw tokens are the underlying values proudced by lexical analysis
/// of the source text. Raw tokens could be high-level tokens tokens
/// like identifiers or brackets, or trivia tokens like whitepsace or
/// comments.
#[derive(Debug, PartialEq)]
struct RawToken {
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
enum RawTokenKind {
    /// A plain syntax token
    Plain(TokenKind),
    /// A trivia token
    Trivia(TriviaTokenKind),
}

/// The Raw Tokeniser
///
/// This walks a state machine over the underlying `SourceText` and
/// returns a sequence of tokens.
struct RawTokeniser<'t> {
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
                '<' => self.ch_choice(
                    &mut chars,
                    '=',
                    TokenKind::LessThan,
                    TokenKind::LessThanEqual,
                ),
                '>' => self.ch_choice(
                    &mut chars,
                    '=',
                    TokenKind::MoreThan,
                    TokenKind::MoreThanEqual,
                ),
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
                        TriviaTokenKind::Junk.into()
                    }
                }
                c if c.is_alphabetic() || c == '_' => {
                    self.skip_over(&mut chars, |c| c.is_alphanumeric() || c == '_');
                    let lex_val = self.source.slice(start, self.pos);
                    let ident = self.source.intern(lex_val);
                    TokenKind::Word(ident).into()
                }
                c if c.is_whitespace() => {
                    self.skip_over(&mut chars, |c| c != '\r' && c != '\n' && c.is_whitespace());
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

/// The Token Iterator
///
/// Lexer which groups trivia to transform a stream of raw tokens into
/// a stream of plain tokens with the trivia attached to the 'closest'
/// plain token.
///
/// For our case trivia at the start of a line is attached to the
/// first plain token. Trivia after a token up to the end of line or
/// next plain token is attached as trailing trivia.
pub struct Tokeniser<'t> {
    inner: Peekable<RawTokeniser<'t>>,
    diagnostics: Vec<String>,
}

impl<'t> Tokeniser<'t> {
    /// Construct a new Tokeniser for the given source text
    pub fn new(source: &'t SourceText) -> Self {
        Tokeniser {
            inner: RawTokeniser::new(source).peekable(),
            diagnostics: Vec::new(),
        }
    }

    /// Get mutable access to the `Tokeniser`'s diagnostics
    ///
    /// This converts a mutable reference to the tokeniser to a
    /// reference to the internal diagnostics collection. Used when
    /// parsing to join together parser and tokeniser diagnostics.
    ///
    /// TODO: We should pass some form of diagnostics 'sink' to the
    /// tokeniser rather than pulling diagnostics from it.
    pub fn diagnostics_mut(&mut self) -> &mut Vec<String> {
        &mut self.diagnostics
    }

    /// Append a new trivia token to the trivia list
    ///
    /// Creates a new trivai token for the given kind and span and
    /// adds it to the list. If a diagnostic needs to be emittied
    /// about the new token then one is created.
    fn buffer_trivia(
        kind: TriviaTokenKind,
        span: Span,
        trivia: &mut Vec<TriviaToken>,
        diagnostics: &mut Vec<String>,
    ) {
        if kind == TriviaTokenKind::Junk {
            diagnostics.push(format!("unrecognised character at {:?}", span));
        }
        trivia.push(TriviaToken::with_span(span, kind));
    }

    /// Collect leading trivia
    ///
    /// Buffer all of theleading trivia and create a new token with
    /// that trivia. This is called from `Iterator::next` to create
    /// tokens.
    fn collect_leading(&mut self) -> Option<Token> {
        let mut leading = Vec::new();
        while let Some(token) = self.inner.next() {
            match token.kind {
                RawTokenKind::Trivia(trivia_kind) => {
                    Self::buffer_trivia(
                        trivia_kind,
                        token.span,
                        &mut leading,
                        &mut self.diagnostics,
                    );
                }
                RawTokenKind::Plain(plain_kind) => {
                    return Some(
                        Token::with_span(token.span, plain_kind).with_leading_trivia(leading),
                    );
                }
            }
        }
        if leading.is_empty() {
            None
        } else {
            let last_pos = leading.last().unwrap().end();
            Some(
                Token::with_span(Span::new_at(last_pos), TokenKind::End)
                    .with_leading_trivia(leading),
            )
        }
    }

    /// Collect trailing trivia
    ///
    /// This method returns a list of trivia from the current position
    /// to the end of the line or next plain token, whichever comes
    /// first. It is called from `Iterator::next` to attach trailing
    /// trivia to each token.
    fn collect_trailing(&mut self) -> Vec<TriviaToken> {
        let mut trailing = Vec::new();
        while let Some(next) = self.inner.peek() {
            match next.kind {
                RawTokenKind::Plain(_) => break,
                RawTokenKind::Trivia(trivia_kind) => {
                    if trivia_kind == TriviaTokenKind::Newline {
                        break;
                    }
                    Self::buffer_trivia(
                        trivia_kind,
                        next.span,
                        &mut trailing,
                        &mut self.diagnostics,
                    );
                    self.inner.next();
                }
            }
        }
        return trailing;
    }
}

impl<'t> Iterator for Tokeniser<'t> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.collect_leading()
            .map(|t| t.with_trailing_trivia(self.collect_trailing()))
    }
}

impl<T> TokenStream for T
where
    T: Iterator<Item = Token>,
{
    fn next_token(&mut self) -> Token {
        self.next().unwrap_or_else(|| Token::new(TokenKind::End))
    }
}

#[cfg(test)]
mod test {

    use super::*;

    macro_rules! check_lex {
        ($src:expr, |$source:ident| $expected:expr) => {
            let src: &str = $src;
            let $source = SourceText::new(src);
            let tokeniser = RawTokeniser::new(&$source);
            let tokens = tokeniser.collect::<Vec<_>>();
            let start = $source.start();
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
        ($src:expr, $expected:expr) => {
            check_lex!($src, |t| $expected);
        };
    }

    #[test]
    pub fn check_single_token_lex() {
        // Punctuation tokens
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
        check_lex!("<=", RawTokenKind::Plain(TokenKind::LessThanEqual));
        check_lex!(">", RawTokenKind::Plain(TokenKind::MoreThan));
        check_lex!(">=", RawTokenKind::Plain(TokenKind::MoreThanEqual));

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
        check_lex!("a", |s| RawTokenKind::Plain(TokenKind::Word(s.intern("a"))));
        check_lex!("hello", |s| RawTokenKind::Plain(TokenKind::Word(
            s.intern("hello")
        )));
        check_lex!("a1", |s| RawTokenKind::Plain(TokenKind::Word(
            s.intern("a1")
        )));
        check_lex!("foo_bar", |s| RawTokenKind::Plain(TokenKind::Word(
            s.intern("foo_bar")
        )));
        check_lex!("_", |s| RawTokenKind::Plain(TokenKind::Word(s.intern("_"))));
        check_lex!("_private", |s| RawTokenKind::Plain(TokenKind::Word(
            s.intern("_private")
        )));
        check_lex!("while", |s| RawTokenKind::Plain(TokenKind::Word(
            s.intern("while")
        )));
        check_lex!("ünîçøδé", |s| RawTokenKind::Plain(TokenKind::Word(
            s.intern("ünîçøδé")
        )));
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
    fn newlines_break_whitespace() {
        for line in [" \n ", " \r ", " \r\n "].iter() {
            let src = SourceText::new(*line);
            let tokeniser = RawTokeniser::new(&src);
            let tokens = tokeniser.map(|t| t.kind).collect::<Vec<_>>();

            assert_eq!(
                vec![
                    RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                    RawTokenKind::Trivia(TriviaTokenKind::Newline),
                    RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                ],
                tokens
            );
        }
    }

    #[test]
    fn newlines_are_not_attached_to_comments() {
        let src = SourceText::new("# comment\n#another comment\n1");
        let tokeniser = RawTokeniser::new(&src);
        let tokens = tokeniser.map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            vec![
                RawTokenKind::Trivia(TriviaTokenKind::Comment),
                RawTokenKind::Trivia(TriviaTokenKind::Newline),
                RawTokenKind::Trivia(TriviaTokenKind::Comment),
                RawTokenKind::Trivia(TriviaTokenKind::Newline),
                RawTokenKind::Plain(TokenKind::Literal(Literal::Number(1))),
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
                RawTokenKind::Plain(TokenKind::Word(src.intern("var"))),
                RawTokenKind::Trivia(TriviaTokenKind::Whitespace),
                RawTokenKind::Plain(TokenKind::Word(src.intern("foo"))),
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
    fn main_tokeniser_returns_expected_grouping() {
        let src = SourceText::new("(hello + world)");
        let tokeniser = Tokeniser::new(&src);
        let tokens = tokeniser.collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::with_span(
                    Span::new(Pos::from(0), Pos::from(1)),
                    TokenKind::OpenBracket
                ),
                Token::with_span(
                    Span::new(Pos::from(1), Pos::from(6)),
                    TokenKind::Word(src.intern("hello"))
                ),
                Token::with_span(Span::new(Pos::from(7), Pos::from(8)), TokenKind::Plus),
                Token::with_span(
                    Span::new(Pos::from(9), Pos::from(14)),
                    TokenKind::Word(src.intern("world"))
                ),
                Token::with_span(
                    Span::new(Pos::from(14), Pos::from(15)),
                    TokenKind::CloseBracket
                ),
            ],
            tokens
        );
    }

    #[test]
    fn token_stream_returns_eof() {
        let tokens = Vec::new();
        let mut token_stream = tokens.into_iter();

        assert_eq!(TokenKind::End, token_stream.next_token().kind);
        assert_eq!(TokenKind::End, token_stream.next_token().kind);
        assert_eq!(TokenKind::End, token_stream.next_token().kind);
    }

    #[test]
    fn token_stream_returns_eof_after_tokens() {
        let tokens = vec![
            Token::new(TokenKind::OpenSqBracket),
            Token::new(TokenKind::Comma),
        ];
        let mut token_stream = tokens.into_iter();

        assert_eq!(TokenKind::OpenSqBracket, token_stream.next_token().kind);
        assert_eq!(TokenKind::Comma, token_stream.next_token().kind);
        assert_eq!(TokenKind::End, token_stream.next_token().kind);
        assert_eq!(TokenKind::End, token_stream.next_token().kind);
        assert_eq!(TokenKind::End, token_stream.next_token().kind);
    }

    #[test]
    fn test_trivia_grouping() {
        let src = SourceText::new("fn foo() \n  var x = 2\nend");
        let tokeniser = Tokeniser::new(&src);
        let tokens = tokeniser.collect::<Vec<_>>();

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(0), Pos::from(2)),
                TokenKind::Word(src.intern("fn"))
            ),
            tokens[0]
        );
        assert!(tokens[0].leading().is_empty());
        assert_eq!(
            &[TriviaToken::with_span(
                Span::new(Pos::from(2), Pos::from(3)),
                TriviaTokenKind::Whitespace
            )],
            tokens[0].trailing()
        );

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(3), Pos::from(6)),
                TokenKind::Word(src.intern("foo"))
            ),
            tokens[1]
        );
        assert!(tokens[1].leading().is_empty());
        assert!(tokens[1].trailing().is_empty());

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(6), Pos::from(7)),
                TokenKind::OpenBracket
            ),
            tokens[2]
        );
        assert!(tokens[2].leading().is_empty());
        assert!(tokens[2].trailing().is_empty());

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(7), Pos::from(8)),
                TokenKind::CloseBracket
            ),
            tokens[3]
        );
        assert!(tokens[3].leading().is_empty());
        assert_eq!(
            &[TriviaToken::with_span(
                Span::new(Pos::from(8), Pos::from(9)),
                TriviaTokenKind::Whitespace
            )],
            tokens[3].trailing()
        );

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(12), Pos::from(15)),
                TokenKind::Word(src.intern("var"))
            ),
            tokens[4]
        );
        assert_eq!(
            &[
                TriviaToken::with_span(
                    Span::new(Pos::from(9), Pos::from(10)),
                    TriviaTokenKind::Newline
                ),
                TriviaToken::with_span(
                    Span::new(Pos::from(10), Pos::from(12)),
                    TriviaTokenKind::Whitespace
                )
            ],
            tokens[4].leading()
        );
        assert_eq!(
            &[TriviaToken::with_span(
                Span::new(Pos::from(15), Pos::from(16)),
                TriviaTokenKind::Whitespace
            )],
            tokens[4].trailing()
        );

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(16), Pos::from(17)),
                TokenKind::Word(src.intern("x"))
            ),
            tokens[5]
        );
        assert!(tokens[5].leading().is_empty());
        assert_eq!(
            &[TriviaToken::with_span(
                Span::new(Pos::from(17), Pos::from(18)),
                TriviaTokenKind::Whitespace
            )],
            tokens[5].trailing()
        );

        assert_eq!(
            Token::with_span(Span::new(Pos::from(18), Pos::from(19)), TokenKind::Equals),
            tokens[6]
        );
        assert!(tokens[6].leading().is_empty());
        assert_eq!(
            &[TriviaToken::with_span(
                Span::new(Pos::from(19), Pos::from(20)),
                TriviaTokenKind::Whitespace
            )],
            tokens[6].trailing()
        );

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(20), Pos::from(21)),
                TokenKind::Literal(Literal::Number(2))
            ),
            tokens[7]
        );
        assert!(tokens[7].leading().is_empty());
        assert!(tokens[7].trailing().is_empty());

        assert_eq!(
            Token::with_span(
                Span::new(Pos::from(22), Pos::from(25)),
                TokenKind::Word(src.intern("end"))
            ),
            tokens[8]
        );
        assert_eq!(
            &[TriviaToken::with_span(
                Span::new(Pos::from(21), Pos::from(22)),
                TriviaTokenKind::Newline
            )],
            tokens[8].leading()
        );
        assert!(tokens[8].trailing().is_empty());
    }
}
