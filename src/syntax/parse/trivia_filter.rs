//! Trivia Filter
//!
//! This iterator adapater removes whitespace and other 'trivia' from
//! the token stream.

use super::token::Token;

pub trait TriviaFilterable<'a>
where
    Self: Iterator<Item = Token<'a>> + Sized,
{
    /// Filter the Trivia Tokens
    ///
    /// Wrap this iterator to filter the whitespace tokens
    fn without_trivia(self) -> TriviaFilter<Self> {
        TriviaFilter::new(self)
    }
}

/// Trivia Filter Iterator Adapter
///
/// This is use to filter out tokess where `Token::is_trivia` returns
/// true from the token stream. Ideally the parser would be capable of
/// handling these tokens and store them in the tree.
///
/// FIXME: We shouldn't be filtering triva from the tree
pub struct TriviaFilter<T> {
    inner: T,
}

impl<'a, T> TriviaFilter<T>
where
    T: Iterator<Item = Token<'a>>,
{
    /// Create a new trivia filter wrapping the innner iterator.
    pub fn new(inner: T) -> Self {
        TriviaFilter { inner }
    }
}

impl<'a, T> Iterator for TriviaFilter<T>
where
    T: Iterator<Item = Token<'a>>,
{
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token) = self.inner.next() {
            if !token.is_trivia() {
                return Some(token);
            }
        }
        None
    }
}

impl<'a, T> TriviaFilterable<'a> for T where T: Iterator<Item = Token<'a>> {}

#[cfg(test)]
mod test {

    use super::super::super::text::SourceText;
    use super::super::token::*;
    use super::super::tokeniser::Tokeniser;
    use super::*;

    #[test]
    fn trivia_filter_without_trivia() {
        let source = SourceText::new("1'2'3");
        let tokens = Tokeniser::new(&source).without_trivia().collect::<Vec<_>>();
        assert_eq!(
            vec![
                Token::Literal(Literal::Number(1)),
                Token::Literal(Literal::RawString("2".into())),
                Token::Literal(Literal::Number(3)),
            ],
            tokens
        );
    }

    #[test]
    fn trivia_filter_removes_whitespace() {
        let source = SourceText::new("1 \t'2'");
        let tokens = Tokeniser::new(&source).without_trivia().collect::<Vec<_>>();
        assert_eq!(
            vec![
                Token::Literal(Literal::Number(1)),
                Token::Literal(Literal::RawString("2".into())),
            ],
            tokens
        );
    }

    #[test]
    fn trivia_filter_with_only_trivia() {
        let source = SourceText::new(
            r#"
        # a commment

        # another comment"#,
        );
        let mut tokens = Tokeniser::new(&source).without_trivia();
        assert_eq!(None, tokens.next());
        // Check the iterator is fused
        assert_eq!(None, tokens.next());
    }
}
