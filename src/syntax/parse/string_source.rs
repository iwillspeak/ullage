use super::Source;

/// Tokenisation input from a string slice
pub struct StringSource<'a> {
    /// The buffer this `Source` wraps.
    buff: &'a str,
}

impl<'a> StringSource<'a> {
    /// Create a new StringSource wrapping a character buffer.
    pub fn new(s: &'a str) -> Self {
        StringSource { buff: s }
    }
}

impl<'a> Source<'a> for StringSource<'a> {
    type Pos = usize;

    fn take(&mut self, pos: Self::Pos) -> Option<(char, Self::Pos)> {
        self.buff[pos..].chars().nth(0).map(|ch| (ch, pos + ch.len_utf8()))
    }

    fn slice(&self, start: Self::Pos, end: Self::Pos) -> &'a str {
        &self.buff[start..end]
    }
}
