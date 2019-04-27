//! Source Text
//!
//! This module contains a a structure to represent the source text of
//! the program. It is used to hold the source code in memory for
//! iteration by the lexer. Indexing in to the source is possible
//! using `Location`s, and `Location`s can be turned into `(line,
//! col)` position pairs for displaying in diagnostics.

use super::{Ident, Interner, Pos};
use std::cell::RefCell;
use std::io::{self, prelude::*};
use std::fs::File;
use std::path::Path;

/// Source Text Struct
///
/// A source text is a pair of a string containing the contents of the
/// file or other input and a start position.
pub struct SourceText {
    /// The contents of the source text
    source: String,
    /// The offsets of the beginning of each line. Can be used to
    /// convert a character offset into the (line, column)
    line_offsets: Vec<usize>,
    /// String interner to create identifiers
    ///
    /// FIXME: Should this live here?
    interner: RefCell<Interner>,
}

impl SourceText {
    /// Create a `SourceText` for the given string
    pub fn new<T: Into<String>>(source: T) -> Self {
        let source: String = source.into();
        let line_offsets = get_line_offsets(&source[..]);
        SourceText {
            source,
            line_offsets,
            interner: Default::default(),
        }
    }

    /// Create a source text from standard input
    ///
    /// Reads the contents of `io:;stdin` to a buffer and creats a new
    /// source tex from that.
    pub fn from_stdin() -> io::Result<Self> {
        let mut s = String::new();
        io::stdin().read_to_string(&mut s)?;
        Ok(SourceText::new(s))
    }

    /// Create a source text from a file
    ///
    /// Reads the contents of a given file path into a buffer and
    /// creates a new source text from that.
    pub fn from_path<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let mut s = String::new();
        File::open(path)?.read_to_string(&mut s)?;
        Ok(SourceText::new(s))
    }

    /// Get the Starting Position
    ///
    /// Retunrns the index into the buffer which points to the first
    /// character. As this is a 'cursor' which points 'between' the
    /// characters even an empty source will have at least one
    /// distinct position.
    pub fn start(&self) -> Pos {
        Pos::from(0)
    }

    /// Get Line Count
    ///
    /// Returns the number of lines in the source text.
    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }

    /// Intern a String Value
    pub fn intern(&self, value: &str) -> Ident {
        self.interner.borrow_mut().intern(value)
    }

    /// Lookup the value of an identifier
    pub fn interned_value(&self, ident: Ident) -> String {
        self.interner.borrow().interned_value(ident).into()
    }

    /// Get Line Position
    ///
    /// Returns the `(line, col)` position of the given position in
    /// the source.
    pub fn line_pos<T: Into<Pos>>(&self, pos: T) -> (usize, usize) {
        let offset = pos.into().offset();
        match self.line_offsets.binary_search(&offset) {
            Ok(index) => (index + 1, 0),
            Err(index) => {
                let nearest_line_start = self.line_offsets[index - 1];
                (index, offset - nearest_line_start)
            }
        }
    }

    /// Slice into the Source
    pub fn slice(&self, start: Pos, end: Pos) -> &str {
        &self.source[start.offset()..end.offset()]
    }

    /// Walk the Source Characters
    pub fn walk_chars(&self, start: Pos) -> impl Iterator<Item = (char, Pos)> + '_ {
        self.source[start.offset()..]
            .chars()
            .scan(start, |pos, ch| {
                let next = Pos::from(pos.offset() + ch.len_utf8());
                *pos = next;
                Some((ch, next))
            })
    }
}

fn get_line_offsets(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(idx, _)| idx + 1))
        .collect()
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn source_empty_has_single_line() {
        let source = SourceText::new("");
        assert_eq!(1, source.line_count());
    }

    #[test]
    fn source_with_windows_newline() {
        let source = SourceText::new("\r\n");
        assert_eq!(2, source.line_count());
    }

    #[test]
    fn source_with_multiple_lines() {
        let source = SourceText::new(
            r#"
fn hello(world: String): String
    print 'hello ' + world
end
"#,
        );
        assert_eq!(5, source.line_count());
    }

    #[test]
    fn source_pos_to_line_col() {
        let source = SourceText::new(
            r#"
# Modululs remainder
#
# Returns the modulus remainder of n/d
fn mod(n: Number, d: Number): Number
   n if n < d else mod(n - d, d)
end
"#,
        );

        assert_eq!((1, 0), source.line_pos(0));
        assert_eq!((2, 1), source.line_pos(2));
        assert_eq!((4, 8), source.line_pos(32));
        assert_eq!((4, 38), source.line_pos(62));
        assert_eq!((5, 0), source.line_pos(63));
        assert_eq!((5, 30), source.line_pos(93));
        assert_eq!((8, 0), source.line_pos(137));
    }
}
