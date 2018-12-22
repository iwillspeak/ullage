//! Syntax Text
//!
//! This module contains types to represent the source text of the
//! program.

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
}

impl SourceText {
    /// Create a `SourceText`
    ///
    /// Creates a new source text for the given string
    pub fn new<T: Into<String>>(source: T) -> Self {
        let source: String = source.into();
        let line_offsets = get_line_offsets(&source[..]);
        SourceText {
            source,
            line_offsets,
        }
    }

    /// Get Line Count
    ///
    /// Returns the number of lines in the source text.
    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }

    /// Get Line Position
    ///
    /// Returns the `(line, col)` position of the given offset into
    /// the source.
    pub fn line_pos(&self, offset: usize) -> (usize, usize) {
        match self.line_offsets.binary_search(&offset) {
            Ok(index) => (index + 1, 0),
            Err(index) => {
                let nearest_line_start = self.line_offsets[index - 1];
                (index, offset - nearest_line_start)
            }
        }
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
