//! Syntax Position Information
//!
//! This module provides the types needed to represent positions with
//! a buffer. Position information is all derived from `Cursor`s, byte
//! offsets within a canonical set of bytes which make up the current
//! compilation session.

/// Source Buffer Cursor
///
/// Used to represent a position within a the source of a compilation
/// session. For a given compilation session a `Cursor` should
/// uniquely identify a position in all sources.
#[derive(Debug,PartialEq)]
pub struct Cursor(usize);

/// Source Buffer Span
///
/// A span represents a range of positions within the source. Each
/// span is deliniated by the start and end `Cursor`s. Spans can be
/// used to identify the extent of lexemes in the AST, and ranges of
/// interest when emitting error information.
#[derive(Debug,PartialEq)]
pub struct Span {
    start: Cursor,
    end: Cursor,
}

/// Source Location
///
/// Represents an abstraction over a source location. This is either a
/// specific cursor within the input or a span.
#[derive(Debug,PartialEq)]
pub enum Location {
    /// A specific cursor location within the source.
    Cursor(Cursor),

    /// A range of the given source
    Span(Span),
}


impl From<usize> for Cursor {
    fn from(offset: usize) -> Self {
        Cursor(offset)
    }
}

impl Span {
    /// Create a Span Around two Cursors
    ///
    /// The returned span starts after the first cursor and finishes
    /// before the second one. The cursors themselves can be thought
    /// to point 'between' the characters in the buffer.
    pub fn new(start: Cursor, end: Cursor) -> Self {
        Span {
            start: start,
            end: end,
        }
    }
}

impl From<Span> for Location {
    fn from(span: Span) -> Self {
        Location::Span(span)
    }
}

impl From<Cursor> for Location {
    fn from(cur: Cursor) -> Self {
        Location::Cursor(cur)
    }
}

#[cfg(test)]
pub mod test {

    use super::*;

    #[test]
    fn cursor_from_usize() {
        let cursor = Cursor::from(1);
        assert_eq!(Cursor(1), cursor);
    }

    #[test]
    fn span_from_cursor_pair() {
        let span = Span::new(1.into(), 3.into());
        assert_eq!(Cursor::from(1), span.start);
        assert_eq!(Cursor::from(3), span.end);
    }

    #[test]
    fn location_from_span_or_cursor() {
        let cur_loc = Location::from(Cursor::from(123));
        let span_loc = Location::from(Span::new(456.into(), 789.into()));

        assert_eq!(Location::Cursor(Cursor(123)), cur_loc);
        assert_eq!(Location::Span(Span {
                       start: 456.into(),
                       end: 789.into(),
                   }),
                   span_loc);
    }
}
