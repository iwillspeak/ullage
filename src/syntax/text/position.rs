//! Syntax Position Information
//!
//! This module provides the types needed to represent positions with
//! a buffer.

/// Source Buffer Position
///
/// Used to represent a position within a the source of a compilation
/// session.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Pos(usize);

/// Source Buffer Span
///
/// A span represents a range of positions within the source. Each
/// span is deliniated by the start and end `Pos`s. Spans can be
/// used to identify the extent of lexemes in the AST, and ranges of
/// interest when emitting error information.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Span {
    start: Pos,
    end: Pos,
}

/// Source Location
///
/// Represents an abstraction over a source location. This is either a
/// specific cursor within the input or a span.
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Location {
    /// A specific position within the source.
    Pos(Pos),

    /// A range of the given source
    Span(Span),
}

impl Pos {
    /// Get the Byte Offset
    ///
    /// Returns the offset from the beginning of the `SourceText` for
    /// this position.
    pub fn offset(&self) -> usize {
        self.0
    }
}

impl std::ops::Add for Pos {
    type Output = Self;

    fn add(self, other: Pos) -> Self {
        Pos(self.0 + other.0)
    }
}

impl From<usize> for Pos {
    fn from(offset: usize) -> Self {
        Pos(offset)
    }
}

impl Span {
    /// Create a Span Around two Cursors
    ///
    /// The returned span starts after the first cursor and finishes
    /// before the second one. The cursors themselves can be thought
    /// to point 'between' the characters in the buffer.
    pub fn new(start: Pos, end: Pos) -> Self {
        Span {
            start: start,
            end: end,
        }
    }
}

impl Location {
    /// Get the Start of the Location
    ///
    /// Returns the lowest point in this location.
    pub fn start(&self) -> Pos {
        match *self {
            Location::Pos(pos) => pos,
            Location::Span(span) => span.start,
        }
    }

    /// Get the End of the Location
    ///
    /// Returns the highest point in this locaiton.
    pub fn end(&self) -> Pos {
        match *self {
            Location::Pos(pos) => pos,
            Location::Span(span) => span.end,
        }
    }
}

impl From<Span> for Location {
    fn from(span: Span) -> Self {
        Location::Span(span)
    }
}

impl From<Pos> for Location {
    fn from(pos: Pos) -> Self {
        Location::Pos(pos)
    }
}

#[cfg(test)]
pub mod test {

    use super::*;

    #[test]
    fn pos_from_usize() {
        let pos = Pos::from(1);
        assert_eq!(Pos(1), pos);
    }

    #[test]
    fn pos_converts_back_to_offset() {
        let pos = Pos::from(123);
        assert_eq!(123, pos.offset());
    }

    #[test]
    fn span_from_cursor_pair() {
        let span = Span::new(1.into(), 3.into());
        assert_eq!(Pos::from(1), span.start);
        assert_eq!(Pos::from(3), span.end);
    }

    #[test]
    fn location_from_span_or_cursor() {
        let pos_loc = Location::from(Pos::from(123));
        let span_loc = Location::from(Span::new(456.into(), 789.into()));

        assert_eq!(Location::Pos(Pos(123)), pos_loc);
        assert_eq!(
            Location::Span(Span {
                start: 456.into(),
                end: 789.into(),
            }),
            span_loc
        );
    }

    #[test]
    fn location_start_end() {
        let pos_loc = Location::from(Pos::from(123));
        let span_loc = Location::from(Span::new(12.into(), 43.into()));

        assert_eq!(123, pos_loc.start().offset());
        assert_eq!(123, pos_loc.end().offset());
        assert_eq!(12, span_loc.start().offset());
        assert_eq!(43, span_loc.end().offset());
    }
}
