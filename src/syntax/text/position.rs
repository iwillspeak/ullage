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

/// Dummy Span used when no real span is avaiable.
pub const DUMMY_SPAN: Span = Span {
    start: Pos(0),
    end: Pos(0),
};

impl Pos {
    /// Get the Byte Offset
    ///
    /// Returns the offset from the beginning of the `SourceText` for
    /// this position.
    pub fn offset(self) -> usize {
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
        Span { start, end }
    }

    /// Create a zero-width span at a given position
    ///
    /// The retunred span has the same start and end position.
    pub fn new_at(pos: Pos) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }

    /// Get the starting position of this span
    pub fn start(&self) -> Pos {
        self.start
    }

    /// Get the end position of this span
    pub fn end(&self) -> Pos {
        self.end
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
    fn span_has_start_end() {
        let span_loc = Span::new(12.into(), 43.into());

        assert_eq!(12, span_loc.start().offset());
        assert_eq!(43, span_loc.end().offset());
    }

    #[test]
    fn span_at_pos() {
        let pos = Pos::from(123);
        let span = Span::new_at(pos);

        assert_eq!(123, span.start().offset());
        assert_eq!(123, span.end().offset());
    }
}
