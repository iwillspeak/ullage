//! Source Text
//!
//! This module contains abstractions relating to the text of the
//! program. The main memer is the `SourceText` structure which
//! provides backing for the source code as well as line information.
//!
//! Positions within the source are represnted by the `Pos`, `Span`
//! and `Location` types.

mod intern;
mod position;
mod source_text;

pub use self::intern::{Ident, Interner};
pub use self::position::{Pos, Span, DUMMY_SPAN};
pub use self::source_text::SourceText;
