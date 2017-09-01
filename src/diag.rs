// //! Compiler Diagnostics Handling
// //!
// //! This module contians the `DiagSession` type. This type holds a
// //! collection of compiler warnings and errors.

// use syntax::position::Location;

// /// A Diagnostics Session
// ///
// /// Represents a collection of diagnostics which stages in the
// /// compilation can write into, and which can be queried for it's
// /// state.
// pub struct DiagSession {
//     has_errors: bool,
//     diags: Vec<Diag>,
// }

// /// A Diagnostic Element
// ///
// /// Represents a single compiler diagnostic.
// pub struct Diag {
//     level: DiagSeverity,
//     loc: Option<Location>,
//     message: String,
// }

// /// Diagnostic Severity
// ///
// /// Represents the level of a given diagnostic. Used to differentiate
// /// between compilation errors which mean that the compilation process
// /// can no logner continue; warnings, and notes.
// #[derive(PartialEq)]
// pub enum DiagSeverity {
//     Error,
//     Warn,
//     Note,
// }

// impl DiagSession {
//     /// Create a Diagnostics Session
//     pub fn new() -> Self {
//         DiagSession {
//             has_errors: false,
//             diags: Vec::new(),
//         }
//     }

//     /// Check if the Session has any Erorrs
//     ///
//     /// Returns a boolean indicating if any error level diagnostics
//     /// have been added to the session.
//     pub fn has_errors(&self) -> bool {
//         self.has_errors
//     }


//     /// Add a Diagnostic to the Session
//     ///
//     /// Updates the list of diagnostics in the session to add the
//     /// given diagnostic. Sets the state of `has_errors` if the
//     /// daignostic is an error.
//     pub fn add(&mut self, diag: Diag) {
//         if diag.is_err() {
//             self.has_errors = true;
//         }
//         self.diags.push(diag);
//     }
// }

// impl Diag {
//     /// Create a Diagnostic
//     pub fn new<S: Into<String>>(loc: Option<Location>, level: DiagSeverity, message: S) -> Self {
//         Diag {
//             level: level,
//             loc: loc,
//             message: message.into(),
//         }
//     }

//     /// Create an Error Diagnostic
//     pub fn err<S: Into<String>>(message: S) -> Self {
//         Diag::new(None, DiagSeverity::Error, message)
//     }

//     /// Create an Error Diagnostic with Locaiton Info
//     pub fn err_pos<S: Into<String>>(message: S, loc: Location) -> Self {
//         Diag::new(Some(loc), DiagSeverity::Error, message)
//     }

//     /// Create a Warning Diagnostic
//     pub fn warn<S: Into<String>>(message: S) -> Self {
//         Diag::new(None, DiagSeverity::Warn, message)
//     }

//     /// Create a Warning Diagnostic with Locaiton Info
//     pub fn warn_pos<S: Into<String>>(message: S, loc: Location) -> Self {
//         Diag::new(Some(loc), DiagSeverity::Warn, message)
//     }

//     /// Create a Note Diagnostic
//     pub fn note<S: Into<String>>(message: S) -> Self {
//         Diag::new(None, DiagSeverity::Note, message)
//     }

//     /// Create a Note Daignostic with Location Info
//     pub fn note_pos<S: Into<String>>(message: S, loc: Location) -> Self {
//         Diag::new(Some(loc), DiagSeverity::Note, message)
//     }

//     /// Check if the Given Diagnostic is an Error
//     pub fn is_err(&self) -> bool {
//         self.level == DiagSeverity::Error
//     }
// }

// #[cfg(test)]
// pub mod test {

//     use super::*;
//     use syntax::position::*;

//     #[test]
//     fn diagnotics_session_add_erro_has_errors() {
//         let mut sess = DiagSession::new();
//         sess.add(Diag::err("test error"));
//         assert!(sess.has_errors());
//     }

//     #[test]
//     fn create_daignostics() {
//         let warn = Diag::warn("hello");
//         let warn_pos = Diag::warn_pos("world", Cursor::from(123).into());
//         let err = Diag::err("hello");
//         let err_pos = Diag::err_pos("world", Cursor::from(123).into());
//         let note = Diag::warn("hello");
//         let note_pos = Diag::warn_pos("world", Cursor::from(123).into());

//         assert_eq!(warn.message, "hello");
//         assert_eq!(err.message, "hello");
//         assert_eq!(note.message, "hello");
//         assert_eq!(warn_pos.message, "world");
//         assert_eq!(err_pos.message, "world");
//         assert_eq!(note_pos.message, "world");
//         assert_eq!(warn_pos.loc, Some(Location::from(Cursor::from(123))));
//         assert_eq!(err_pos.loc, Some(Location::from(Cursor::from(123))));
//         assert_eq!(note_pos.loc, Some(Location::from(Cursor::from(123))));
//     }
// }
