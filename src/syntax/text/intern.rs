//! String Interning
//!
//! This module contains types to model interned strings. The idea is
//! that a string can be stored in an `Interner` only once. The
//! contents of the string can't be accessed without the `Interner`,
//! but interned strings can be compared for equality quickly.

use std::collections::HashMap;

/// Interner
///
/// Keeps a list of intered strings and a map to look them up.
pub struct Interner {
    lookup: HashMap<String, Ident>,
    next: u32,
}

/// Interned String
///
/// Represents a kind of interned string value. Known keywords appear
/// in here to allow them to be easily matched.
#[derive(Debug, PartialEq, Hash, Eq, Copy, Clone)]
pub enum Ident {
    /// the `else` keyword
    Else,
    /// the `end` keyword
    End,
    /// the `false` keyword
    False,
    /// the `fn` keyword
    Fn,
    /// the `if` keyword
    If,
    /// the `let` keyword
    Let,
    /// the `print` keyword
    Print,
    /// the `true` keyword
    True,
    /// the `unless` keyword
    Unless,
    /// the `until` keyword
    Until,
    /// the `var` keyword
    Var,
    /// the `while` keyword
    While,
    /// Other unknown identifier values.
    Unknown(u32),
}

impl Interner {
    /// Intern a String
    ///
    /// If the string is already in this `Interner` then the existing
    /// inered string is returned. If this is a new string a new
    /// intern value is allocated and returned.
    pub fn intern(&mut self, value: &str) -> Ident {
        match value {
            "else" => Ident::Else,
            "end" => Ident::End,
            "false" => Ident::False,
            "fn" => Ident::Fn,
            "if" => Ident::If,
            "let" => Ident::Let,
            "print" => Ident::Print,
            "true" => Ident::True,
            "unless" => Ident::Unless,
            "until" => Ident::Until,
            "var" => Ident::Var,
            "while" => Ident::While,
            _ => self.intern_unknown(value),
        }
    }

    /// Intern a New Value
    ///
    /// When we have checked that the value isn't a known ident this
    /// can be used to create a new `Ident::Unknown` entry in the
    /// table.
    fn intern_unknown(&mut self, value: &str) -> Ident {
        if let Some(found) = self.lookup.get(value) {
            return *found;
        }
        let ident = Ident::Unknown(self.next);
        self.next += 1;
        self.lookup.insert(value.into(), ident);
        ident
    }

    /// Borrow the Interned value
    ///
    /// Used to conver the interned value back to a string.
    pub fn interned_value(&self, ident: Ident) -> &str {
        match ident {
            Ident::Else => "else",
            Ident::End => "end",
            Ident::False => "false",
            Ident::Fn => "fn",
            Ident::If => "if",
            Ident::Let => "let",
            Ident::Print => "print",
            Ident::True => "true",
            Ident::Unless => "unless",
            Ident::Until => "until",
            Ident::Var => "var",
            Ident::While => "while",
            _ => self
                .lookup
                .iter()
                .find(|(_, v)| **v == ident)
                .map(|(k, _)| &k[..])
                .unwrap_or(""),
        }
    }
}

impl Default for Interner {
    fn default() -> Self {
        Interner {
            lookup: HashMap::default(),
            next: 1,
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn intern_dedupes_words() {
        let mut interner = Interner::default();

        let foo1 = interner.intern("foo");
        let foo2 = interner.intern("foo");
        let bar = interner.intern("bar");

        assert_eq!(foo1, foo2);
        assert_ne!(foo1, bar);
        assert_ne!(foo2, bar);
    }
}
