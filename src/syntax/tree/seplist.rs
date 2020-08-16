//! Separeted Syntax List
//!
//! This module holds the definition of the `SepList<T, S>`
//! type. Separed lists are used in the syntax tree to hold delimited
//! items such as parameter or argument lists.
//!
//! A `SepList` is made up of two lists of items, the main tokens and
//! the separators.

use super::Token;
use std::marker::PhantomData;

/// The separated list type holds a list of syntax items and the
/// separators between then.
#[derive(Debug, PartialEq)]
pub struct SepList<T, S = Token> {
    /// The items in the list
    items: Vec<T>,
    /// The separators between the items
    separators: Vec<S>,
}

impl<T, S> SepList<T, S> {
    /// Create a new seplist from the given items and separators
    ///
    /// The separator length should be equal or 1 shorter than the
    /// items length.
    pub fn new(items: Vec<T>, separators: Vec<S>) -> Self {
        SepList { items, separators }
    }

    /// Create an empty separated list
    ///
    /// The new list will contain no items and no separators. This is
    /// mainly useful for tests or when fabricating trees by hand. The
    /// parser will usually genrate an empty list by calling
    /// `SepList::builder().build()`
    pub fn empty() -> Self {
        SepList::new(Vec::new(), Vec::new())
    }

    /// Create a list builder. This provides a structured way of
    /// incrementally building a separated list.
    pub fn builder() -> SepListBuilder<T, S, Item> {
        SepListBuilder {
            items: Vec::new(),
            separators: Vec::new(),
            state: Default::default(),
        }
    }

    /// Borrow the separators as a slice
    ///
    /// Standard iteration of this collection just accesses the main
    /// items. This allows access to the separators too.
    pub fn separators(&self) -> &[S] {
        &self.separators
    }
}

impl<T, S> std::ops::Deref for SepList<T, S> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.items
    }
}

/// Fluent typestate API for builing a separated list
pub struct SepListBuilder<T, S, State> {
    /// The buffered items for this list
    items: Vec<T>,
    /// The buffered separators for this list
    separators: Vec<S>,
    /// Phantom state data
    state: PhantomData<State>,
}

/// Initial state for the separated list builder
pub struct Item {}

/// Separated list builder state when item has been seen
pub struct Separator {}

impl<T, S, State> SepListBuilder<T, S, State> {
    /// Finish building the list
    pub fn build(self) -> SepList<T, S> {
        SepList::new(self.items, self.separators)
    }
}

impl<T, S> SepListBuilder<T, S, Item> {
    /// Push an item into the separated list and wait for a separator
    pub fn push_item(mut self, item: T) -> SepListBuilder<T, S, Separator> {
        self.items.push(item);
        SepListBuilder {
            items: self.items,
            separators: self.separators,
            state: Default::default(),
        }
    }
}

impl<T, S> SepListBuilder<T, S, Separator> {
    /// Push a separator onto the list and wait for another item
    pub fn push_sep(mut self, sep: S) -> SepListBuilder<T, S, Item> {
        self.separators.push(sep);
        SepListBuilder {
            items: self.items,
            separators: self.separators,
            state: Default::default(),
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn create_new_seplist() {
        let empty = SepList::<(), u32>::new(Vec::new(), Vec::new());
        let with_items = SepList::new(vec![1, 2, 4, 8], vec![',', '!', '*']);

        assert_eq!(0, empty.len());
        assert_eq!(4, with_items.len());
    }

    #[test]
    fn seplist_builder() {
        let list = SepList::builder()
            .push_item(123)
            .push_sep(',')
            .push_item(456)
            .push_sep('.')
            .build();

        assert_eq!(2, list.len());
        assert_eq!(Some(&123), list.get(0));
        assert_eq!(Some(&456), list.get(1));
        assert_eq!(None, list.get(2));
        assert_eq!(Some(&','), list.separators().get(0));
        assert_eq!(Some(&'.'), list.separators().get(1));
        assert_eq!(None, list.separators().get(2));
    }
}
