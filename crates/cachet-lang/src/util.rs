use std::borrow::{Borrow, BorrowMut};
use std::fmt;
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

use crate::ast::Spanned;

pub enum MaybeOwned<'a, T: 'a + ToOwned + ?Sized> {
    Borrowed(&'a mut T),
    Owned(<T as ToOwned>::Owned),
}

impl<T: ToOwned + ?Sized> MaybeOwned<'_, T> {
    pub fn into_owned(self) -> <T as ToOwned>::Owned {
        match self {
            MaybeOwned::Borrowed(borrowed) => borrowed.to_owned(),
            MaybeOwned::Owned(owned) => owned,
        }
    }
}

impl<T: ToOwned + ?Sized> AsRef<T> for MaybeOwned<'_, T> {
    fn as_ref(&self) -> &T {
        &*self
    }
}

impl<T: ToOwned + ?Sized> AsMut<T> for MaybeOwned<'_, T>
where
    T::Owned: BorrowMut<T>,
{
    fn as_mut(&mut self) -> &mut T {
        &mut *self
    }
}

impl<T: ToOwned + ?Sized> Borrow<T> for MaybeOwned<'_, T> {
    fn borrow(&self) -> &T {
        &*self
    }
}

impl<T: ToOwned + ?Sized> BorrowMut<T> for MaybeOwned<'_, T>
where
    T::Owned: BorrowMut<T>,
{
    fn borrow_mut(&mut self) -> &mut T {
        &mut *self
    }
}

impl<T: ToOwned + ?Sized> Deref for MaybeOwned<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            MaybeOwned::Borrowed(borrowed) => borrowed,
            MaybeOwned::Owned(owned) => owned.borrow(),
        }
    }
}

impl<T: ToOwned + ?Sized> DerefMut for MaybeOwned<'_, T>
where
    T::Owned: BorrowMut<T>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            MaybeOwned::Borrowed(borrowed) => borrowed,
            MaybeOwned::Owned(owned) => owned.borrow_mut(),
        }
    }
}

pub fn collect_eager<T, U: FromIterator<T>>(mut iter: impl Iterator<Item = T>) -> U {
    let result = U::from_iter(&mut iter);
    for _ in iter {}
    result
}

pub fn map_spanned<T, U>(
    x: Spanned<T>,
    f: impl FnOnce(Spanned<T>) -> Option<U>,
) -> Option<Spanned<U>> {
    let span = x.span;
    match f(x) {
        None => None,
        Some(y) => Some(Spanned::new(span, y)),
    }
}

// TODO(spinda): Deduplicate with the `cachet-compiler` crate.
pub fn fmt_join<T: fmt::Display>(
    f: &mut impl fmt::Write,
    sep: impl fmt::Display,
    mut iter: impl Iterator<Item = T>,
) -> Result<(), fmt::Error> {
    if let Some(first_item) = iter.next() {
        write!(f, "{}", first_item)?;
        for item in iter {
            write!(f, "{}{}", sep, item)?;
        }
    }
    Ok(())
}

pub fn fmt_join_or<T, W: fmt::Write>(
    f: &mut W,
    iter: impl Iterator<Item = T>,
    fmt_item: impl Fn(&mut W, T) -> Result<(), fmt::Error>,
) -> Result<(), fmt::Error> {
    let mut iter = iter.peekable();
    if let Some(first_item) = iter.next() {
        fmt_item(f, first_item)?;
        if let Some(second_item) = iter.next() {
            write!(
                f,
                "{}",
                match iter.peek() {
                    Some(_) => ", ",
                    None => " or ",
                }
            )?;
            fmt_item(f, second_item)?;
            while let Some(item) = iter.next() {
                write!(
                    f,
                    "{}",
                    match iter.peek() {
                        Some(_) => ", ",
                        None => ", or ",
                    }
                )?;
                fmt_item(f, item)?;
            }
        }
    }
    Ok(())
}

macro_rules! box_from {
    ($src:ty => $dst:ty) => {
        impl ::std::convert::From<$src> for $dst {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(Box::new(src))
            }
        }
    };
}
pub(crate) use box_from;

macro_rules! deref_from {
    ($src:ty => $dst:ty) => {
        impl ::std::convert::From<$src> for $dst {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(*src)
            }
        }
    };
}
pub(crate) use deref_from;

macro_rules! deref_index {
    ($parent:ty[$index:ty] => $output:ty) => {
        impl ::std::ops::Index<$index> for $parent {
            type Output = $output;

            fn index(&self, index: $index) -> &Self::Output {
                &self[*index]
            }
        }

        impl ::std::ops::IndexMut<$index> for $parent {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                &mut self[*index]
            }
        }
    };
}
pub(crate) use deref_index;

macro_rules! field_index {
    ($parent:ty:$field:ident[$index:ty] => $output:ty) => {
        impl ::std::ops::Index<$index> for $parent {
            type Output = $output;

            fn index(&self, index: $index) -> &Self::Output {
                &self.$field[index]
            }
        }

        impl ::std::ops::IndexMut<$index> for $parent {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }

        $crate::util::deref_index!($parent[&$index] => $output);
    };
}
pub(crate) use field_index;

macro_rules! typed_index {
    ($parent:ty:$field:ident[$index:ident] => $output:ty) => {
        #[derive(
            ::derive_more::Display,
            ::derive_more::From,
            ::derive_more::Into,
            ::std::clone::Clone,
            ::std::cmp::Eq,
            ::std::cmp::Ord,
            ::std::cmp::PartialEq,
            ::std::cmp::PartialOrd,
            ::std::hash::Hash,
            ::std::marker::Copy,
        )]
        pub struct $index(usize);

        // Use a compact representation for `Debug`-formatting indexes, even in
        // pretty mode.
        impl ::std::fmt::Debug for $index {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
                write!(f, "{}({})", stringify!($index), self.0)
            }
        }

        $crate::util::field_index!($parent:$field[$index] => $output);
    };
}
pub(crate) use typed_index;
