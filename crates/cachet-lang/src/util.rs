use std::fmt;
use std::iter::FromIterator;

use crate::ast::Spanned;

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
