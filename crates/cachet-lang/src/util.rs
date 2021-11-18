use std::fmt;
use std::iter::FromIterator;

use crate::ast::{Hole, HoleConfig, HoleSpanned};

pub fn collect_eager<T: Iterator, U: FromIterator<T::Item>>(mut iter: T) -> U {
    let result = U::from_iter(&mut iter);
    for _ in iter {}
    result
}

pub fn map_spanned<C: HoleConfig, T, U>(
    x: HoleSpanned<C, T>,
    f: impl FnOnce(HoleSpanned<C, T>) -> Option<U>,
) -> Option<HoleSpanned<C, U>> {
    let span = x.span;
    match f(x) {
        None => None,
        Some(y) => Some(HoleSpanned { span, value: y }),
    }
}

pub fn map_try<C: HoleConfig, T, U>(
    x: Hole<C, T>,
    f: impl FnOnce(T) -> Option<U>,
) -> Option<Hole<C, U>> {
    match x {
        Hole::Full(reachable, y) => f(y).map(|z| Hole::Full(reachable, z)),
        Hole::Empty(reachable) => Some(Hole::Empty(reachable)),
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
    (
        $src:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(Box::new(src))
            }
        }
    };
}
pub(crate) use box_from;

macro_rules! chain_from {
    (
        $src:ty => $mid:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(::std::convert::Into::<$mid>::into(src))
            }
        }
    };
}
pub(crate) use chain_from;

macro_rules! deref_from {
    (
        $src:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(*src)
            }
        }
    };
}
pub(crate) use deref_from;

macro_rules! deref_index {
    (
        $parent:ty[$index:ty] => $output:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::ops::Index<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            type Output = $output;

            fn index(&self, index: $index) -> &Self::Output {
                &self[*index]
            }
        }

        impl$($(<$($generic_params),+>)?)? ::std::ops::IndexMut<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                &mut self[*index]
            }
        }
    };
}
pub(crate) use deref_index;

macro_rules! field_index {
    (
        $parent:ty:$field:ident[$index:ty] => $output:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::ops::Index<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            type Output = $output;

            fn index(&self, index: $index) -> &Self::Output {
                &self.$field[index]
            }
        }

        impl$($(<$($generic_params),+>)?)? ::std::ops::IndexMut<$index> for $parent
        $($(where $($where_clause)+)?)?
        {
            fn index_mut(&mut self, index: $index) -> &mut Self::Output {
                &mut self.$field[index]
            }
        }

        $crate::util::deref_index!(
            $parent[&$index] => $output
            $(| $(<$($generic_params),+>)? $(where $($where_clause)+)?)?
        );
    };
}
pub(crate) use field_index;

macro_rules! typed_index {
    ($vis:vis $index:ident) => {
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
        $vis struct $index(usize);

        // Use a compact representation for `Debug`-formatting indexes, even in
        // pretty mode.
        impl ::std::fmt::Debug for $index {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
                write!(f, "{}({})", stringify!($index), self.0)
            }
        }
    };
}
pub(crate) use typed_index;

macro_rules! typed_field_index {
    (
        $parent:ty:$field:ident[$vis:vis $index:ident] => $output:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        $crate::util::typed_index!($vis $index);
        $crate::util::field_index!(
            $parent:$field[$index] => $output
            $(| $(<$($generic_params),+>)? $(where $($where_clause)+)?)?
        );
    };
}
pub(crate) use typed_field_index;
