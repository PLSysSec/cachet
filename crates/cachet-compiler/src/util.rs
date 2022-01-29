// vim: set tw=99 ts=4 sts=4 sw=4 et:

#![allow(dead_code)]

use std::borrow::{Borrow, BorrowMut};
use std::fmt;
use std::ops::{Deref, DerefMut};

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

pub fn fmt_join_trailing<T: fmt::Display>(
    f: &mut impl fmt::Write,
    sep: impl fmt::Display,
    iter: impl Iterator<Item = T>,
) -> Result<(), fmt::Error> {
    for item in iter {
        write!(f, "{}{}", item, sep)?;
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
