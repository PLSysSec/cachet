use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};
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

impl<T: Debug + ToOwned + ?Sized> Debug for MaybeOwned<'_, T>
where
    T::Owned: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            MaybeOwned::Borrowed(borrowed) => Debug::fmt(borrowed, f),
            MaybeOwned::Owned(owned) => Debug::fmt(owned, f),
        }
    }
}
