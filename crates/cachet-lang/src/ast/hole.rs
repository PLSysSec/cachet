// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut};

use void::{unreachable, Void};

pub type Yes = ();
pub type No = Void;

pub trait Flag: Copy + Debug + PartialEq + private_flag::PrivateFlag {
    type Not: Flag;
    //type And<T: Flag>: Flag;
    //type Or<T: Flag>: Flag;

    fn when<T>(f: impl FnOnce(Self) -> T) -> Hole<(CanBeFull<Self>, CanBeEmpty<Not<Self>>), T>;
}

impl Flag for Yes {
    type Not = No;
    //type And<T: Flag> = T;
    //type Or<T: Flag> = Yes;

    fn when<T>(f: impl FnOnce(Self) -> T) -> Hole<(CanBeFull<Self>, CanBeEmpty<Not<Self>>), T> {
        Hole::full(f(()))
    }
}

impl Flag for No {
    type Not = Yes;
    //type And<T: Flag> = No;
    //type Or<T: Flag> = T;

    fn when<T>(_f: impl FnOnce(Self) -> T) -> Hole<(CanBeFull<Self>, CanBeEmpty<Not<Self>>), T> {
        Hole::empty()
    }
}

pub type Not<T> = <T as Flag>::Not;
//pub type And<T, U> = <T as Flag>::And<U>;
//pub type Or<T, U> = <T as Flag>::Or<U>;

mod private_flag {
    use super::{Debug, No, Yes};

    pub trait PrivateFlag: Copy + Debug + PartialEq {}
    impl PrivateFlag for Yes {}
    impl PrivateFlag for No {}
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CanBeFull<T>(PhantomData<T>);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CanBeEmpty<T>(PhantomData<T>);

pub type Always = (CanBeFull<Yes>, CanBeEmpty<No>);
pub type Sometimes = (CanBeFull<Yes>, CanBeEmpty<Yes>);
pub type Never = (CanBeFull<No>, CanBeEmpty<Yes>);
pub type Unreachable = (CanBeFull<No>, CanBeEmpty<No>);

pub trait HoleConfig: Copy + Debug + PartialEq + private_hole_config::PrivateHoleConfig {
    type CanBeFull: Flag;
    type CanBeEmpty: Flag;
}

impl<F: Flag, E: Flag> HoleConfig for (CanBeFull<F>, CanBeEmpty<E>) {
    type CanBeFull = F;
    type CanBeEmpty = E;
}

mod private_hole_config {
    use super::{CanBeEmpty, CanBeFull, Debug, Flag};

    pub trait PrivateHoleConfig: Copy + Debug + PartialEq {}
    impl<F: Flag, E: Flag> PrivateHoleConfig for (CanBeFull<F>, CanBeEmpty<E>) {}
}

pub trait HoleConfigFields: HoleConfig {
    type CanBeFullField: Copy + Debug + PartialEq;
    type CanBeEmptyField: Copy + Debug + PartialEq;
}

impl<C: HoleConfig> HoleConfigFields for C {
    type CanBeFullField = CanBeFull<<C as HoleConfig>::CanBeFull>;
    type CanBeEmptyField = CanBeEmpty<<C as HoleConfig>::CanBeEmpty>;
}

pub trait CanBeFullHoleConfig: HoleConfig<CanBeFull = Yes> {}
impl<C: HoleConfig<CanBeFull = Yes>> CanBeFullHoleConfig for C {}

pub trait CanBeEmptyHoleConfig: HoleConfig<CanBeEmpty = Yes> {}
impl<C: HoleConfig<CanBeEmpty = Yes>> CanBeEmptyHoleConfig for C {}

pub trait ReachableHoleConfig:
    HoleConfig + private_reachable_hole_config::PrivateReachableHoleConfig
{
    fn default_hole<T: Default>() -> Hole<Self, T>;
}

impl ReachableHoleConfig for Always {
    fn default_hole<T: Default>() -> Hole<Self, T> {
        Hole::full(T::default())
    }
}

impl ReachableHoleConfig for Sometimes {
    fn default_hole<T: Default>() -> Hole<Self, T> {
        Hole::empty()
    }
}

impl ReachableHoleConfig for Never {
    fn default_hole<T: Default>() -> Hole<Self, T> {
        Hole::empty()
    }
}

mod private_reachable_hole_config {
    use super::{Always, HoleConfig, Never, Sometimes};

    pub trait PrivateReachableHoleConfig: HoleConfig {}
    impl PrivateReachableHoleConfig for Always {}
    impl PrivateReachableHoleConfig for Sometimes {}
    impl PrivateReachableHoleConfig for Never {}
}

#[derive(Clone, Copy, PartialEq)]
pub enum Hole<C: HoleConfig, T> {
    Full(C::CanBeFull, T),
    Empty(C::CanBeEmpty),
}

impl<C: HoleConfig, T: Debug> Debug for Hole<C, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Hole::Full(_, value) => Debug::fmt(value, f),
            Hole::Empty(_) => write!(f, "<empty>"),
        }
    }
}

impl<T> Hole<Always, T> {
    pub fn into_inner(self) -> T {
        match self {
            Hole::Empty(void) => unreachable(void),
            Hole::Full(_, x) => x,
        }
    }
}

impl<T> Hole<Sometimes, T> {
    pub fn from_option(option: Option<T>) -> Self {
        match option {
            None => Hole::empty(),
            Some(x) => Hole::full(x),
        }
    }
}

impl<C: CanBeEmptyHoleConfig, T> Hole<C, T> {
    pub const fn empty() -> Self {
        Hole::Empty(())
    }
}

impl<C: CanBeFullHoleConfig, T> Hole<C, T> {
    pub const fn full(x: T) -> Self {
        Hole::Full((), x)
    }

    pub fn expect(self, msg: &str) -> T {
        match self {
            Hole::Empty(_) => panic!("{}", msg),
            Hole::Full(_, x) => x,
        }
    }

    pub fn unwrap(self) -> T {
        self.expect("called `Hole::unwrap()` on an `Empty` value")
    }
}

impl<C: HoleConfig, T> Hole<C, T> {
    pub fn try_full(
        value: T,
    ) -> Hole<(CanBeFull<C::CanBeFull>, CanBeEmpty<Not<C::CanBeFull>>), Self> {
        Self::try_full_with(move || value)
    }

    pub fn try_full_with(
        f: impl FnOnce() -> T,
    ) -> Hole<(CanBeFull<C::CanBeFull>, CanBeEmpty<Not<C::CanBeFull>>), Self> {
        C::CanBeFull::when(move |reachable| Hole::Full(reachable, f()))
    }

    pub fn try_empty() -> Hole<(CanBeFull<C::CanBeEmpty>, CanBeEmpty<Not<C::CanBeEmpty>>), Self> {
        C::CanBeEmpty::when(|reachable| Hole::Empty(reachable))
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Hole::Empty(_) => default,
            Hole::Full(_, x) => x,
        }
    }

    pub fn unwrap_or_else(self, f: impl FnOnce() -> T) -> T {
        match self {
            Hole::Empty(_) => f(),
            Hole::Full(_, x) => x,
        }
    }

    pub fn as_ref(&self) -> Hole<C, &T> {
        match self {
            Hole::Empty(reachable) => Hole::Empty(*reachable),
            Hole::Full(reachable, x) => Hole::Full(*reachable, x),
        }
    }

    pub fn as_mut(&mut self) -> Hole<C, &mut T> {
        match self {
            Hole::Empty(reachable) => Hole::Empty(*reachable),
            Hole::Full(reachable, x) => Hole::Full(*reachable, x),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Hole<C, U> {
        match self {
            Hole::Empty(reachable) => Hole::Empty(reachable),
            Hole::Full(reachable, x) => Hole::Full(reachable, f(x)),
        }
    }
}

impl<T> AsRef<T> for Hole<Always, T> {
    fn as_ref(&self) -> &T {
        &*self
    }
}

impl<T> AsMut<T> for Hole<Always, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut *self
    }
}

impl<T> Borrow<T> for Hole<Always, T> {
    fn borrow(&self) -> &T {
        &*self
    }
}

impl<T> BorrowMut<T> for Hole<Always, T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut *self
    }
}

impl<T> Deref for Hole<Always, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref().into_inner()
    }
}

impl<T> DerefMut for Hole<Always, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut().into_inner()
    }
}

impl<C: ReachableHoleConfig, T: Default> Default for Hole<C, T> {
    fn default() -> Self {
        C::default_hole()
    }
}

impl<C: CanBeFullHoleConfig, T: Index<U>, U> Index<U> for Hole<C, T> {
    type Output = T::Output;

    fn index(&self, index: U) -> &Self::Output {
        &self.as_ref().unwrap()[index]
    }
}

impl<C: CanBeFullHoleConfig, T: IndexMut<U>, U> IndexMut<U> for Hole<C, T> {
    fn index_mut(&mut self, index: U) -> &mut Self::Output {
        &mut self.as_mut().unwrap()[index]
    }
}

impl<C: CanBeFullHoleConfig, T> From<T> for Hole<C, T> {
    fn from(x: T) -> Self {
        Hole::full(x)
    }
}

impl<C: HoleConfig, T> From<Hole<C, T>> for Option<T> {
    fn from(hole: Hole<C, T>) -> Option<T> {
        match hole {
            Hole::Empty(_) => None,
            Hole::Full(_, x) => Some(x),
        }
    }
}
