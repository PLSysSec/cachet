// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::borrow::Borrow;
use std::error;
use std::fmt;
use std::iter::FromIterator;
use std::ops::Deref;
use std::vec::IntoIter as VecIntoIter;

use derive_more::Display;
use internment::Intern;

use crate::util::{deref_from, fmt_join};

pub use codespan::Span;

#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub const fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }

    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            span: self.span,
            value: &self.value,
        }
    }

    pub fn as_mut(&mut self) -> Spanned<&mut T> {
        Spanned {
            span: self.span,
            value: &mut self.value,
        }
    }
}

impl<T: Clone> Spanned<&T> {
    pub fn cloned(self) -> Spanned<T> {
        self.map(|value| value.clone())
    }
}

impl<T: Copy> Spanned<&T> {
    pub fn copied(self) -> Spanned<T> {
        self.map(|value| *value)
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.value, f)?;
        write!(f, " @ {}", self.span)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.value, f)
    }
}

impl<T: error::Error> error::Error for Spanned<T> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.value.source()
    }

    fn description(&self) -> &str {
        #[allow(deprecated)]
        self.value.description()
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        #[allow(deprecated)]
        self.value.cause()
    }
}

#[derive(Clone, Copy)]
pub struct MaybeSpanned<T> {
    pub span: Option<Span>,
    pub value: T,
}

impl<T> MaybeSpanned<T> {
    pub const fn new(span: Option<Span>, value: T) -> Self {
        Self { span, value }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> MaybeSpanned<U> {
        MaybeSpanned {
            span: self.span,
            value: f(self.value),
        }
    }

    pub const fn as_ref(&self) -> MaybeSpanned<&T> {
        MaybeSpanned {
            span: self.span,
            value: &self.value,
        }
    }

    pub fn as_mut(&mut self) -> MaybeSpanned<&mut T> {
        MaybeSpanned {
            span: self.span,
            value: &mut self.value,
        }
    }
}

impl<T: Clone> MaybeSpanned<&T> {
    pub fn cloned(self) -> MaybeSpanned<T> {
        self.map(|value| value.clone())
    }
}

impl<T: Copy> MaybeSpanned<&T> {
    pub fn copied(self) -> MaybeSpanned<T> {
        self.map(|value| *value)
    }
}

impl<T: fmt::Debug> fmt::Debug for MaybeSpanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.span {
            Some(span) => fmt::Debug::fmt(&Spanned::new(span, &self.value), f),
            None => fmt::Debug::fmt(&self.value, f),
        }
    }
}

impl<T: fmt::Display> fmt::Display for MaybeSpanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.value, f)
    }
}

impl<T: error::Error> error::Error for MaybeSpanned<T> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.value.source()
    }

    fn description(&self) -> &str {
        #[allow(deprecated)]
        self.value.description()
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        #[allow(deprecated)]
        self.value.cause()
    }
}

impl<T> From<Spanned<T>> for MaybeSpanned<T> {
    fn from(spanned: Spanned<T>) -> Self {
        MaybeSpanned::new(Some(spanned.span), spanned.value)
    }
}

impl<T> From<T> for MaybeSpanned<T> {
    fn from(value: T) -> Self {
        MaybeSpanned::new(None, value)
    }
}

// TODO(spinda): Expand derives.
#[derive(Clone, Copy, Display, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ident(Intern<String>);

impl<T: ?Sized> AsRef<T> for Ident
where
    String: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        (**self).as_ref()
    }
}

impl<T: ?Sized> Borrow<T> for Ident
where
    String: Borrow<T>,
{
    fn borrow(&self) -> &T {
        (**self).borrow()
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&**self, f)
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s.into())
    }
}

impl From<&String> for Ident {
    fn from(s: &String) -> Self {
        String::from(s).into()
    }
}

impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        String::from(s).into()
    }
}

impl From<&mut str> for Ident {
    fn from(s: &mut str) -> Self {
        String::from(s).into()
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        (*ident).clone()
    }
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Path(Intern<PathNode>);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct PathNode {
    parent: Option<Path>,
    ident: Ident,
}

impl Path {
    pub fn new(parent: Option<Path>, ident: Ident) -> Self {
        Path(Intern::new(PathNode { parent, ident }))
    }

    pub fn parent(self) -> Option<Path> {
        self.0.parent
    }

    pub fn has_parent(self) -> bool {
        self.parent().is_some()
    }

    pub fn ident(self) -> Ident {
        self.0.ident
    }

    pub fn member(self, ident: Ident) -> Path {
        Path::new(Some(self), ident)
    }

    pub fn push(&mut self, ident: Ident) {
        *self = self.member(ident);
    }

    pub fn len(mut self) -> usize {
        let mut len = 0;
        while let Some(parent) = self.parent() {
            len += 1;
            self = parent;
        }
        len + 1
    }

    pub fn segments(mut self) -> Vec<Ident> {
        let mut segments = Vec::with_capacity(self.len());
        loop {
            segments.push(self.ident());
            if let Some(parent) = self.parent() {
                self = parent;
            } else {
                break;
            }
        }
        segments.reverse();
        segments
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&format!("{}", self), f)
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join(f, "::", self.into_iter())
    }
}

impl From<Ident> for Path {
    fn from(ident: Ident) -> Self {
        Path::new(None, ident)
    }
}

deref_from!(&Ident => Path);

impl Extend<Ident> for Path {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = Ident>,
    {
        for ident in iter.into_iter() {
            self.push(ident);
        }
    }
}

impl<'a> Extend<&'a Ident> for Path {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = &'a Ident>,
    {
        self.extend(iter.into_iter().copied());
    }
}

impl FromIterator<Ident> for Path {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Ident>,
    {
        let mut iter = iter.into_iter();
        let mut path = Path::from(iter.next().expect("Paths can't be empty"));
        path.extend(iter);
        path
    }
}

impl<'a> FromIterator<&'a Ident> for Path {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = &'a Ident>,
    {
        Path::from_iter(iter.into_iter().copied())
    }
}

impl IntoIterator for Path {
    type Item = Ident;
    type IntoIter = VecIntoIter<Ident>;

    fn into_iter(self) -> Self::IntoIter {
        self.segments().into_iter()
    }
}

impl IntoIterator for &Path {
    type Item = Ident;
    type IntoIter = <Path as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (*self).into_iter()
    }
}
