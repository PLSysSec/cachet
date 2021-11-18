// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error;
use std::fmt::{self, Debug, Display};

pub use codespan::Span;

use crate::ast::hole::{Always, CanBeEmptyHoleConfig, Hole, HoleConfig, Never, Sometimes};

// TODO(spinda): Define own `Span` type as enum with `Internal` and `External`
// variants.

pub type Spanned<T> = HoleSpanned<Always, T>;
pub type MaybeSpanned<T> = HoleSpanned<Sometimes, T>;

#[derive(Clone, Copy)]
pub struct HoleSpanned<C: HoleConfig, T> {
    pub span: Hole<C, Span>,
    pub value: T,
}

impl<C: HoleConfig, T> HoleSpanned<C, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> HoleSpanned<C, U> {
        HoleSpanned {
            span: self.span,
            value: f(self.value),
        }
    }

    pub const fn as_ref(&self) -> HoleSpanned<C, &T> {
        HoleSpanned {
            span: self.span,
            value: &self.value,
        }
    }

    pub fn as_mut(&mut self) -> HoleSpanned<C, &mut T> {
        HoleSpanned {
            span: self.span,
            value: &mut self.value,
        }
    }
}

impl<T> HoleSpanned<Always, T> {
    pub const fn new(span: Span, value: T) -> Self {
        Self {
            span: Hole::full(span),
            value,
        }
    }

    pub fn span(&self) -> Span {
        self.span.into_inner()
    }
}

impl<T> HoleSpanned<Sometimes, T> {
    pub fn new(span: Option<Span>, value: T) -> Self {
        Self {
            span: Hole::from_option(span),
            value,
        }
    }

    pub fn span(&self) -> Option<Span> {
        self.span.into()
    }
}

impl<T> HoleSpanned<Never, T> {
    pub const fn new(value: T) -> Self {
        Self {
            span: Hole::empty(),
            value,
        }
    }
}

impl<C: HoleConfig, T: Clone> HoleSpanned<C, &T> {
    pub fn cloned(self) -> HoleSpanned<C, T> {
        self.map(|value| value.clone())
    }
}

impl<C: HoleConfig, T: Copy> HoleSpanned<C, &T> {
    pub fn copied(self) -> HoleSpanned<C, T> {
        self.map(|value| *value)
    }
}

impl<C: HoleConfig, T: Debug> Debug for HoleSpanned<C, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.span {
            Hole::Full(_, span) => {
                Debug::fmt(&self.value, f)?;
                write!(f, " @ {}", span)
            }
            Hole::Empty(_) => Debug::fmt(&self.value, f),
        }
    }
}

impl<C: HoleConfig, T: Display> Display for HoleSpanned<C, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Display::fmt(&self.value, f)
    }
}

impl<C: HoleConfig, T: error::Error> error::Error for HoleSpanned<C, T> {
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

impl<C: CanBeEmptyHoleConfig, T> From<T> for HoleSpanned<C, T> {
    fn from(value: T) -> Self {
        HoleSpanned {
            span: Hole::empty(),
            value,
        }
    }
}

impl<T> From<HoleSpanned<Always, T>> for HoleSpanned<Sometimes, T> {
    fn from(spanned: HoleSpanned<Always, T>) -> Self {
        HoleSpanned {
            span: Hole::full(spanned.span()),
            value: spanned.value,
        }
    }
}

impl<T> From<HoleSpanned<Never, T>> for HoleSpanned<Sometimes, T> {
    fn from(spanned: HoleSpanned<Never, T>) -> Self {
        HoleSpanned {
            span: Hole::empty(),
            value: spanned.value,
        }
    }
}
