// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt::{self, Display};

pub use crate::ast::built_ins::*;
pub use crate::ast::ident::*;
pub use crate::ast::span::*;

mod built_ins;
mod ident;
mod span;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CheckKind {
    Assert,
    Assume,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BlockKind {
    Unsafe,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum NegateKind {
    Arithmetic,
    Logical,
}

impl Display for NegateKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                NegateKind::Arithmetic => "-",
                NegateKind::Logical => "!",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CastKind {
    Downcast,
    Upcast,
}

impl CastKind {
    pub const fn is_unsafe(self) -> bool {
        match self {
            CastKind::Downcast => true,
            CastKind::Upcast => false,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CompareKind {
    Eq,
    Neq,
    Lte,
    Gte,
    Lt,
    Gt,
}

impl CompareKind {
    pub fn is_numeric(&self) -> bool {
        match self {
            CompareKind::Lte
            | CompareKind::Gte
            | CompareKind::Lt
            | CompareKind::Gt => true,
            CompareKind::Eq | CompareKind::Neq => false,
        }
    }
}

impl Display for CompareKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                CompareKind::Eq => "==",
                CompareKind::Neq => "!=",
                CompareKind::Lte => "<=",
                CompareKind::Gte => ">=",
                CompareKind::Lt => "<",
                CompareKind::Gt => ">",
            }
        )
    }
}
