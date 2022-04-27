// vim: set tw=99 ts=4 sts=4 sw=4 et:

use derive_more::{Display, From};

pub use crate::ast::ident::*;
pub use crate::ast::span::*;

pub mod ident;
mod span;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VarParamKind {
    In,
    Mut,
    Out,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CheckKind {
    Assert,
    Assume,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BlockKind {
    Unsafe,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum NegateKind {
    #[display(fmt = "-")]
    Arithmetic,
    #[display(fmt = "!")]
    Logical,
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

    pub const fn reverse(self) -> Self {
        match self {
            CastKind::Downcast => CastKind::Upcast,
            CastKind::Upcast => CastKind::Downcast,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, From, Hash, PartialEq)]
pub enum CompareKind {
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = "!=")]
    Neq,
    Numeric(NumericCompareKind),
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum NumericCompareKind {
    #[display(fmt = "<=")]
    Lte,
    #[display(fmt = ">=")]
    Gte,
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = ">")]
    Gt,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum ArithKind {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
}
