// vim: set tw=99 ts=4 sts=4 sw=4 et:

use derive_more::{Display, From};

pub use crate::ast::ident::*;
pub use crate::ast::span::*;

mod ident;
mod span;

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum VarParamKind {
    Value { is_mut: bool },
    Ref(VarRefKind),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VarRefKind {
    In,
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
    Arith,
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

#[derive(Clone, Copy, Debug, Display, Eq, Hash, From, PartialEq)]
pub enum BinOper {
    Arith(ArithBinOper),
    Bitwise(BitwiseBinOper),
    Compare(CompareBinOper),
    Logical(LogicalBinOper),
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum ArithBinOper {
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum BitwiseBinOper {
    #[display(fmt = "<<")]
    Shl,
    #[display(fmt = "&")]
    And,
    #[display(fmt = "^")]
    Xor,
    #[display(fmt = "|")]
    Or,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, Hash, PartialEq)]
pub enum CompareBinOper {
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = "!=")]
    Neq,
    Numeric(NumericCompareBinOper),
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum NumericCompareBinOper {
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = "<=")]
    Lte,
    #[display(fmt = ">=")]
    Gte,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum LogicalBinOper {
    #[display(fmt = "&&")]
    And,
    #[display(fmt = "||")]
    Or,
}
