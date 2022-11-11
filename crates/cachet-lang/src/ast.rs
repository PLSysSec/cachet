// vim: set tw=99 ts=4 sts=4 sw=4 et:

use derive_more::{Display, From};

pub use crate::ast::ident::*;
pub use crate::ast::span::*;

mod ident;
mod span;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VarParamKind {
    In,
    Mut,
    Out,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum CheckKind {
    #[display(fmt = "assert")]
    Assert,
    #[display(fmt = "assume")]
    Assume,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum BlockKind {
    #[display(fmt = "unsafe")]
    Unsafe,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum ForInOrder {
    #[display(fmt = "asc")]
    Ascending,
    #[display(fmt = "desc")]
    Descending,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum NegateKind {
    #[display(fmt = "-")]
    Arith,
    #[display(fmt = "~")]
    Bitwise,
    #[display(fmt = "!")]
    Logical,
}

/// Describes how "safe" a cast is.
/// The variants are ordered so that less safe variants < more safe variants
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum CastSafety {
    Unsafe,     // Downcasting (requires unsafe context)
    Truncating, // Numeric conversions that lose data (requires explicit annotation)
    Lossless,   // Upcasting and value-preserving numeric conversions (can be safely inferred)
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, From, PartialEq)]
pub enum BinOper {
    Arith(ArithBinOper),
    Bitwise(BitwiseBinOper),
    Compare(CompareBinOper),
    Logical(LogicalBinOper),
}

impl BinOper {
    pub const fn is_short_circuiting(&self) -> bool {
        match self {
            BinOper::Logical(_) => true,
            BinOper::Arith(_) | BinOper::Bitwise(_) | BinOper::Compare(_) => false,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum ArithBinOper {
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Mod,
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum BitwiseBinOper {
    #[display(fmt = "<<")]
    Shl,
    #[display(fmt = ">>")]
    Shr,
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
