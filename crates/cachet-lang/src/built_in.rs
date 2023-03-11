use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

use derive_more::Display;
use enum_iterator::IntoEnumIterator;
use enumset::EnumSetType;
use lazy_static::lazy_static;

use crate::ast::{CastSafety, Ident, Path};

macro_rules! impl_ordered_ident_enum {
    ($t:ident) => {
        impl IdentEnum for $t {
            fn idents() -> &'static HashMap<$t, Ident> {
                lazy_static! {
                    static ref IDENTS: HashMap<$t, Ident> = {
                        $t::into_enum_iter()
                            .map(|t| (t, Ident::from_string(t)))
                            .collect()
                    };
                }

                return &*IDENTS;
            }

            fn ident_reverse_map() -> &'static HashMap<Ident, $t> {
                lazy_static! {
                    static ref IDENTS: HashMap<Ident, $t> =
                        { $t::idents().iter().map(|(t, i)| (*i, *t)).collect() };
                }

                return &*IDENTS;
            }
        }
    };
}

pub trait IdentEnum: 'static + Sized + Copy + Hash + Eq + IntoEnumIterator {
    const COUNT: usize = Self::ITEM_COUNT;

    fn idents() -> &'static HashMap<Self, Ident>;
    fn ident_reverse_map() -> &'static HashMap<Ident, Self>;

    fn ident(&self) -> Ident {
        Self::idents()[self]
    }

    fn from_ident(ident: Ident) -> Option<Self> {
        Self::ident_reverse_map().get(&ident).copied()
    }

    fn path(self) -> Path {
        self.ident().into()
    }

    fn from_path(path: Path) -> Option<Self> {
        if path.has_parent() {
            None
        } else {
            Self::from_ident(path.ident())
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Hash, Ord, PartialOrd, IntoEnumIterator, PartialEq, Eq)]
pub enum Width {
    W8 = 8,
    W16 = 16,
    W32 = 32,
    W64 = 64,
}

#[derive(Clone, Copy, Debug, Display, Hash, IntoEnumIterator, PartialEq, Eq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, IntoEnumIterator, Hash)]
pub enum BuiltInType {
    Unit,
    Bool,
    Integral(Signedness, Width),
    Double,
}

impl BuiltInType {
    pub const INT8: Self = BuiltInType::Integral(Signedness::Signed, Width::W8);
    pub const INT16: Self = BuiltInType::Integral(Signedness::Signed, Width::W16);
    pub const INT32: Self = BuiltInType::Integral(Signedness::Signed, Width::W32);
    pub const INT64: Self = BuiltInType::Integral(Signedness::Signed, Width::W64);

    pub const UINT8: Self = BuiltInType::Integral(Signedness::Unsigned, Width::W8);
    pub const UINT16: Self = BuiltInType::Integral(Signedness::Unsigned, Width::W16);
    pub const UINT32: Self = BuiltInType::Integral(Signedness::Unsigned, Width::W32);
    pub const UINT64: Self = BuiltInType::Integral(Signedness::Unsigned, Width::W64);
}

impl Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Unit => "Unit",
                Self::Bool => "Bool",
                &Self::INT8 => "Int8",
                &Self::INT16 => "Int16",
                &Self::INT32 => "Int32",
                &Self::INT64 => "Int64",
                &Self::UINT8 => "UInt8",
                &Self::UINT16 => "UInt16",
                &Self::UINT32 => "UInt32",
                &Self::UINT64 => "UInt64",
                Self::Double => "Double",
            }
        )
    }
}

impl_ordered_ident_enum!(BuiltInType);

impl BuiltInType {
    pub fn casts_to(self, other: Self) -> CastSafety {
        use BuiltInType::*;

        // You can always cast to yourself
        if self == other {
            return CastSafety::Lossless;
        }

        match (self, other) {
            // Casting an integral to a larger integral is always safe
            (Integral(_, self_width), Integral(_, other_width)) if other_width > self_width => {
                CastSafety::Lossless
            }

            // Casting to a smaller type always causes data loss.
            (Integral(_, self_width), Integral(_, other_width)) if other_width < self_width => {
                CastSafety::Truncating
            }

            // Casting to the same size but different signedness also causes data loss
            (Integral(self_sign, self_width), Integral(other_sign, other_width))
                if other_width == self_width && other_sign != self_sign =>
            {
                CastSafety::Truncating
            }

            // Integers (signed or unsigned) of 52 bits or less can be safely cast to Doubles
            (Integral(_, self_width), Double) => {
                if (self_width as u8) < 52 {
                    CastSafety::Lossless
                } else {
                    CastSafety::Truncating
                }
            }

            // Dataloss is always possible when casting from a floating point type to an integral
            // type
            (Double, Integral(..)) => CastSafety::Truncating,

            // Bools can cast to integrals and doubles safely
            (Bool, Integral(..) | Double) => CastSafety::Lossless,

            _ => CastSafety::Unsafe,
        }
    }

    pub const fn is_numeric(self) -> bool {
        match self {
            BuiltInType::Bool | BuiltInType::Integral(..) | BuiltInType::Double => true,
            BuiltInType::Unit => false,
        }
    }

    pub const fn is_signed_numeric(self) -> bool {
        match self {
            BuiltInType::Bool
            | BuiltInType::Integral(Signedness::Signed, _)
            | BuiltInType::Double => {
                debug_assert!(self.is_numeric());
                true
            }
            BuiltInType::Unit | BuiltInType::Integral(Signedness::Unsigned, _) => false,
        }
    }

    pub const fn is_integral(self) -> bool {
        match self {
            BuiltInType::Bool | BuiltInType::Integral(..) => {
                debug_assert!(self.is_numeric());
                true
            }
            BuiltInType::Unit | BuiltInType::Double => false,
        }
    }
}

#[derive(Clone, Copy, Display, Debug, Hash, IntoEnumIterator, PartialEq, Eq)]
pub enum BuiltInVar {
    #[display(fmt = "unit")]
    Unit,
    #[display(fmt = "true")]
    True,
    #[display(fmt = "false")]
    False,
}

impl_ordered_ident_enum!(BuiltInVar);

impl BuiltInVar {
    pub const fn built_in_type(self) -> BuiltInType {
        match self {
            BuiltInVar::Unit => BuiltInType::Unit,
            BuiltInVar::True | BuiltInVar::False => BuiltInType::Bool,
        }
    }
}

#[derive(Display, EnumSetType, Debug, Hash, IntoEnumIterator)]
pub enum BuiltInAttr {
    #[display(fmt = "prelude")]
    Prelude,

    #[display(fmt = "spec")]
    Spec,

    #[display(fmt = "refined")]
    Refined,
}

impl_ordered_ident_enum!(BuiltInAttr);
