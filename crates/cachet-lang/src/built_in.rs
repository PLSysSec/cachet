use crate::ast::ident::{Ident, Path};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::mem::transmute;

macro_rules! ordered_ident_enum {
    ($t:ident { $($i:ident = $l:literal),+ }) => {
        #[repr(usize)]
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum $t {
            $($i),+
        }

        impl $t {
            pub const ALL: [Self;  [$(Self::$i),*].len()] = [$(Self::$i),*];
            pub const COUNT: usize = Self::ALL.len();

            fn idents() -> &'static [Ident] {
                lazy_static! {
                    static ref IDENTS: [Ident; $t::COUNT] = {
                        [$($l.into()),*]
                    };
                }

                return &*IDENTS;
            }

            fn ident_reverse_map() -> &'static HashMap<Ident, $t> {
                lazy_static! {
                    static ref IDENTS: HashMap<Ident, $t> = {
                        $t::idents().iter().enumerate().map(|(i, ident)| (*ident, unsafe {transmute(i)})).collect()
                    };
                }

                return &*IDENTS;
            }

            pub fn ident(self) -> Ident {
                Self::idents()[self as usize]
            }

            pub fn from_ident(ident: Ident) -> Option<Self> {
                Self::ident_reverse_map().get(&ident).cloned()
            }
        }
    }
}

ordered_ident_enum! {
    BuiltInType {
        Unit = "Unit",
        Bool = "Bool",
        Int32 = "Int32",
        Int64 = "Int64",
        UInt16 = "UInt16",
        Double = "Double"
    }
}

impl BuiltInType {
    pub fn path(self) -> Path {
        self.ident().into()
    }

    pub fn from_path(path: Path) -> Option<BuiltInType> {
        if path.has_parent() {
            None
        } else {
            BuiltInType::from_ident(path.ident())
        }
    }

    pub fn index(self) -> usize {
        self as usize
    }

    pub fn from_index(idx: usize) -> Option<BuiltInType> {
        (idx < Self::COUNT).then(|| unsafe { std::mem::transmute(idx) })
    }

    pub const fn supertype(self) -> Option<BuiltInType> {
        match self {
            BuiltInType::Bool => Some(BuiltInType::Int32),
            BuiltInType::Unit
            | BuiltInType::Int64
            | BuiltInType::Int32
            | BuiltInType::UInt16
            | BuiltInType::Double => None,
        }
    }

    pub const fn is_numeric(self) -> bool {
        match self {
            BuiltInType::Bool
            | BuiltInType::Int32
            | BuiltInType::Int64
            | BuiltInType::UInt16
            | BuiltInType::Double => true,
            BuiltInType::Unit => false,
        }
    }

    pub const fn is_signed_numeric(self) -> bool {
        match self {
            BuiltInType::Bool | BuiltInType::Int32 | BuiltInType::Int64 | BuiltInType::Double => {
                true
            }
            BuiltInType::UInt16 | BuiltInType::Unit => false,
        }
    }
}

ordered_ident_enum! {
    BuiltInVar {
        Unit = "unit",
        True = "true",
        False = "false"
    }
}

impl BuiltInVar {
    pub fn path(self) -> Path {
        self.ident().into()
    }

    pub fn from_path(path: Path) -> Option<BuiltInVar> {
        if path.has_parent() {
            None
        } else {
            BuiltInVar::from_ident(path.ident())
        }
    }

    pub const fn built_in_type(self) -> BuiltInType {
        match self {
            BuiltInVar::Unit => BuiltInType::Unit,
            BuiltInVar::True | BuiltInVar::False => BuiltInType::Bool,
        }
    }
}
