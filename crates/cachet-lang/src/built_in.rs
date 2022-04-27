use crate::ast::ident::{Ident, Path};

macro_rules! count {
    () => (0usize);
    ( $x:ident $($xs:ident)* ) => (1usize + count!($($xs)*));
}

macro_rules! ordered_ident_enum {
    ($t:ident { $($i:ident = $l:literal),+ }) => {
        #[repr(usize)]
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum $t {
            $($i),+
        }

        impl $t {
            pub const ALL: [Self; count!($($i)*)] = [$(Self::$i),*];

            pub fn ident(self) -> Ident {
                match self {
                    $(
                        Self::$i => Ident::from($l)
                    ),+
                }
            }

            pub fn from_ident(ident: Ident) -> Option<Self> {
                match ident.as_ref() {
                    $(
                        $l => Some(Self::$i)
                    ),+,
                    _ => None
                }
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
        (idx < Self::ALL.len()).then(|| unsafe { std::mem::transmute(idx) })
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
