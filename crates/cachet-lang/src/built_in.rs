use crate::ast::ident::{Ident, Path};

macro_rules! count {
    () => (0usize);
    ( $x:ident $($xs:ident)* ) => (1usize + count!($($xs)*));
}

macro_rules! built_in_types {
    ($($i:ident),+) => {
        #[repr(usize)]
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum BuiltInType {
            $($i),+
        }

        impl BuiltInType {
            pub const ALL: [Self; count!($($i)*)] = [$(Self::$i),*];

            pub fn ident(self) -> Ident {
                match self {
                    $(
                        Self::$i => Ident::from(stringify!($i))
                    ),+
                }
            }

            pub fn from_ident(ident: Ident) -> Option<Self> {
                match ident.as_ref() {
                    $(
                        stringify!($i) => Some(Self::$i)
                    ),+,
                    _ => None
                }
            }
        }
    }
}

built_in_types! {
    Unit,
    Bool,
    Int32,
    Int64,
    UInt16,
    Double
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

macro_rules! built_in_vars{
    ($($i:ident = $l:literal),+) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        pub enum BuiltInVar {
            $($i),*
        }

        impl BuiltInVar {
            pub const ALL: [Self; count!($($i)*)] = [$(Self::$i),*];

            pub fn ident(self) -> Ident {
                match self {
                    $(
                        BuiltInVar::$i => Ident::from($l)
                    ),+
                }
            }

            pub fn from_ident(ident: Ident) -> Option<BuiltInVar> {
                match ident.as_ref() {
                    $(
                        $l => Some(BuiltInVar::$i)
                    ),+,
                    _ => None
                }
            }
        }
    }
}

built_in_vars! {
    Unit = "unit",
    True = "true",
    False = "false"
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
