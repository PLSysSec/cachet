// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use lazy_static::lazy_static;

use crate::ast::ident::{Ident, Path};

pub const NUM_BUILT_IN_TYPES: usize = 4;
pub const NUM_BUILT_IN_VARS: usize = 3;

// TODO(spinda): It would be awesome if we could replace this with something
// like `TiArray<BuiltInType, NUM_BUILT_IN_TYPES>`.
pub type BuiltInTypeMap<T> = [T; NUM_BUILT_IN_TYPES];
pub type BuiltInVarMap<T> = [T; NUM_BUILT_IN_VARS];

lazy_static! {
    pub static ref UNIT_TYPE_IDENT: Ident = Ident::from("Unit");
    pub static ref BOOL_TYPE_IDENT: Ident = Ident::from("Bool");
    pub static ref INT32_TYPE_IDENT: Ident = Ident::from("Int32");
    pub static ref DOUBLE_TYPE_IDENT: Ident = Ident::from("Double");
    pub static ref BUILT_IN_TYPE_IDENTS: BuiltInTypeMap<Ident> = [
        *UNIT_TYPE_IDENT,
        *BOOL_TYPE_IDENT,
        *INT32_TYPE_IDENT,
        *DOUBLE_TYPE_IDENT,
    ];
}

lazy_static! {
    pub static ref UNIT_TYPE_PATH: Path = (*UNIT_TYPE_IDENT).into();
    pub static ref BOOL_TYPE_PATH: Path = (*BOOL_TYPE_IDENT).into();
    pub static ref INT32_TYPE_PATH: Path = (*INT32_TYPE_IDENT).into();
    pub static ref DOUBLE_TYPE_PATH: Path = (*DOUBLE_TYPE_IDENT).into();
    pub static ref BUILT_IN_TYPE_PATHS: BuiltInTypeMap<Path> = [
        *UNIT_TYPE_PATH,
        *BOOL_TYPE_PATH,
        *INT32_TYPE_PATH,
        *DOUBLE_TYPE_PATH,
    ];
}

lazy_static! {
    pub static ref UNIT_VAR_IDENT: Ident = Ident::from("unit");
    pub static ref TRUE_VAR_IDENT: Ident = Ident::from("true");
    pub static ref FALSE_VAR_IDENT: Ident = Ident::from("false");
    pub static ref BUILT_IN_VAR_IDENTS: BuiltInVarMap<Ident> =
        [*UNIT_VAR_IDENT, *TRUE_VAR_IDENT, *FALSE_VAR_IDENT,];
}

lazy_static! {
    pub static ref UNIT_VAR_PATH: Path = (*UNIT_VAR_IDENT).into();
    pub static ref TRUE_VAR_PATH: Path = (*TRUE_VAR_IDENT).into();
    pub static ref FALSE_VAR_PATH: Path = (*FALSE_VAR_IDENT).into();
    pub static ref BUILT_IN_VAR_PATHS: BuiltInVarMap<Path> =
        [*UNIT_VAR_PATH, *TRUE_VAR_PATH, *FALSE_VAR_PATH,];
}

// TODO(spinda): Implement TryFrom<Ident> and TryFrom<Path>.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInType {
    Unit,
    Bool,
    Int32,
    Double,
}

pub const BUILT_IN_TYPES: BuiltInTypeMap<BuiltInType> = [
    BuiltInType::Unit,
    BuiltInType::Bool,
    BuiltInType::Int32,
    BuiltInType::Double,
];

lazy_static! {
    static ref IDENT_TO_BUILT_IN_TYPE: HashMap<Ident, BuiltInType> = {
        let mut ident_to_built_in_type = HashMap::with_capacity(NUM_BUILT_IN_TYPES);
        for (built_in_type_ident, built_in_type) in
            BUILT_IN_TYPE_IDENTS.iter().zip(BUILT_IN_TYPES.iter())
        {
            ident_to_built_in_type.insert(*built_in_type_ident, *built_in_type);
        }
        ident_to_built_in_type
    };
}

impl BuiltInType {
    pub const fn index(self) -> usize {
        match self {
            BuiltInType::Unit => 0,
            BuiltInType::Bool => 1,
            BuiltInType::Int32 => 2,
            BuiltInType::Double => 3,
        }
    }

    pub fn ident(self) -> Ident {
        BUILT_IN_TYPE_IDENTS[self]
    }

    pub fn path(self) -> Path {
        BUILT_IN_TYPE_PATHS[self]
    }

    pub fn from_ident(ident: Ident) -> Option<BuiltInType> {
        IDENT_TO_BUILT_IN_TYPE.get(&ident).copied()
    }

    pub fn from_path(path: Path) -> Option<BuiltInType> {
        if path.has_parent() {
            None
        } else {
            BuiltInType::from_ident(path.ident())
        }
    }

    pub const fn supertype(self) -> Option<BuiltInType> {
        match self {
            BuiltInType::Bool => Some(BuiltInType::Int32),
            BuiltInType::Unit | BuiltInType::Int32 | BuiltInType::Double => None,
        }
    }

    pub const fn is_numeric(self) -> bool {
        match self {
            BuiltInType::Bool | BuiltInType::Int32 | BuiltInType::Double => true,
            BuiltInType::Unit => false,
        }
    }
}

impl<T> Index<BuiltInType> for BuiltInTypeMap<T> {
    type Output = T;

    fn index(&self, built_in_type: BuiltInType) -> &Self::Output {
        &self[built_in_type.index()]
    }
}

impl<T> IndexMut<BuiltInType> for BuiltInTypeMap<T> {
    fn index_mut(&mut self, built_in_type: BuiltInType) -> &mut Self::Output {
        &mut self[built_in_type.index()]
    }
}

impl<T> Index<&'_ BuiltInType> for BuiltInTypeMap<T> {
    type Output = T;

    fn index(&self, built_in_type: &BuiltInType) -> &Self::Output {
        &self[built_in_type.index()]
    }
}

impl<T> IndexMut<&'_ BuiltInType> for BuiltInTypeMap<T> {
    fn index_mut(&mut self, built_in_type: &BuiltInType) -> &mut Self::Output {
        &mut self[built_in_type.index()]
    }
}

impl From<BuiltInType> for Ident {
    fn from(built_in_type: BuiltInType) -> Self {
        built_in_type.ident()
    }
}

impl From<BuiltInType> for Path {
    fn from(built_in_type: BuiltInType) -> Self {
        built_in_type.path()
    }
}

// TODO(spinda): Implement TryFrom<Ident> and TryFrom<Path>.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInVar {
    Unit,
    True,
    False,
}

pub const BUILT_IN_VARS: BuiltInVarMap<BuiltInVar> =
    [BuiltInVar::Unit, BuiltInVar::True, BuiltInVar::False];

lazy_static! {
    static ref IDENT_TO_BUILT_IN_VAR: HashMap<Ident, BuiltInVar> = {
        let mut ident_to_built_in_var = HashMap::with_capacity(NUM_BUILT_IN_VARS);
        for (built_in_var_ident, built_in_var) in
            BUILT_IN_VAR_IDENTS.iter().zip(BUILT_IN_VARS.iter())
        {
            ident_to_built_in_var.insert(*built_in_var_ident, *built_in_var);
        }
        ident_to_built_in_var
    };
}

impl BuiltInVar {
    pub const fn index(self) -> usize {
        match self {
            BuiltInVar::Unit => 0,
            BuiltInVar::True => 1,
            BuiltInVar::False => 2,
        }
    }

    pub fn ident(self) -> Ident {
        BUILT_IN_VAR_IDENTS[self]
    }

    pub fn path(self) -> Path {
        BUILT_IN_VAR_PATHS[self]
    }

    pub fn from_ident(ident: Ident) -> Option<BuiltInVar> {
        IDENT_TO_BUILT_IN_VAR.get(&ident).copied()
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

impl<T> Index<BuiltInVar> for BuiltInVarMap<T> {
    type Output = T;

    fn index(&self, built_in_var: BuiltInVar) -> &Self::Output {
        &self[built_in_var.index()]
    }
}

impl<T> IndexMut<BuiltInVar> for BuiltInVarMap<T> {
    fn index_mut(&mut self, built_in_var: BuiltInVar) -> &mut Self::Output {
        &mut self[built_in_var.index()]
    }
}

impl<T> Index<&'_ BuiltInVar> for BuiltInVarMap<T> {
    type Output = T;

    fn index(&self, built_in_var: &BuiltInVar) -> &Self::Output {
        &self[built_in_var.index()]
    }
}

impl<T> IndexMut<&'_ BuiltInVar> for BuiltInVarMap<T> {
    fn index_mut(&mut self, built_in_var: &BuiltInVar) -> &mut Self::Output {
        &mut self[built_in_var.index()]
    }
}

impl From<BuiltInVar> for Ident {
    fn from(built_in_var: BuiltInVar) -> Self {
        built_in_var.ident()
    }
}

impl From<BuiltInVar> for Path {
    fn from(built_in_var: BuiltInVar) -> Self {
        built_in_var.path()
    }
}
