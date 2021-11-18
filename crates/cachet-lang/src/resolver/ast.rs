// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use derive_more::From;
use lazy_static::lazy_static;
use typed_index_collections::TiVec;

pub use crate::ast::*;
use crate::parser::BUILT_IN_VAR_IDENTS;
pub use crate::parser::{
    BlockExprKind, CheckStmtKind, CompareExprKind, EnumItem, NegateExprKind, VariantIndex,
    NUM_BUILT_IN_VARS,
};
use crate::util::{deref_from, deref_index, field_index, typed_field_index};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ResolverPhase;

impl Phase for ResolverPhase {
    type TypeKey = TypeIndex;
    type VarKey = VarIndex;
    type LabelKey = LabelIndex;
    type IrKey = IrIndex;
    type ParentKey<C: ParentConfig> = ParentIndex<C>;
    type FnKey = FnIndex;
    type OpKey = OpIndex;

    type CallableItemName = Path;
    type CallableItemCanHaveParent = Yes;
    type BodyHasLocals = Always;
    type LocalVarHasType = Sometimes;
    type LocalVarTypeHasSpan = Always;
    type LetStmtLhs = LocalVarIndex;
    type ExprHasType = Never;
}

export_phase_type_aliases!(Resolver);

// TODO(spinda): Implement TryFrom<Ident> and TryFrom<Path>.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInVar {
    Unit,
    True,
    False,
}

pub const BUILT_IN_VARS: [BuiltInVar; NUM_BUILT_IN_VARS] =
    [BuiltInVar::Unit, BuiltInVar::True, BuiltInVar::False];

// TODO(spinda): It would be awesome if we could replace this with something
// like `TiArray<BuiltInVar, NUM_BUILT_IN_VARS>`.
pub type BuiltInVarMap<T> = [T; NUM_BUILT_IN_VARS];

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

#[derive(Clone, Debug)]
pub struct Env {
    pub enum_items: TiVec<EnumIndex, EnumItem>,
    pub struct_items: TiVec<StructIndex, StructItem>,
    pub ir_items: TiVec<IrIndex, IrItem>,
    pub global_var_items: TiVec<GlobalVarIndex, GlobalVarItem>,
    pub fn_items: TiVec<FnIndex, ResolverFnItem>,
    pub op_items: TiVec<OpIndex, ResolverOpItem>,
}

field_index!(Env:enum_items[EnumIndex] => EnumItem);
field_index!(Env:struct_items[StructIndex] => StructItem);
field_index!(Env:ir_items[IrIndex] => IrItem);
typed_field_index!(Env:global_var_items[pub GlobalVarIndex] => GlobalVarItem);
typed_field_index!(Env:fn_items[pub FnIndex] => ResolverFnItem);
typed_field_index!(Env:op_items[pub OpIndex] => ResolverOpItem);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct EnumVariantIndex {
    pub enum_index: EnumIndex,
    pub variant_index: VariantIndex,
}

impl Index<EnumVariantIndex> for Env {
    type Output = Spanned<Ident>;

    fn index(&self, index: EnumVariantIndex) -> &Self::Output {
        &self.enum_items[index.enum_index][index.variant_index]
    }
}

impl IndexMut<EnumVariantIndex> for Env {
    fn index_mut(&mut self, index: EnumVariantIndex) -> &mut Self::Output {
        &mut self.enum_items[index.enum_index][index.variant_index]
    }
}

deref_index!(Env[&EnumVariantIndex] => Spanned<Ident>);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum VarIndex {
    BuiltIn(BuiltInVar),
    EnumVariant(EnumVariantIndex),
    Global(GlobalVarIndex),
    Param(VarParamIndex),
    OutParam(OutVarParamIndex),
    Local(LocalVarIndex),
}

deref_from!(&BuiltInVar => VarIndex);
deref_from!(&EnumVariantIndex => VarIndex);
deref_from!(&GlobalVarIndex => VarIndex);
deref_from!(&VarParamIndex => VarIndex);
deref_from!(&OutVarParamIndex => VarIndex);
deref_from!(&LocalVarIndex => VarIndex);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum CallableIndex {
    Fn(FnIndex),
    Op(OpIndex),
}

deref_from!(&FnIndex => CallableIndex);
deref_from!(&OpIndex => CallableIndex);

#[derive(Clone, Debug)]
pub struct StructItem {
    pub ident: Spanned<Ident>,
    pub supertype: Option<TypeIndex>,
}

#[derive(Clone, Debug)]
pub struct IrItem {
    pub ident: Spanned<Ident>,
    pub emits: Option<IrIndex>,
}

#[derive(Clone, Debug)]
pub struct GlobalVarItem {
    pub path: Spanned<Path>,
    pub parent: Option<AnyParentIndex>,
    pub is_mut: bool,
    pub type_: TypeIndex,
}
