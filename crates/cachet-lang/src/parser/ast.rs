// vim: set tw=99 ts=4 sts=4 sw=4 et:

// TODO(spinda): Unify AST using trait associated items and void types.

use derive_more::From;
use lazy_static::lazy_static;
use typed_index_collections::TiVec;

pub use crate::ast::*;
use crate::util::typed_field_index;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ParserPhase;

impl Phase for ParserPhase {
    type TypeKey = Path;
    type VarKey = Path;
    type LabelKey = Path;
    type IrKey = Path;
    type ParentKey<C: ParentConfig> = Path;
    type FnKey = Path;
    type OpKey = Path;

    type CallableItemName = Ident;
    type CallableItemCanHaveParent = No;
    type BodyHasLocals = Never;
    type LocalVarHasType = Sometimes;
    type LocalVarTypeHasSpan = Always;
    type LetStmtLhs = ParserLocalVar;
    type ExprHasType = Never;
}

export_phase_type_aliases!(Parser);

pub const NUM_BUILT_IN_VARS: usize = 3;

lazy_static! {
    pub static ref UNIT_VAR_IDENT: Ident = Ident::from("unit");
    pub static ref TRUE_VAR_IDENT: Ident = Ident::from("true");
    pub static ref FALSE_VAR_IDENT: Ident = Ident::from("false");
    pub static ref BUILT_IN_VAR_IDENTS: [Ident; NUM_BUILT_IN_VARS] =
        [*UNIT_VAR_IDENT, *TRUE_VAR_IDENT, *FALSE_VAR_IDENT];
}

#[derive(Clone, Debug, From)]
pub enum Item {
    Enum(EnumItem),
    Struct(StructItem),
    Ir(IrItem),
    Impl(ImplItem),
    GlobalVar(GlobalVarItem),
    Fn(ParserFnItem),
    Op(ParserOpItem),
}

#[derive(Clone, Debug)]
pub struct EnumItem {
    pub ident: Spanned<Ident>,
    // TODO(spinda): Make this `variant_idents`, and `variant_paths` after name
    // resolution.
    pub variants: TiVec<VariantIndex, Spanned<Ident>>,
}

typed_field_index!(EnumItem:variants[pub VariantIndex] => Spanned<Ident>);

#[derive(Clone, Debug)]
pub struct StructItem {
    pub ident: Spanned<Ident>,
    pub supertype: Option<Spanned<Path>>,
}

#[derive(Clone, Debug)]
pub struct IrItem {
    pub ident: Spanned<Ident>,
    pub emits: Option<Spanned<Path>>,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Clone, Debug)]
pub struct ImplItem {
    pub parent: Spanned<Path>,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Clone, Debug)]
pub struct GlobalVarItem {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Spanned<Path>,
}
