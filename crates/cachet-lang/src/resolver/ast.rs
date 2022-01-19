// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use derive_more::From;
use typed_index_collections::TiVec;

use crate::ast::{
    BlockKind, BuiltInType, BuiltInVar, CheckKind, CompareKind, Ident, MaybeSpanned,
    NegateKind, Path, Spanned,
};
pub use crate::parser::VariantIndex;
use crate::util::{box_from, deref_from, deref_index, field_index, typed_index};

#[derive(Clone, Debug)]
pub struct Env {
    pub enum_items: TiVec<EnumIndex, EnumItem>,
    pub struct_items: TiVec<StructIndex, StructItem>,
    pub ir_items: TiVec<IrIndex, IrItem>,
    pub global_var_items: TiVec<GlobalVarIndex, GlobalVarItem>,
    pub fn_items: TiVec<FnIndex, CallableItem>,
    pub op_items: TiVec<OpIndex, CallableItem>,
}

typed_index!(Env:enum_items[EnumIndex] => EnumItem);
typed_index!(Env:struct_items[StructIndex] => StructItem);
typed_index!(Env:ir_items[IrIndex] => IrItem);
typed_index!(Env:global_var_items[GlobalVarIndex] => GlobalVarItem);
typed_index!(Env:fn_items[FnIndex] => CallableItem);
typed_index!(Env:op_items[OpIndex] => CallableItem);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum TypeIndex {
    BuiltIn(BuiltInType),
    Enum(EnumIndex),
    Struct(StructIndex),
}

impl TypeIndex {
    pub fn is_numeric(&self) -> bool {
        match self {
            TypeIndex::BuiltIn(built_in_type) => built_in_type.is_numeric(),
            TypeIndex::Enum(_) | TypeIndex::Struct(_) => false,
        }
    }
}

deref_from!(&BuiltInType => TypeIndex);
deref_from!(&EnumIndex => TypeIndex);
deref_from!(&StructIndex => TypeIndex);

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

impl Index<CallableIndex> for Env {
    type Output = CallableItem;

    fn index(&self, index: CallableIndex) -> &Self::Output {
        match index {
            CallableIndex::Fn(fn_index) => &self.fn_items[fn_index],
            CallableIndex::Op(op_index) => &self.op_items[op_index],
        }
    }
}

impl IndexMut<CallableIndex> for Env {
    fn index_mut(&mut self, index: CallableIndex) -> &mut Self::Output {
        match index {
            CallableIndex::Fn(fn_index) => &mut self.fn_items[fn_index],
            CallableIndex::Op(op_index) => &mut self.op_items[op_index],
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum ParentIndex {
    #[from(types(
        BuiltInType,
        "&BuiltInType",
        EnumIndex,
        "&EnumIndex",
        StructIndex,
        "&StructIndex"
    ))]
    Type(TypeIndex),
    #[from]
    Ir(IrIndex),
}

deref_from!(&TypeIndex => ParentIndex);
deref_from!(&IrIndex => ParentIndex);

pub trait Typed {
    fn type_(&self) -> TypeIndex;
}

impl<T: Typed> Typed for Box<T> {
    fn type_(&self) -> TypeIndex {
        (**self).type_()
    }
}

impl<T: Typed> Typed for Spanned<T> {
    fn type_(&self) -> TypeIndex {
        self.value.type_()
    }
}

impl<T: Typed> Typed for MaybeSpanned<T> {
    fn type_(&self) -> TypeIndex {
        self.value.type_()
    }
}

impl Typed for BuiltInType {
    fn type_(&self) -> TypeIndex {
        self.into()
    }
}

impl Typed for BuiltInVar {
    fn type_(&self) -> TypeIndex {
        self.built_in_type().into()
    }
}

impl Typed for TypeIndex {
    fn type_(&self) -> TypeIndex {
        *self
    }
}

#[derive(Clone, Debug)]
pub struct EnumItem {
    pub ident: Spanned<Ident>,
    pub variants: TiVec<VariantIndex, Spanned<Path>>,
}

field_index!(EnumItem:variants[VariantIndex] => Spanned<Path>);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct EnumVariantIndex {
    pub enum_index: EnumIndex,
    pub variant_index: VariantIndex,
}

impl Typed for EnumVariantIndex {
    fn type_(&self) -> TypeIndex {
        TypeIndex::Enum(self.enum_index)
    }
}

impl Index<EnumVariantIndex> for Env {
    type Output = Spanned<Path>;

    fn index(&self, index: EnumVariantIndex) -> &Self::Output {
        &self.enum_items[index.enum_index][index.variant_index]
    }
}

impl IndexMut<EnumVariantIndex> for Env {
    fn index_mut(&mut self, index: EnumVariantIndex) -> &mut Self::Output {
        &mut self.enum_items[index.enum_index][index.variant_index]
    }
}

deref_index!(Env[&EnumVariantIndex] => Spanned<Path>);

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
    pub parent: Option<ParentIndex>,
    pub is_mut: bool,
    pub type_: TypeIndex,
}

impl Typed for GlobalVarItem {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug)]
pub struct CallableItem {
    pub path: Spanned<Path>,
    pub parent: Option<ParentIndex>,
    pub is_unsafe: bool,
    pub params: Params,
    pub param_order: Vec<ParamIndex>,
    pub ret: Option<Spanned<TypeIndex>>,
    pub body: Spanned<Option<Body>>,
}

impl Typed for CallableItem {
    fn type_(&self) -> TypeIndex {
        match self.ret {
            Some(ret) => ret.value,
            None => BuiltInType::Unit.into(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Params {
    pub var_params: TiVec<VarParamIndex, VarParam>,
    pub out_var_params: TiVec<OutVarParamIndex, OutVarParam>,
    pub label_params: TiVec<LabelParamIndex, Spanned<Ident>>,
}

typed_index!(Params:var_params[VarParamIndex] => VarParam);
typed_index!(Params:out_var_params[OutVarParamIndex] => OutVarParam);
typed_index!(Params:label_params[LabelParamIndex] => Spanned<Ident>);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum ParamIndex {
    Var(VarParamIndex),
    OutVar(OutVarParamIndex),
    Label(LabelParamIndex),
}

deref_from!(&VarParamIndex => ParamIndex);
deref_from!(&OutVarParamIndex => ParamIndex);
deref_from!(&LabelParamIndex => ParamIndex);

#[derive(Clone, Debug)]
pub struct VarParam {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: TypeIndex,
}

impl Typed for VarParam {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug)]
pub struct OutVarParam {
    pub ident: Spanned<Ident>,
    pub type_: TypeIndex,
}

impl Typed for OutVarParam {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    Expr(Expr),
    OutVar(OutVar),
    Label(LabelIndex),
}

deref_from!(&LabelIndex => Arg);

#[derive(Clone, Copy, Debug, From)]
pub enum OutVar {
    #[from]
    Out(Spanned<VarIndex>),
    OutLet(LocalVarIndex),
}

#[derive(Clone, Debug)]
pub struct Call {
    pub target: Spanned<CallableIndex>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
}

#[derive(Clone, Debug)]
pub struct Body {
    pub locals: Locals,
    pub block: Block,
}

field_index!(Body:locals[LocalVarIndex] => LocalVar);
field_index!(Body:locals[LocalLabelIndex] => Spanned<Ident>);

#[derive(Clone, Debug, Default)]
pub struct Locals {
    pub local_vars: TiVec<LocalVarIndex, LocalVar>,
    pub local_labels: TiVec<LocalLabelIndex, Spanned<Ident>>,
}

typed_index!(Locals:local_vars[LocalVarIndex] => LocalVar);
typed_index!(Locals:local_labels[LocalLabelIndex] => Spanned<Ident>);

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Option<TypeIndex>,
}

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum LabelIndex {
    Param(LabelParamIndex),
    Local(LocalLabelIndex),
}

deref_from!(&LabelParamIndex => LabelIndex);
deref_from!(&LocalLabelIndex => LabelIndex);

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, From)]
pub enum Stmt {
    #[from]
    Let(LetStmt),
    #[from]
    If(IfStmt),
    #[from]
    Check(CheckStmt),
    #[from]
    Goto(GotoStmt),
    #[from]
    Emit(Call),
    #[from(types(Block))]
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: LocalVarIndex,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Spanned<Expr>,
    pub then: Block,
    pub else_: Option<Block>,
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckKind,
    pub cond: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct GotoStmt {
    pub label: LabelIndex,
}

#[derive(Clone, Debug, From)]
pub enum Expr {
    #[from]
    Block(Box<BlockExpr>),
    #[from]
    Var(Spanned<VarIndex>),
    #[from]
    Call(Call),
    #[from]
    Negate(Box<NegateExpr>),
    #[from]
    Cast(Box<CastExpr>),
    #[from]
    Compare(Box<CompareExpr>),
    #[from]
    Assign(Box<AssignExpr>),
}

impl From<Block> for Expr {
    fn from(block: Block) -> Self {
        BlockExpr::from(block).into()
    }
}

impl From<Spanned<&VarIndex>> for Expr {
    fn from(var_index: Spanned<&VarIndex>) -> Self {
        var_index.copied().into()
    }
}

box_from!(BlockExpr => Expr);
box_from!(NegateExpr => Expr);
box_from!(CastExpr => Expr);
box_from!(CompareExpr => Expr);
box_from!(AssignExpr => Expr);

deref_from!(&Spanned<VarIndex> => Expr);

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub kind: Option<BlockKind>,
    pub block: Block,
}

impl From<Block> for BlockExpr {
    fn from(block: Block) -> BlockExpr {
        BlockExpr { kind: None, block }
    }
}

#[derive(Clone, Debug)]
pub struct NegateExpr {
    pub kind: Spanned<NegateKind>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<TypeIndex>,
}

impl Typed for CastExpr {
    fn type_(&self) -> TypeIndex {
        self.type_.value
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: Spanned<CompareKind>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
}

impl CompareExpr {
    pub const TYPE: BuiltInType = BuiltInType::Bool;
}

impl Typed for CompareExpr {
    fn type_(&self) -> TypeIndex {
        CompareExpr::TYPE.into()
    }
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Spanned<VarIndex>,
    pub rhs: Spanned<Expr>,
}

impl AssignExpr {
    pub const TYPE: BuiltInType = BuiltInType::Unit;
}

impl Typed for AssignExpr {
    fn type_(&self) -> TypeIndex {
        AssignExpr::TYPE.into()
    }
}