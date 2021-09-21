// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use derive_more::From;
use thiserror::Error;
use typed_index_collections::TiVec;

use crate::ast::{Ident, Path, Spanned};
use crate::resolver;
pub use crate::resolver::{
    BlockExprKind, BuiltInType, BuiltInTypeMap, BuiltInVar, BuiltInVarMap, CallableIndex,
    CheckStmtKind, CompareExprKind, EnumIndex, EnumItem, EnumVariantIndex, FnIndex,
    GlobalVarIndex, GlobalVarItem, GotoStmt, IrIndex, IrItem, LabelIndex, LabelParamIndex,
    LocalLabelIndex, LocalVarIndex, NegateExprKind, OpIndex, OutVar, OutVarParam,
    OutVarParamIndex, ParentIndex, StructIndex, StructItem, TypeIndex,
    VarIndex, VarParam, VarParamIndex, VariantIndex, BUILT_IN_TYPES, BUILT_IN_VARS,
    NUM_BUILT_IN_TYPES, NUM_BUILT_IN_VARS,
};
use crate::util::{box_from, deref_from, deref_index, field_index};

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

impl Typed for EnumVariantIndex {
    fn type_(&self) -> TypeIndex {
        TypeIndex::Enum(self.enum_index)
    }
}

impl Typed for GlobalVarItem {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl Typed for resolver::FnItem {
    fn type_(&self) -> TypeIndex {
        match self.ret {
            Some(ret) => ret.value,
            None => BuiltInType::Unit.into(),
        }
    }
}

impl Typed for VarParam {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl Typed for OutVarParam {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug)]
pub struct Env {
    pub enum_items: TiVec<EnumIndex, EnumItem>,
    pub struct_items: TiVec<StructIndex, StructItem>,
    pub ir_items: TiVec<IrIndex, IrItem>,
    pub global_var_items: TiVec<GlobalVarIndex, GlobalVarItem>,
    pub fn_items: TiVec<FnIndex, FnItem>,
    pub op_items: TiVec<OpIndex, OpItem>,
    pub decl_order: Vec<DeclIndex>,
}

field_index!(Env:enum_items[EnumIndex] => EnumItem);
field_index!(Env:struct_items[StructIndex] => StructItem);
field_index!(Env:ir_items[IrIndex] => IrItem);
field_index!(Env:global_var_items[GlobalVarIndex] => GlobalVarItem);
field_index!(Env:fn_items[FnIndex] => FnItem);
field_index!(Env:op_items[OpIndex] => OpItem);

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
pub enum DeclIndex {
    Enum(EnumIndex),
    Struct(StructIndex),
    GlobalVar(GlobalVarIndex),
    Fn(FnIndex),
    Op(OpIndex),
}

deref_from!(&EnumIndex => DeclIndex);
deref_from!(&StructIndex => DeclIndex);
deref_from!(&OpIndex => DeclIndex);
deref_from!(&FnIndex => DeclIndex);

impl TryFrom<TypeIndex> for DeclIndex {
    type Error = NotPartOfDeclOrderError;

    fn try_from(type_index: TypeIndex) -> Result<Self, Self::Error> {
        match type_index {
            TypeIndex::BuiltIn(_) => Err(NotPartOfDeclOrderError),
            TypeIndex::Enum(enum_index) => Ok(enum_index.into()),
            TypeIndex::Struct(struct_index) => Ok(struct_index.into()),
        }
    }
}

impl TryFrom<&TypeIndex> for DeclIndex {
    type Error = NotPartOfDeclOrderError;

    fn try_from(src: &TypeIndex) -> Result<Self, Self::Error> {
        TryFrom::try_from(*src)
    }
}

impl TryFrom<VarIndex> for DeclIndex {
    type Error = NotPartOfDeclOrderError;

    fn try_from(var_index: VarIndex) -> Result<Self, Self::Error> {
        match var_index {
            VarIndex::BuiltIn(_)
            | VarIndex::EnumVariant(_)
            | VarIndex::Param(_)
            | VarIndex::OutParam(_)
            | VarIndex::Local(_) => Err(NotPartOfDeclOrderError),
            VarIndex::Global(global_var_index) => Ok(global_var_index.into()),
        }
    }
}

impl TryFrom<&VarIndex> for DeclIndex {
    type Error = NotPartOfDeclOrderError;

    fn try_from(src: &VarIndex) -> Result<Self, Self::Error> {
        TryFrom::try_from(*src)
    }
}

impl From<CallableIndex> for DeclIndex {
    fn from(callable_index: CallableIndex) -> DeclIndex {
        match callable_index {
            CallableIndex::Op(op_index) => DeclIndex::Op(op_index),
            CallableIndex::Fn(fn_index) => DeclIndex::Fn(fn_index),
        }
    }
}

deref_from!(&CallableIndex => DeclIndex);

#[derive(Clone, Copy, Debug, Default, Error)]
#[error("item is not part of the declaration order")]
pub struct NotPartOfDeclOrderError;

#[derive(Clone, Debug)]
pub struct FnItem {
    pub path: Spanned<Path>,
    pub parent: Option<ParentIndex>,
    pub is_unsafe: bool,
    pub params: FnParams,
    pub param_order: Vec<FnParamIndex>,
    pub ret: TypeIndex,
    pub body: Option<Body>,
}

impl Typed for FnItem {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

#[derive(Clone, Debug)]
pub struct OpItem {
    pub path: Spanned<Path>,
    pub parent: IrIndex,
    pub is_unsafe: bool,
    pub params: Params,
    pub param_order: Vec<ParamIndex>,
    pub body: Body,
}

#[derive(Clone, Debug, Default)]
pub struct FnParams {
    pub params: Params,
    pub out_var_params: TiVec<OutVarParamIndex, OutVarParam>,
}

field_index!(FnParams:params[VarParamIndex] => VarParam);
field_index!(FnParams:params[LabelParamIndex] => Spanned<Ident>);
field_index!(FnParams:out_var_params[OutVarParamIndex] => OutVarParam);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum FnParamIndex {
    #[from(types(VarParamIndex, "&VarParamIndex", LabelParamIndex, "&LabelParamIndex"))]
    Param(ParamIndex),
    #[from]
    OutVar(OutVarParamIndex),
}

deref_from!(&ParamIndex => FnParamIndex);
deref_from!(&OutVarParamIndex => FnParamIndex);

#[derive(Clone, Debug, Default)]
pub struct Params {
    pub var_params: TiVec<VarParamIndex, VarParam>,
    pub label_params: TiVec<LabelParamIndex, Spanned<Ident>>,
}

field_index!(Params:var_params[VarParamIndex] => VarParam);
field_index!(Params:label_params[LabelParamIndex] => Spanned<Ident>);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum ParamIndex {
    Var(VarParamIndex),
    Label(LabelParamIndex),
}

deref_from!(&VarParamIndex => ParamIndex);
deref_from!(&LabelParamIndex => ParamIndex);

#[derive(Clone, Debug)]
pub struct Body {
    pub local_vars: TiVec<LocalVarIndex, LocalVar>,
    pub local_labels: TiVec<LocalLabelIndex, Spanned<Ident>>,
    pub block: Block,
}

impl Typed for Body {
    fn type_(&self) -> TypeIndex {
        self.block.value.type_()
    }
}

field_index!(Body:local_vars[LocalVarIndex] => LocalVar);
field_index!(Body:local_labels[LocalLabelIndex] => Spanned<Ident>);

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: TypeIndex,
}

impl Typed for LocalVar {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub value: Expr,
}

impl Typed for Block {
    fn type_(&self) -> TypeIndex {
        self.value.type_()
    }
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
    Emit(EmitStmt),
    #[from(types(Block))]
    Expr(Expr),
}

deref_from!(&GotoStmt => Stmt);

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: LocalVarIndex,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Block,
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckStmtKind,
    pub cond: Expr,
}

#[derive(Clone, Debug)]
pub struct EmitStmt {
    pub target: OpIndex,
    pub is_unsafe: bool,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug, From)]
pub enum FnArg {
    #[from(types(Expr, LabelIndex, "&LabelIndex"))]
    Arg(Arg),
    #[from]
    OutVar(OutVarArg),
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    Expr(Expr),
    Label(LabelIndex),
}

deref_from!(&LabelIndex => Arg);

#[derive(Clone, Debug)]
pub struct OutVarArg {
    pub out_var: OutVar,
    pub type_: TypeIndex,
    pub upcast_route: Vec<TypeIndex>,
}

impl Typed for OutVarArg {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug, From)]
pub enum Expr {
    #[from]
    Block(Box<BlockExpr>),
    #[from(types(BuiltInVar, "&BuiltInVar"))]
    Var(VarExpr),
    #[from]
    Call(CallExpr),
    #[from]
    Negate(Box<NegateExpr>),
    #[from]
    Cast(Box<CastExpr>),
    #[from]
    Compare(Box<CompareExpr>),
    #[from]
    Assign(Box<AssignExpr>),
}

impl Typed for Expr {
    fn type_(&self) -> TypeIndex {
        match self {
            Expr::Block(block_expr) => block_expr.type_(),
            Expr::Var(var_expr) => var_expr.type_(),
            Expr::Call(call_expr) => call_expr.type_(),
            Expr::Negate(negate_expr) => negate_expr.type_(),
            Expr::Cast(cast_expr) => cast_expr.type_(),
            Expr::Compare(compare_expr) => compare_expr.type_(),
            Expr::Assign(assign_expr) => assign_expr.type_(),
        }
    }
}

box_from!(BlockExpr => Expr);
box_from!(NegateExpr => Expr);
box_from!(CastExpr => Expr);
box_from!(CompareExpr => Expr);
box_from!(AssignExpr => Expr);

impl From<Block> for Expr {
    fn from(block: Block) -> Self {
        BlockExpr::from(block).into()
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub kind: Option<BlockExprKind>,
    pub block: Block,
}

impl Typed for BlockExpr {
    fn type_(&self) -> TypeIndex {
        self.block.type_()
    }
}

impl From<Block> for BlockExpr {
    fn from(block: Block) -> BlockExpr {
        BlockExpr { kind: None, block }
    }
}

#[derive(Clone, Debug)]
pub struct VarExpr {
    pub var: VarIndex,
    pub type_: TypeIndex,
}

impl Typed for VarExpr {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl From<BuiltInVar> for VarExpr {
    fn from(built_in_var: BuiltInVar) -> Self {
        VarExpr {
            var: built_in_var.into(),
            type_: built_in_var.type_().into(),
        }
    }
}

deref_from!(&BuiltInVar => VarExpr);

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: FnIndex,
    pub is_unsafe: bool,
    pub args: Vec<FnArg>,
    pub ret: TypeIndex,
}

impl Typed for CallExpr {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

#[derive(Clone, Debug)]
pub struct NegateExpr {
    pub kind: NegateExprKind,
    pub expr: Expr,
}

impl Typed for NegateExpr {
    fn type_(&self) -> TypeIndex {
        self.expr.type_()
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub kind: CastExprKind,
    pub expr: Expr,
    pub type_: TypeIndex,
}

impl Typed for CastExpr {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CastExprKind {
    Downcast,
    Upcast,
}

impl CastExprKind {
    pub const fn is_unsafe(self) -> bool {
        match self {
            CastExprKind::Downcast => true,
            CastExprKind::Upcast => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: CompareExprKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl Typed for CompareExpr {
    fn type_(&self) -> TypeIndex {
        BuiltInType::Bool.into()
    }
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Spanned<VarIndex>,
    pub rhs: Expr,
}

impl Typed for AssignExpr {
    fn type_(&self) -> TypeIndex {
        BuiltInType::Unit.into()
    }
}
