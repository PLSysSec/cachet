// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use derive_more::From;
use thiserror::Error;
use typed_index_collections::TiVec;

use cachet_util::{box_from, deref_from, deref_index, field_index};

use crate::ast::{
    BlockKind, BuiltInType, BuiltInVar, CastKind, CheckKind, CompareKind, NegateKind, Path,
    Spanned,
};
use crate::resolver;
pub use crate::resolver::{
    CallableIndex, EnumIndex, EnumItem, EnumVariantIndex, FnIndex, GlobalVarIndex, GlobalVarItem,
    IrIndex, IrItem, Label, LabelIndex, LabelParamIndex, LabelStmt, Literal, LocalLabelIndex,
    LocalVarIndex, OpIndex, OutVar, OutVarParam, OutVarParamIndex, ParamIndex, Params,
    ParentIndex, StructIndex, StructItem, TypeIndex, Typed, VarIndex, VarParam, VarParamIndex,
    VariantIndex,
};

#[derive(Clone, Debug)]
pub struct Env {
    pub enum_items: TiVec<EnumIndex, EnumItem>,
    pub struct_items: TiVec<StructIndex, StructItem>,
    pub ir_items: TiVec<IrIndex, IrItem>,
    pub global_var_items: TiVec<GlobalVarIndex, GlobalVarItem>,
    pub fn_items: TiVec<FnIndex, CallableItem>,
    pub op_items: TiVec<OpIndex, CallableItem>,
    pub decl_order: Vec<DeclIndex>,
}

field_index!(Env:enum_items[EnumIndex] => EnumItem);
field_index!(Env:struct_items[StructIndex] => StructItem);
field_index!(Env:ir_items[IrIndex] => IrItem);
field_index!(Env:global_var_items[GlobalVarIndex] => GlobalVarItem);
field_index!(Env:fn_items[FnIndex] => CallableItem);
field_index!(Env:op_items[OpIndex] => CallableItem);

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

deref_index!(Env[&CallableIndex] => CallableItem);

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
pub struct CallableItem {
    pub path: Spanned<Path>,
    pub parent: Option<ParentIndex>,
    pub is_unsafe: bool,
    pub params: Params,
    pub param_order: Vec<ParamIndex>,
    pub ret: TypeIndex,
    pub interprets: Option<IrIndex>,
    pub emits: Option<IrIndex>,
    pub body: Option<Body>,
}

impl Typed for CallableItem {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    Expr(Expr),
    OutVar(OutVarArg),
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

#[derive(Clone, Debug)]
pub struct Call {
    pub target: CallableIndex,
    pub is_unsafe: bool,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug)]
pub struct Body {
    pub locals: Locals,
    pub block: Block,
}

impl Typed for Body {
    fn type_(&self) -> TypeIndex {
        self.block.value.type_()
    }
}

field_index!(Body:locals[LocalVarIndex] => LocalVar);
field_index!(Body:locals[LocalLabelIndex] => Label);

#[derive(Clone, Debug, Default)]
pub struct Locals {
    pub local_vars: TiVec<LocalVarIndex, LocalVar>,
    pub local_labels: TiVec<LocalLabelIndex, Label>,
}

field_index!(Locals:local_vars[LocalVarIndex] => LocalVar);
field_index!(Locals:local_labels[LocalLabelIndex] => Label);

pub type LocalVar = VarParam;

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
    Label(LabelStmt),
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

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: LocalVarIndex,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Block,
    pub else_: Option<Block>,
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckKind,
    pub cond: Expr,
}

#[derive(Clone, Debug)]
pub struct GotoStmt {
    pub label: LabelIndex,
    pub ir: IrIndex,
}

#[derive(Clone, Debug)]
pub struct EmitStmt {
    pub call: Call,
    pub ir: IrIndex,
}

#[derive(Clone, Debug, From)]
pub enum Expr {
    #[from]
    Block(Box<BlockExpr>),
    #[from]
    Literal(Literal),
    #[from(types(BuiltInVar, "&BuiltInVar"))]
    Var(VarExpr),
    #[from]
    Invoke(InvokeExpr),
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
            Expr::Literal(literal) => literal.type_(),
            Expr::Var(var_expr) => var_expr.type_(),
            Expr::Invoke(call_expr) => call_expr.type_(),
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

deref_from!(&Literal => Expr);

impl From<Block> for Expr {
    fn from(block: Block) -> Self {
        BlockExpr::from(block).into()
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub kind: Option<BlockKind>,
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
pub struct InvokeExpr {
    pub call: Call,
    pub ret: TypeIndex,
}

impl Typed for InvokeExpr {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

#[derive(Clone, Debug)]
pub struct NegateExpr {
    pub kind: NegateKind,
    pub expr: Expr,
}

impl Typed for NegateExpr {
    fn type_(&self) -> TypeIndex {
        self.expr.type_()
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub kind: CastKind,
    pub expr: Expr,
    pub type_: TypeIndex,
}

impl Typed for CastExpr {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: CompareKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl CompareExpr {
    pub const TYPE: BuiltInType = resolver::CompareExpr::TYPE;
}

impl Typed for CompareExpr {
    fn type_(&self) -> TypeIndex {
        CompareExpr::TYPE.into()
    }
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Spanned<VarIndex>,
    pub rhs: Expr,
}

impl AssignExpr {
    pub const TYPE: BuiltInType = resolver::AssignExpr::TYPE;
}

impl Typed for AssignExpr {
    fn type_(&self) -> TypeIndex {
        AssignExpr::TYPE.into()
    }
}
