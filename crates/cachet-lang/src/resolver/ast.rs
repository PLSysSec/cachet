// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into};
use lazy_static::lazy_static;
use typed_index_collections::TiVec;

use crate::ast::{Ident, Path, Spanned};
pub use crate::parser::{
    BlockExprKind, CheckStmtKind, CompareExprKind, EnumItem, NegateExprKind, VariantIndex,
    NUM_BUILT_IN_TYPES, NUM_BUILT_IN_VARS,
};
use crate::parser::{BUILT_IN_TYPE_IDENTS, BUILT_IN_VAR_IDENTS};
use crate::util::{box_from, deref_from, deref_index, typed_index};

// TODO(spinda): Implement TryFrom<Ident> and TryFrom<Path>.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInType {
    Unit,
    Bool,
    Int32,
    Double,
}

pub const BUILT_IN_TYPES: [BuiltInType; NUM_BUILT_IN_TYPES] = [
    BuiltInType::Unit,
    BuiltInType::Bool,
    BuiltInType::Int32,
    BuiltInType::Double,
];

// TODO(spinda): It would be awesome if we could replace this with something
// like `TiArray<BuiltInType, NUM_BUILT_IN_TYPES>`.
pub type BuiltInTypeMap<T> = [T; NUM_BUILT_IN_TYPES];

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
    pub fn_items: TiVec<FnIndex, FnItem>,
    pub op_items: TiVec<OpIndex, OpItem>,
}

typed_index!(Env:enum_items[EnumIndex] => EnumItem);
typed_index!(Env:struct_items[StructIndex] => StructItem);
typed_index!(Env:ir_items[IrIndex] => IrItem);
typed_index!(Env:global_var_items[GlobalVarIndex] => GlobalVarItem);
typed_index!(Env:fn_items[FnIndex] => FnItem);
typed_index!(Env:op_items[OpIndex] => OpItem);

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
    type Output = FnItem;

    fn index(&self, index: CallableIndex) -> &Self::Output {
        match index {
            CallableIndex::Fn(fn_index) => &self.fn_items[fn_index],
            CallableIndex::Op(op_index) => self.op_items[op_index].as_ref(),
        }
    }
}

impl IndexMut<CallableIndex> for Env {
    fn index_mut(&mut self, index: CallableIndex) -> &mut Self::Output {
        match index {
            CallableIndex::Fn(fn_index) => &mut self.fn_items[fn_index],
            CallableIndex::Op(op_index) => self.op_items[op_index].as_mut(),
        }
    }
}

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

#[derive(Clone, Debug)]
pub struct FnItem {
    pub path: Spanned<Path>,
    pub parent: Option<ParentIndex>,
    pub is_unsafe: bool,
    pub params: Params,
    pub param_order: Vec<ParamIndex>,
    pub ret: Option<Spanned<TypeIndex>>,
    pub body: Spanned<Option<Body>>,
}

// TODO(spinda): Expand derives.
#[derive(AsMut, AsRef, Clone, Debug, Deref, DerefMut, From, Into)]
pub struct OpItem(pub FnItem);

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

#[derive(Clone, Debug)]
pub struct OutVarParam {
    pub ident: Spanned<Ident>,
    pub type_: TypeIndex,
}

#[derive(Clone, Debug)]
pub struct Body {
    pub local_vars: TiVec<LocalVarIndex, LocalVar>,
    pub local_labels: TiVec<LocalLabelIndex, Spanned<Ident>>,
    pub block: Block,
}

typed_index!(Body:local_vars[LocalVarIndex] => LocalVar);
typed_index!(Body:local_labels[LocalLabelIndex] => Spanned<Ident>);

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
    Emit(EmitStmt),
    #[from(types(Block))]
    Expr(Expr),
}

deref_from!(&GotoStmt => Stmt);

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: LocalVarIndex,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Spanned<Expr>,
    pub then: Block,
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckStmtKind,
    pub cond: Spanned<Expr>,
}

#[derive(Clone, Copy, Debug)]
pub struct GotoStmt {
    pub label: LabelIndex,
}

#[derive(Clone, Debug)]
pub struct EmitStmt {
    pub target: Spanned<OpIndex>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    Expr(Expr),
    OutVar(OutVar),
    Label(LabelIndex),
}

deref_from!(&LabelIndex => Arg);

#[derive(Clone, Copy, Debug)]
pub enum OutVar {
    Out(Spanned<VarIndex>),
    OutLet(LocalVarIndex),
}

#[derive(Clone, Debug, From)]
pub enum Expr {
    #[from]
    Block(Box<BlockExpr>),
    #[from]
    Var(Spanned<VarIndex>),
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

box_from!(BlockExpr => Expr);
box_from!(NegateExpr => Expr);
box_from!(CastExpr => Expr);
box_from!(CompareExpr => Expr);
box_from!(AssignExpr => Expr);

deref_from!(&Spanned<VarIndex> => Expr);

impl From<Spanned<&VarIndex>> for Expr {
    fn from(var_index: Spanned<&VarIndex>) -> Self {
        var_index.copied().into()
    }
}

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

impl From<Block> for BlockExpr {
    fn from(block: Block) -> BlockExpr {
        BlockExpr { kind: None, block }
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: Spanned<FnIndex>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
}

#[derive(Clone, Debug)]
pub struct NegateExpr {
    pub kind: Spanned<NegateExprKind>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<TypeIndex>,
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: Spanned<CompareExprKind>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Spanned<VarIndex>,
    pub rhs: Spanned<Expr>,
}
