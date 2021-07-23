// vim: set tw=99 ts=4 sts=4 sw=4 et:

pub use crate::frontend::resolver::{
    BlockExprKind, BuiltInType, BuiltInTypeMap, BuiltInVar, BuiltInVarMap, CheckStmtKind,
    CompareExprKind, ConstDef, ConstIndex, EnumDef, EnumIndex, EnumVariantIndex, FnIndex, Ident,
    LocalVarIndex, OpIndex, ParamVar, ParamVarIndex, ScopedVarIndex, Sig, Spanned, StructDef,
    StructIndex, TypeIndex, VarIndex, VariantIndex, BUILT_IN_TYPES, BUILT_IN_VARS,
    NUM_BUILT_IN_TYPES, NUM_BUILT_IN_VARS,
};

pub trait Typed {
    fn type_(&self) -> TypeIndex;
}

impl<T: Typed> Typed for Box<T> {
    fn type_(&self) -> TypeIndex {
        (**self).type_()
    }
}

impl Typed for EnumVariantIndex {
    fn type_(&self) -> TypeIndex {
        TypeIndex::Enum(self.enum_index)
    }
}

impl Typed for BuiltInVar {
    fn type_(&self) -> TypeIndex {
        self.built_in_type().into()
    }
}

#[derive(Clone, Debug)]
pub struct Env {
    pub enum_defs: Vec<EnumDef>,
    pub struct_defs: Vec<StructDef>,
    pub type_def_order: Vec<TypeIndex>,
    pub const_defs: Vec<ConstDef>,
    pub fn_defs: Vec<FnDef>,
    pub fn_def_order: Vec<FnIndex>,
    pub op_defs: Vec<OpDef>,
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub sig: Sig,
    pub parent_type: Option<TypeIndex>,
    pub is_unsafe: bool,
    pub body: Option<Body>,
}

#[derive(Clone, Debug)]
pub struct OpDef {
    pub sig: Sig,
    pub body: Body,
}

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Ident,
    pub type_: TypeIndex,
}

impl Typed for LocalVar {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

#[derive(Clone, Debug)]
pub struct Body {
    pub local_vars: Vec<LocalVar>,
    pub block: Block,
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

impl From<Block> for Stmt {
    fn from(block: Block) -> Self {
        Expr::from(block).into()
    }
}

impl From<Block> for Expr {
    fn from(block: Block) -> Self {
        BlockExpr::from(block).into()
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(LetStmt),
    Check(CheckStmt),
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: LocalVarIndex,
    pub rhs: Expr,
}

impl From<LetStmt> for Stmt {
    fn from(let_stmt: LetStmt) -> Self {
        Stmt::Let(let_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckStmtKind,
    pub cond: Expr,
}

impl From<CheckStmt> for Stmt {
    fn from(check_stmt: CheckStmt) -> Self {
        Stmt::Check(check_stmt)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Block(Box<BlockExpr>),
    Var(VarExpr),
    Call(CallExpr),
    Cast(Box<CastExpr>),
    Compare(Box<CompareExpr>),
    Assign(Box<AssignExpr>),
}

impl Typed for Expr {
    fn type_(&self) -> TypeIndex {
        match self {
            Expr::Block(block_expr) => block_expr.type_(),
            Expr::Var(var_expr) => var_expr.type_(),
            Expr::Call(call_expr) => call_expr.type_(),
            Expr::Cast(cast_expr) => cast_expr.type_(),
            Expr::Compare(compare_expr) => compare_expr.type_(),
            Expr::Assign(assign_expr) => assign_expr.type_(),
        }
    }
}

impl From<BuiltInVar> for Expr {
    fn from(built_in_var: BuiltInVar) -> Self {
        Expr::Var(built_in_var.into())
    }
}

impl From<&BuiltInVar> for Expr {
    fn from(built_in_var: &BuiltInVar) -> Self {
        Expr::Var(built_in_var.into())
    }
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        Stmt::Expr(expr)
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

impl From<Box<BlockExpr>> for Expr {
    fn from(block_expr: Box<BlockExpr>) -> Self {
        Expr::Block(block_expr)
    }
}

impl From<BlockExpr> for Expr {
    fn from(block_expr: BlockExpr) -> Self {
        Box::new(block_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct VarExpr {
    pub index: VarIndex,
    pub type_: TypeIndex,
}

impl From<VarExpr> for Expr {
    fn from(var_expr: VarExpr) -> Self {
        Expr::Var(var_expr)
    }
}

impl Typed for VarExpr {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl From<BuiltInVar> for VarExpr {
    fn from(built_in_var: BuiltInVar) -> Self {
        VarExpr {
            index: built_in_var.into(),
            type_: built_in_var.type_().into(),
        }
    }
}

impl From<&BuiltInVar> for VarExpr {
    fn from(built_in_var: &BuiltInVar) -> Self {
        (*built_in_var).into()
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: FnIndex,
    pub is_fallible: bool,
    pub is_unsafe: bool,
    pub args: Vec<CallExprArg>,
    pub ret: TypeIndex,
}

impl Typed for CallExpr {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

impl From<CallExpr> for Expr {
    fn from(call_expr: CallExpr) -> Self {
        Expr::Call(call_expr)
    }
}

#[derive(Clone, Debug)]
pub enum CallExprArg {
    Expr(Expr),
    OutRef(OutRef),
}

impl Typed for CallExprArg {
    fn type_(&self) -> TypeIndex {
        match self {
            CallExprArg::Expr(expr) => expr.type_(),
            CallExprArg::OutRef(out_ref) => out_ref.type_(),
        }
    }
}

impl From<Expr> for CallExprArg {
    fn from(expr: Expr) -> Self {
        CallExprArg::Expr(expr)
    }
}

#[derive(Clone, Debug)]
pub struct OutRef {
    pub index: ScopedVarIndex,
    pub type_: TypeIndex,
    pub upcast_route: Vec<TypeIndex>,
}

impl Typed for OutRef {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl From<OutRef> for CallExprArg {
    fn from(out_ref: OutRef) -> Self {
        CallExprArg::OutRef(out_ref)
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

impl From<Box<CastExpr>> for Expr {
    fn from(cast_expr: Box<CastExpr>) -> Self {
        Expr::Cast(cast_expr)
    }
}

impl From<CastExpr> for Expr {
    fn from(cast_expr: CastExpr) -> Self {
        Box::new(cast_expr).into()
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

impl From<Box<CompareExpr>> for Expr {
    fn from(compare_expr: Box<CompareExpr>) -> Self {
        Expr::Compare(compare_expr)
    }
}

impl From<CompareExpr> for Expr {
    fn from(compare_expr: CompareExpr) -> Self {
        Box::new(compare_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: ParamVarIndex,
    pub rhs: Expr,
}

impl Typed for AssignExpr {
    fn type_(&self) -> TypeIndex {
        self.rhs.type_()
    }
}

impl From<Box<AssignExpr>> for Expr {
    fn from(assign_expr: Box<AssignExpr>) -> Self {
        Expr::Assign(assign_expr)
    }
}

impl From<AssignExpr> for Expr {
    fn from(assign_expr: AssignExpr) -> Self {
        Box::new(assign_expr).into()
    }
}
