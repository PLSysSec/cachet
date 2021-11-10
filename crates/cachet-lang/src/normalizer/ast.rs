// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use derive_more::From;
use typed_index_collections::TiVec;

use crate::ast::{Ident, Path, Spanned};
pub use crate::type_checker::{
    BlockExprKind as BlockKind, BuiltInType, BuiltInTypeMap, BuiltInVar, BuiltInVarMap,
    CallableIndex, CastExprKind, CheckStmtKind, CompareExprKind, DeclIndex, EnumIndex, EnumItem,
    EnumVariantIndex, FnIndex, FnParamIndex, FnParams, GlobalVarIndex, GlobalVarItem, GotoStmt,
    IrIndex, IrItem, LabelIndex, LabelParamIndex, LocalLabelIndex, LocalVar, LocalVarIndex,
    NegateExprKind, NotPartOfDeclOrderError, OpIndex, OutVar, OutVarArg, OutVarParam,
    OutVarParamIndex, ParamIndex, Params, ParentIndex, StructIndex, StructItem, TypeIndex, Typed,
    VarExpr, VarIndex, VarParam, VarParamIndex, VariantIndex, BUILT_IN_TYPES, BUILT_IN_VARS,
    NUM_BUILT_IN_TYPES, NUM_BUILT_IN_VARS,
};
use crate::util::{box_from, deref_from, deref_index, field_index};

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

#[derive(Clone, Debug)]
pub struct Body {
    pub local_vars: TiVec<LocalVarIndex, LocalVar>,
    pub local_labels: TiVec<LocalLabelIndex, Spanned<Ident>>,
    pub stmts: Vec<Stmt>,
}

field_index!(Body:local_vars[LocalVarIndex] => LocalVar);
field_index!(Body:local_labels[LocalLabelIndex] => Spanned<Ident>);

#[derive(Clone, Debug, From)]
pub enum Stmt {
    Let(LetStmt),
    If(IfStmt),
    Check(CheckStmt),
    Goto(GotoStmt),
    Emit(EmitStmt),
    Block(BlockStmt),
    Call(FnCall),
    Assign(AssignStmt),
    Ret(RetStmt),
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
    pub then: Vec<Stmt>,
    pub else_: Option<Vec<Stmt>>,
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

#[derive(Clone, Debug)]
pub struct BlockStmt {
    pub kind: Option<BlockKind>,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct AssignStmt {
    pub lhs: VarIndex,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct RetStmt {
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct FnCall {
    pub target: FnIndex,
    pub is_unsafe: bool,
    pub args: Vec<FnArg>,
    pub ret: TypeIndex,
}

impl Typed for FnCall {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

#[derive(Clone, Debug, From)]
pub enum FnArg {
    #[from(types(AtomExpr, LabelIndex, "&LabelIndex"))]
    Arg(Arg),
    #[from]
    OutVar(OutVarArg),
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    Expr(AtomExpr),
    Label(LabelIndex),
}

deref_from!(&LabelIndex => Arg);

#[derive(Clone, Debug, From)]
pub enum Expr {
    #[from]
    Block(Box<BlockExpr>),
    #[from(types(BuiltInVar, "&BuiltInVar"))]
    Var(VarExpr),
    #[from]
    Call(FnCall),
    #[from]
    Negate(Box<NegateExpr>),
    #[from]
    Cast(Box<CastExpr>),
    #[from]
    Compare(CompareExpr),
}

impl Typed for Expr {
    fn type_(&self) -> TypeIndex {
        match self {
            Expr::Block(block_expr) => block_expr.type_(),
            Expr::Var(var_expr) => var_expr.type_(),
            Expr::Call(fn_call) => fn_call.type_(),
            Expr::Negate(negate_expr) => negate_expr.type_(),
            Expr::Cast(cast_expr) => cast_expr.type_(),
            Expr::Compare(compare_expr) => compare_expr.type_(),
        }
    }
}

box_from!(BlockExpr => Expr);
box_from!(NegateExpr => Expr);
box_from!(CastExpr => Expr);

impl From<AtomExpr> for Expr {
    fn from(atom_expr: AtomExpr) -> Self {
        match atom_expr {
            AtomExpr::Var(var_expr) => var_expr.into(),
            AtomExpr::Negate(negate_expr) => (*negate_expr).into(),
            AtomExpr::Cast(cast_expr) => (*cast_expr).into(),
            AtomExpr::Compare(compare_expr) => (*compare_expr).into(),
        }
    }
}

impl From<NegateExpr<AtomExpr>> for Expr {
    fn from(negate_expr: NegateExpr<AtomExpr>) -> Self {
        Expr::from(NegateExpr::<Expr>::from(negate_expr))
    }
}

impl From<CastExpr<AtomExpr>> for Expr {
    fn from(cast_expr: CastExpr<AtomExpr>) -> Self {
        Expr::from(CastExpr::<Expr>::from(cast_expr))
    }
}

#[derive(Clone, Debug, From)]
pub enum AtomExpr {
    #[from(types(BuiltInVar, "&BuiltInVar"))]
    Var(VarExpr),
    #[from]
    Negate(Box<NegateExpr<AtomExpr>>),
    #[from]
    Cast(Box<CastExpr<AtomExpr>>),
    #[from]
    Compare(Box<CompareExpr>),
}

impl Typed for AtomExpr {
    fn type_(&self) -> TypeIndex {
        match self {
            AtomExpr::Var(var_expr) => var_expr.type_(),
            AtomExpr::Negate(negate_expr) => negate_expr.type_(),
            AtomExpr::Cast(cast_expr) => cast_expr.type_(),
            AtomExpr::Compare(compare_expr) => compare_expr.type_(),
        }
    }
}

box_from!(NegateExpr<AtomExpr> => AtomExpr);
box_from!(CastExpr<AtomExpr> => AtomExpr);
box_from!(CompareExpr => AtomExpr);

impl TryFrom<Expr> for AtomExpr {
    type Error = Expr;

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            expr @ (Expr::Block(_) | Expr::Call(_)) => Err(expr),
            Expr::Var(var_expr) => Ok(var_expr.into()),
            Expr::Negate(negate_expr) => Ok((*negate_expr).try_into()?),
            Expr::Cast(cast_expr) => Ok((*cast_expr).try_into()?),
            Expr::Compare(compare_expr) => Ok(compare_expr.into()),
        }
    }
}

impl TryFrom<NegateExpr> for AtomExpr {
    type Error = NegateExpr;

    fn try_from(negate_expr: NegateExpr) -> Result<Self, Self::Error> {
        Ok(AtomExpr::from(NegateExpr::<AtomExpr>::try_from(
            negate_expr,
        )?))
    }
}

impl TryFrom<CastExpr> for AtomExpr {
    type Error = CastExpr;

    fn try_from(cast_expr: CastExpr) -> Result<Self, Self::Error> {
        Ok(AtomExpr::from(CastExpr::<AtomExpr>::try_from(cast_expr)?))
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub kind: Option<BlockKind>,
    pub stmts: Vec<Stmt>,
    pub value: Expr,
}

impl Typed for BlockExpr {
    fn type_(&self) -> TypeIndex {
        self.value.type_()
    }
}

#[derive(Clone, Debug)]
pub struct NegateExpr<E = Expr> {
    pub kind: NegateExprKind,
    pub expr: E,
}

impl<E: Typed> Typed for NegateExpr<E> {
    fn type_(&self) -> TypeIndex {
        self.expr.type_()
    }
}

impl From<NegateExpr<AtomExpr>> for NegateExpr {
    fn from(negate_expr: NegateExpr<AtomExpr>) -> Self {
        NegateExpr {
            kind: negate_expr.kind,
            expr: negate_expr.expr.into(),
        }
    }
}

impl TryFrom<NegateExpr> for NegateExpr<AtomExpr> {
    type Error = NegateExpr;

    fn try_from(negate_expr: NegateExpr) -> Result<Self, Self::Error> {
        match negate_expr.expr.try_into() {
            Ok(expr) => Ok(NegateExpr {
                kind: negate_expr.kind,
                expr,
            }),
            Err(expr) => Err(NegateExpr {
                kind: negate_expr.kind,
                expr,
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr<E = Expr> {
    pub kind: CastExprKind,
    pub expr: E,
    pub type_: TypeIndex,
}

impl<E> Typed for CastExpr<E> {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl From<CastExpr<AtomExpr>> for CastExpr {
    fn from(cast_expr: CastExpr<AtomExpr>) -> Self {
        CastExpr {
            kind: cast_expr.kind,
            expr: cast_expr.expr.into(),
            type_: cast_expr.type_,
        }
    }
}

impl TryFrom<CastExpr> for CastExpr<AtomExpr> {
    type Error = CastExpr;

    fn try_from(cast_expr: CastExpr) -> Result<Self, Self::Error> {
        match cast_expr.expr.try_into() {
            Ok(expr) => Ok(CastExpr {
                kind: cast_expr.kind,
                expr,
                type_: cast_expr.type_,
            }),
            Err(expr) => Err(CastExpr {
                kind: cast_expr.kind,
                expr,
                type_: cast_expr.type_,
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: CompareExprKind,
    pub lhs: AtomExpr,
    pub rhs: AtomExpr,
}

impl Typed for CompareExpr {
    fn type_(&self) -> TypeIndex {
        BuiltInType::Bool.into()
    }
}
