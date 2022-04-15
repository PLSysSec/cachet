// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use derive_more::From;
use typed_index_collections::TiVec;

use cachet_util::{box_from, deref_from, deref_index, field_index};

use crate::ast::{
    BlockKind, BuiltInType, BuiltInVar, CastKind, CheckKind, CompareKind, NegateKind, Path,
    Spanned,
};
use crate::type_checker;
pub use crate::type_checker::{
    BindStmt, CallableIndex, DeclIndex, EnumIndex, EnumItem, EnumVariantIndex, FnIndex,
    GlobalVarIndex, GlobalVarItem, GotoStmt, IrIndex, IrItem, Label, LabelIndex, LabelParam,
    LabelParamIndex, LabelStmt, Literal, LocalLabelIndex, LocalVar, LocalVarIndex, Locals,
    NotPartOfDeclOrderError, OpIndex, OutVar, OutVarArg, ParamIndex, Params, ParentIndex,
    StructIndex, StructItem, TypeIndex, Typed, VarExpr, VarIndex, VarParam, VarParamIndex,
    VariantIndex,
};

#[derive(Clone, Debug)]
pub struct Env<B = ()> {
    pub enum_items: TiVec<EnumIndex, EnumItem>,
    pub struct_items: TiVec<StructIndex, StructItem>,
    pub ir_items: TiVec<IrIndex, IrItem>,
    pub global_var_items: TiVec<GlobalVarIndex, GlobalVarItem>,
    pub fn_items: TiVec<FnIndex, CallableItem<B>>,
    pub op_items: TiVec<OpIndex, CallableItem<B>>,
    pub decl_order: Vec<DeclIndex>,
}

field_index!(Env<B>:enum_items[EnumIndex] => EnumItem | <B>);
field_index!(Env<B>:struct_items[StructIndex] => StructItem | <B>);
field_index!(Env<B>:ir_items[IrIndex] => IrItem | <B>);
field_index!(Env<B>:global_var_items[GlobalVarIndex] => GlobalVarItem | <B>);
field_index!(Env<B>:fn_items[FnIndex] => CallableItem<B> | <B>);
field_index!(Env<B>:op_items[OpIndex] => CallableItem<B> | <B>);

impl<B> Index<EnumVariantIndex> for Env<B> {
    type Output = Spanned<Path>;

    fn index(&self, index: EnumVariantIndex) -> &Self::Output {
        &self.enum_items[index.enum_index][index.variant_index]
    }
}

impl<B> IndexMut<EnumVariantIndex> for Env<B> {
    fn index_mut(&mut self, index: EnumVariantIndex) -> &mut Self::Output {
        &mut self.enum_items[index.enum_index][index.variant_index]
    }
}

deref_index!(Env<B>[&EnumVariantIndex] => Spanned<Path> | <B>);

impl<B> Index<CallableIndex> for Env<B> {
    type Output = CallableItem<B>;

    fn index(&self, index: CallableIndex) -> &Self::Output {
        match index {
            CallableIndex::Fn(fn_index) => &self.fn_items[fn_index],
            CallableIndex::Op(op_index) => &self.op_items[op_index],
        }
    }
}

impl<B> IndexMut<CallableIndex> for Env<B> {
    fn index_mut(&mut self, index: CallableIndex) -> &mut Self::Output {
        match index {
            CallableIndex::Fn(fn_index) => &mut self.fn_items[fn_index],
            CallableIndex::Op(op_index) => &mut self.op_items[op_index],
        }
    }
}

deref_index!(Env<B>[&CallableIndex] => CallableItem<B> | <B>);

#[derive(Clone, Debug)]
pub struct CallableItem<B = ()> {
    pub path: Spanned<Path>,
    pub parent: Option<ParentIndex>,
    pub is_unsafe: bool,
    pub params: Params,
    pub param_order: Vec<ParamIndex>,
    pub ret: Option<TypeIndex>,
    pub interprets: Option<IrIndex>,
    pub emits: Option<IrIndex>,
    pub body: Option<Body<B>>,
}

impl<B> Typed for CallableItem<B> {
    fn type_(&self) -> TypeIndex {
        self.ret.unwrap_or_else(|| BuiltInType::Unit.into())
    }
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    Expr(AtomExpr),
    OutVar(OutVarArg),
    Label(LabelArg),
}

#[derive(Clone, Debug)]
pub struct LabelArg {
    pub label: LabelIndex,
    pub is_out: bool,
    pub ir: IrIndex,
}

#[derive(Clone, Debug)]
pub struct Call {
    pub target: CallableIndex,
    pub is_unsafe: bool,
    pub args: Vec<Arg>,
}

#[derive(Clone, Debug)]
pub struct Body<B = ()> {
    pub locals: Locals,
    pub stmts: Vec<Stmt<B>>,
}

field_index!(Body<B>:locals[LocalVarIndex] => LocalVar | <B>);
field_index!(Body<B>:locals[LocalLabelIndex] => Label | <B>);

#[derive(Clone, Debug, From)]
pub enum Stmt<B = ()> {
    #[from]
    Let(LetStmt<B>),
    #[from]
    Label(LabelStmt),
    #[from]
    If(IfStmt<B>),
    #[from]
    Check(CheckStmt<B>),
    #[from]
    Goto(GotoStmt),
    #[from]
    Bind(BindStmt),
    #[from]
    Emit(EmitStmt),
    Block(B, BlockStmt<B>),
    #[from]
    Invoke(InvokeStmt),
    #[from]
    Assign(AssignStmt<B>),
    #[from]
    Ret(RetStmt<B>),
}

impl<B: Default> From<BlockStmt<B>> for Stmt<B> {
    fn from(block_stmt: BlockStmt<B>) -> Self {
        Stmt::Block(B::default(), block_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct LetStmt<B = ()> {
    pub lhs: LocalVarIndex,
    pub rhs: Expr<B>,
}

#[derive(Clone, Debug)]
pub struct IfStmt<B = ()> {
    pub cond: Expr<B>,
    pub then: Vec<Stmt<B>>,
    pub else_: Option<ElseClause<B>>,
}

#[derive(Clone, Debug, From)]
pub enum ElseClause<B = ()> {
    #[from]
    ElseIf(Box<IfStmt<B>>),
    #[from]
    Else(Vec<Stmt<B>>),
}

#[derive(Clone, Debug)]
pub struct CheckStmt<B = ()> {
    pub kind: CheckKind,
    pub cond: Expr<B>,
}

#[derive(Clone, Debug)]
pub struct EmitStmt {
    pub call: Call,
    pub ir: IrIndex,
}

#[derive(Clone, Debug)]
pub struct BlockStmt<B = ()> {
    pub kind: Option<BlockKind>,
    pub stmts: Vec<Stmt<B>>,
}

pub type InvokeStmt = InvokeExpr;

#[derive(Clone, Debug)]
pub struct AssignStmt<B = ()> {
    pub lhs: VarIndex,
    pub rhs: Expr<B>,
}

#[derive(Clone, Debug)]
pub struct RetStmt<B = ()> {
    // TODO(spinda): Record when this statement is at an end of control flow
    // for a body. Do the same for goto statements as well.
    pub value: Option<Expr<B>>,
}

#[derive(Clone, Debug, From)]
pub enum Expr<B = ()> {
    Block(B, Box<BlockExpr<B>>),
    #[from]
    Literal(Literal),
    #[from(types(BuiltInVar, "&BuiltInVar"))]
    Var(VarExpr),
    #[from]
    Invoke(InvokeExpr),
    #[from]
    Negate(Box<NegateExpr<Expr<B>>>),
    #[from]
    Cast(Box<CastExpr<Expr<B>>>),
    #[from]
    Compare(CompareExpr),
}

impl<B> Typed for Expr<B> {
    fn type_(&self) -> TypeIndex {
        match self {
            Expr::Block(_, block_expr) => block_expr.type_(),
            Expr::Literal(literal) => literal.type_(),
            Expr::Var(var_expr) => var_expr.type_(),
            Expr::Invoke(invoke_expr) => invoke_expr.type_(),
            Expr::Negate(negate_expr) => negate_expr.type_(),
            Expr::Cast(cast_expr) => cast_expr.type_(),
            Expr::Compare(compare_expr) => compare_expr.type_(),
        }
    }
}

impl<B: Default> From<Box<BlockExpr<B>>> for Expr<B> {
    fn from(block_expr: Box<BlockExpr<B>>) -> Expr<B> {
        Expr::Block(B::default(), block_expr)
    }
}

box_from!(BlockExpr<B> => Expr<B> | <B> where B: Default);
box_from!(NegateExpr<Expr<B>> => Expr<B> | <B>);
box_from!(CastExpr<Expr<B>> => Expr<B> | <B>);

deref_from!(&Literal => Expr);

impl<B> From<AtomExpr> for Expr<B> {
    fn from(atom_expr: AtomExpr) -> Self {
        match atom_expr {
            AtomExpr::Var(var_expr) => var_expr.into(),
            AtomExpr::Literal(literal) => literal.into(),
            AtomExpr::Negate(negate_expr) => (*negate_expr).into(),
            AtomExpr::Cast(cast_expr) => (*cast_expr).into(),
            AtomExpr::Compare(compare_expr) => (*compare_expr).into(),
        }
    }
}

impl<B> From<NegateExpr<AtomExpr>> for Expr<B> {
    fn from(negate_expr: NegateExpr<AtomExpr>) -> Self {
        Expr::from(NegateExpr::<Expr<B>>::from(negate_expr))
    }
}

impl<B> From<CastExpr<AtomExpr>> for Expr<B> {
    fn from(cast_expr: CastExpr<AtomExpr>) -> Self {
        Expr::from(CastExpr::<Expr<B>>::from(cast_expr))
    }
}

#[derive(Clone, Debug, From)]
pub enum AtomExpr {
    #[from]
    Literal(Literal),
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
            AtomExpr::Literal(literal) => literal.type_(),
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

deref_from!(&Literal => AtomExpr);

impl<B> TryFrom<Expr<B>> for AtomExpr {
    type Error = Expr<B>;

    fn try_from(expr: Expr<B>) -> Result<Self, Self::Error> {
        match expr {
            expr @ (Expr::Block(_, _) | Expr::Invoke(_)) => Err(expr),
            Expr::Literal(literal) => Ok(literal.into()),
            Expr::Var(var_expr) => Ok(var_expr.into()),
            Expr::Negate(negate_expr) => Ok((*negate_expr).try_into()?),
            Expr::Cast(cast_expr) => Ok((*cast_expr).try_into()?),
            Expr::Compare(compare_expr) => Ok(compare_expr.into()),
        }
    }
}

impl<B> TryFrom<NegateExpr<Expr<B>>> for AtomExpr {
    type Error = NegateExpr<Expr<B>>;

    fn try_from(negate_expr: NegateExpr<Expr<B>>) -> Result<Self, Self::Error> {
        Ok(AtomExpr::from(NegateExpr::<AtomExpr>::try_from(
            negate_expr,
        )?))
    }
}

impl<B> TryFrom<CastExpr<Expr<B>>> for AtomExpr {
    type Error = CastExpr<Expr<B>>;

    fn try_from(cast_expr: CastExpr<Expr<B>>) -> Result<Self, Self::Error> {
        Ok(AtomExpr::from(CastExpr::<AtomExpr>::try_from(cast_expr)?))
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr<B = ()> {
    pub kind: Option<BlockKind>,
    pub stmts: Vec<Stmt<B>>,
    pub value: Expr<B>,
}

impl<B> Typed for BlockExpr<B> {
    fn type_(&self) -> TypeIndex {
        self.value.type_()
    }
}

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
pub struct NegateExpr<E = Expr> {
    pub kind: NegateKind,
    pub expr: E,
}

impl<E: Typed> Typed for NegateExpr<E> {
    fn type_(&self) -> TypeIndex {
        self.expr.type_()
    }
}

impl<B> From<NegateExpr<AtomExpr>> for NegateExpr<Expr<B>> {
    fn from(negate_expr: NegateExpr<AtomExpr>) -> Self {
        NegateExpr {
            kind: negate_expr.kind,
            expr: negate_expr.expr.into(),
        }
    }
}

impl<B> TryFrom<NegateExpr<Expr<B>>> for NegateExpr<AtomExpr> {
    type Error = NegateExpr<Expr<B>>;

    fn try_from(negate_expr: NegateExpr<Expr<B>>) -> Result<Self, Self::Error> {
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
    pub kind: CastKind,
    pub expr: E,
    pub type_: TypeIndex,
}

impl<E> Typed for CastExpr<E> {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl<B> From<CastExpr<AtomExpr>> for CastExpr<Expr<B>> {
    fn from(cast_expr: CastExpr<AtomExpr>) -> Self {
        CastExpr {
            kind: cast_expr.kind,
            expr: cast_expr.expr.into(),
            type_: cast_expr.type_,
        }
    }
}

impl<B> TryFrom<CastExpr<Expr<B>>> for CastExpr<AtomExpr> {
    type Error = CastExpr<Expr<B>>;

    fn try_from(cast_expr: CastExpr<Expr<B>>) -> Result<Self, Self::Error> {
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
    pub kind: CompareKind,
    pub lhs: AtomExpr,
    pub rhs: AtomExpr,
}

impl CompareExpr {
    pub const TYPE: BuiltInType = type_checker::CompareExpr::TYPE;
}

impl Typed for CompareExpr {
    fn type_(&self) -> TypeIndex {
        CompareExpr::TYPE.into()
    }
}
