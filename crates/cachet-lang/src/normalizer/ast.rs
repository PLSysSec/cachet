// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use derive_more::From;
use enumset::EnumSet;
use typed_index_collections::TiVec;

use crate::ast::{BinOper, BlockKind, CastKind, CheckKind, NegateKind, Path, Spanned};
use crate::built_in::{BuiltInAttr, BuiltInType, BuiltInVar};

pub use crate::type_checker::{
    BindStmt, CallableIndex, DeclIndex, EnumIndex, EnumItem, EnumVariantIndex, Field, FieldIndex,
    FnIndex, FreeVarRef, GlobalVarIndex, GlobalVarItem, GotoStmt, HasAttrs, IrIndex, IrItem,
    Label, LabelIndex, LabelParam, LabelParamIndex, LabelStmt, Literal, LocalLabelIndex, LocalVar,
    LocalVarIndex, Locals, NotPartOfDeclOrderError, OpIndex, ParamIndex, Params, ParentIndex,
    StructFieldIndex, StructIndex, StructItem, TypeIndex, Typed, VarExpr, VarIndex, VarParam,
    VarParamIndex, VarRef, VarRefArg, VariantIndex,
};
use cachet_util::{box_from, deref_from, deref_index, field_index, typed_index};

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

impl Index<StructFieldIndex> for Env {
    type Output = Field;

    fn index(&self, index: StructFieldIndex) -> &Self::Output {
        &self.struct_items[index.struct_index][index.field_index]
    }
}

impl IndexMut<StructFieldIndex> for Env {
    fn index_mut(&mut self, index: StructFieldIndex) -> &mut Self::Output {
        &mut self.struct_items[index.struct_index][index.field_index]
    }
}

deref_index!(Env[&StructFieldIndex] => Field);

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
    pub attrs: EnumSet<BuiltInAttr>,
    pub is_unsafe: bool,
    pub params: Params,
    pub param_order: Vec<ParamIndex>,
    pub ret: Option<TypeIndex>,
    pub interprets: Option<IrIndex>,
    pub emits: Option<IrIndex>,
    pub body: Option<Body<B>>,
}

impl<B> HasAttrs for CallableItem<B> {
    fn attrs(&self) -> &EnumSet<BuiltInAttr> {
        &self.attrs
    }
}

impl<B> Typed for CallableItem<B> {
    fn type_(&self) -> TypeIndex {
        self.ret.unwrap_or_else(|| BuiltInType::Unit.into())
    }
}

#[derive(Clone, Debug, From)]
pub enum Arg<B = ()> {
    Expr(PureExpr<B>),
    VarRef(VarRefArg),
    Label(LabelArg),
}

#[derive(Clone, Debug)]
pub struct LabelArg {
    pub label: LabelIndex,
    pub is_out_ref: bool,
    pub ir: IrIndex,
}

#[derive(Clone, Debug)]
pub struct Call<B = ()> {
    pub target: CallableIndex,
    pub is_unsafe: bool,
    pub args: Vec<Arg<B>>,
    pub local_call_index: LocalCallIndex,
}

typed_index!(pub LocalCallIndex);

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
    Emit(EmitStmt<B>),
    Block(B, BlockStmt<B>),
    #[from]
    Invoke(InvokeStmt<B>),
    #[from]
    Assign(AssignStmt<B>),
    #[from]
    Ret(RetStmt<B>),
}

impl<B: Default> From<BlockStmt<B>> for Stmt<B> {
    fn from(block_stmt: BlockStmt<B>) -> Self {
        Self::Block(B::default(), block_stmt)
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
pub struct EmitStmt<B = ()> {
    pub call: Call<B>,
    pub ir: IrIndex,
}

#[derive(Clone, Debug)]
pub struct BlockStmt<B = ()> {
    pub kind: Option<BlockKind>,
    pub stmts: Vec<Stmt<B>>,
}

pub type InvokeStmt<B = ()> = InvokeExpr<B>;

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
    Invoke(InvokeExpr<B>),
    #[from]
    FieldAccess(Box<FieldAccessExpr<Expr<B>>>),
    #[from]
    Negate(Box<NegateExpr<Expr<B>>>),
    #[from]
    Cast(Box<CastExpr<Expr<B>>>),
    #[from]
    BinOper(BinOperExpr<B>),
}

impl<B> Typed for Expr<B> {
    fn type_(&self) -> TypeIndex {
        match self {
            Expr::Block(_, block_expr) => block_expr.type_(),
            Expr::Literal(literal) => literal.type_(),
            Expr::Var(var_expr) => var_expr.type_(),
            Expr::Invoke(invoke_expr) => invoke_expr.type_(),
            Expr::FieldAccess(field_access_expr) => field_access_expr.type_(),
            Expr::Negate(negate_expr) => negate_expr.type_(),
            Expr::Cast(cast_expr) => cast_expr.type_(),
            Expr::BinOper(bin_oper_expr) => bin_oper_expr.type_(),
        }
    }
}

impl<B: Default> From<Box<BlockExpr<B>>> for Expr<B> {
    fn from(block_expr: Box<BlockExpr<B>>) -> Self {
        Self::Block(B::default(), block_expr)
    }
}

box_from!(BlockExpr<B> => Expr<B> | <B> where B: Default);
box_from!(FieldAccessExpr<Expr<B>> => Expr<B> | <B>);
box_from!(NegateExpr<Expr<B>> => Expr<B> | <B>);
box_from!(CastExpr<Expr<B>> => Expr<B> | <B>);

deref_from!(&Literal => Expr);

impl<B> From<PureExpr<B>> for Expr<B> {
    fn from(pure_expr: PureExpr<B>) -> Self {
        match pure_expr {
            PureExpr::Block(reachable, block_expr) => {
                Expr::Block(reachable, Box::new((*block_expr).into()))
            }
            PureExpr::Var(var_expr) => var_expr.into(),
            PureExpr::Literal(literal) => literal.into(),
            PureExpr::FieldAccess(field_access_expr) => (*field_access_expr).into(),
            PureExpr::Negate(negate_expr) => (*negate_expr).into(),
            PureExpr::Cast(cast_expr) => (*cast_expr).into(),
            PureExpr::BinOper(bin_oper_expr) => (*bin_oper_expr).into(),
        }
    }
}

impl<B> From<FieldAccessExpr<PureExpr<B>>> for Expr<B> {
    fn from(field_access_expr: FieldAccessExpr<PureExpr<B>>) -> Self {
        Expr::from(FieldAccessExpr::<Expr<B>>::from(field_access_expr))
    }
}

impl<B> From<NegateExpr<PureExpr<B>>> for Expr<B> {
    fn from(negate_expr: NegateExpr<PureExpr<B>>) -> Self {
        Expr::from(NegateExpr::<Expr<B>>::from(negate_expr))
    }
}

impl<B> From<CastExpr<PureExpr<B>>> for Expr<B> {
    fn from(cast_expr: CastExpr<PureExpr<B>>) -> Self {
        Expr::from(CastExpr::<Expr<B>>::from(cast_expr))
    }
}

#[derive(Clone, Debug, From)]
pub enum PureExpr<B = ()> {
    Block(B, Box<PureBlockExpr<B>>),
    #[from]
    Literal(Literal),
    #[from(types(BuiltInVar, "&BuiltInVar"))]
    Var(VarExpr),
    #[from]
    FieldAccess(Box<FieldAccessExpr<PureExpr<B>>>),
    #[from]
    Negate(Box<NegateExpr<PureExpr<B>>>),
    #[from]
    Cast(Box<CastExpr<PureExpr<B>>>),
    #[from]
    BinOper(Box<BinOperExpr<B>>),
}

impl<B> Typed for PureExpr<B> {
    fn type_(&self) -> TypeIndex {
        match self {
            PureExpr::Block(_, block_expr) => block_expr.type_(),
            PureExpr::Literal(literal) => literal.type_(),
            PureExpr::Var(var_expr) => var_expr.type_(),
            PureExpr::FieldAccess(field_access_expr) => field_access_expr.type_(),
            PureExpr::Negate(negate_expr) => negate_expr.type_(),
            PureExpr::Cast(cast_expr) => cast_expr.type_(),
            PureExpr::BinOper(bin_oper_expr) => bin_oper_expr.type_(),
        }
    }
}

impl<B: Default> From<Box<PureBlockExpr<B>>> for PureExpr<B> {
    fn from(block_expr: Box<PureBlockExpr<B>>) -> Self {
        Self::Block(B::default(), block_expr)
    }
}

box_from!(PureBlockExpr<B> => PureExpr<B> | <B> where B: Default);
box_from!(FieldAccessExpr<PureExpr<B>> => PureExpr<B> | <B>);
box_from!(NegateExpr<PureExpr<B>> => PureExpr<B> | <B>);
box_from!(CastExpr<PureExpr<B>> => PureExpr<B> | <B>);
box_from!(BinOperExpr<B> => PureExpr<B> | <B>);

deref_from!(&Literal => PureExpr<B> | <B>);

impl<B> TryFrom<Expr<B>> for PureExpr<B> {
    type Error = Expr<B>;

    fn try_from(expr: Expr<B>) -> Result<Self, Self::Error> {
        match expr {
            Expr::Block(reachable, block_expr) => {
                let block_expr = match (*block_expr).try_into() {
                    Ok(block_expr) => block_expr,
                    Err(block_expr) => return Err(Expr::Block(reachable, Box::new(block_expr))),
                };
                Ok(PureExpr::Block(reachable, Box::new(block_expr)))
            }
            Expr::Literal(literal) => Ok(literal.into()),
            Expr::Var(var_expr) => Ok(var_expr.into()),
            expr @ Expr::Invoke(_) => Err(expr),
            Expr::FieldAccess(field_access_expr) => Ok((*field_access_expr).try_into()?),
            Expr::Negate(negate_expr) => Ok((*negate_expr).try_into()?),
            Expr::Cast(cast_expr) => Ok((*cast_expr).try_into()?),
            Expr::BinOper(bin_oper_expr) => Ok(bin_oper_expr.into()),
        }
    }
}

impl<B> TryFrom<FieldAccessExpr<Expr<B>>> for PureExpr<B> {
    type Error = FieldAccessExpr<Expr<B>>;

    fn try_from(field_access_expr: FieldAccessExpr<Expr<B>>) -> Result<Self, Self::Error> {
        Ok(PureExpr::from(FieldAccessExpr::<PureExpr<B>>::try_from(
            field_access_expr,
        )?))
    }
}

impl<B> TryFrom<NegateExpr<Expr<B>>> for PureExpr<B> {
    type Error = NegateExpr<Expr<B>>;

    fn try_from(negate_expr: NegateExpr<Expr<B>>) -> Result<Self, Self::Error> {
        Ok(PureExpr::from(NegateExpr::<PureExpr<B>>::try_from(
            negate_expr,
        )?))
    }
}

impl<B> TryFrom<CastExpr<Expr<B>>> for PureExpr<B> {
    type Error = CastExpr<Expr<B>>;

    fn try_from(cast_expr: CastExpr<Expr<B>>) -> Result<Self, Self::Error> {
        Ok(PureExpr::from(CastExpr::<PureExpr<B>>::try_from(
            cast_expr,
        )?))
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

impl<B> From<PureBlockExpr<B>> for BlockExpr<B> {
    fn from(block_expr: PureBlockExpr<B>) -> Self {
        Self {
            kind: block_expr.kind,
            stmts: Vec::new(),
            value: block_expr.value.into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PureBlockExpr<B = ()> {
    pub kind: Option<BlockKind>,
    pub value: PureExpr<B>,
}

impl<B> Typed for PureBlockExpr<B> {
    fn type_(&self) -> TypeIndex {
        self.value.type_()
    }
}

impl<B> TryFrom<BlockExpr<B>> for PureBlockExpr<B> {
    type Error = BlockExpr<B>;

    fn try_from(block_expr: BlockExpr<B>) -> Result<Self, Self::Error> {
        if !block_expr.stmts.is_empty() {
            return Err(block_expr);
        }

        Ok(Self {
            kind: block_expr.kind,
            value: block_expr.value.try_into().map_err(|value| BlockExpr {
                kind: block_expr.kind,
                stmts: block_expr.stmts,
                value,
            })?,
        })
    }
}

#[derive(Clone, Debug)]
pub struct InvokeExpr<B = ()> {
    pub call: Call<B>,
    pub ret: TypeIndex,
}

impl<B> Typed for InvokeExpr<B> {
    fn type_(&self) -> TypeIndex {
        self.ret
    }
}

#[derive(Clone, Debug)]
pub struct FieldAccessExpr<E = Expr> {
    pub parent: E,
    pub field: StructFieldIndex,
    pub type_: TypeIndex,
}

impl<E> Typed for FieldAccessExpr<E> {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}

impl<B> From<FieldAccessExpr<PureExpr<B>>> for FieldAccessExpr<Expr<B>> {
    fn from(field_access_expr: FieldAccessExpr<PureExpr<B>>) -> Self {
        FieldAccessExpr {
            parent: field_access_expr.parent.into(),
            field: field_access_expr.field,
            type_: field_access_expr.type_,
        }
    }
}

impl<B> TryFrom<FieldAccessExpr<Expr<B>>> for FieldAccessExpr<PureExpr<B>> {
    type Error = FieldAccessExpr<Expr<B>>;

    fn try_from(field_access_expr: FieldAccessExpr<Expr<B>>) -> Result<Self, Self::Error> {
        match field_access_expr.parent.try_into() {
            Ok(parent) => Ok(FieldAccessExpr {
                parent,
                field: field_access_expr.field,
                type_: field_access_expr.type_,
            }),
            Err(parent) => Err(FieldAccessExpr {
                parent,
                field: field_access_expr.field,
                type_: field_access_expr.type_,
            }),
        }
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

impl<B> From<NegateExpr<PureExpr<B>>> for NegateExpr<Expr<B>> {
    fn from(negate_expr: NegateExpr<PureExpr<B>>) -> Self {
        NegateExpr {
            kind: negate_expr.kind,
            expr: negate_expr.expr.into(),
        }
    }
}

impl<B> TryFrom<NegateExpr<Expr<B>>> for NegateExpr<PureExpr<B>> {
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

impl<B> From<CastExpr<PureExpr<B>>> for CastExpr<Expr<B>> {
    fn from(cast_expr: CastExpr<PureExpr<B>>) -> Self {
        CastExpr {
            kind: cast_expr.kind,
            expr: cast_expr.expr.into(),
            type_: cast_expr.type_,
        }
    }
}

impl<B> TryFrom<CastExpr<Expr<B>>> for CastExpr<PureExpr<B>> {
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
pub struct BinOperExpr<B = ()> {
    pub oper: BinOper,
    pub lhs: PureExpr<B>,
    pub rhs: PureExpr<B>,
    pub type_: TypeIndex,
}

impl<B> Typed for BinOperExpr<B> {
    fn type_(&self) -> TypeIndex {
        self.type_
    }
}
