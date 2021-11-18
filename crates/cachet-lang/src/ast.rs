// vim: set tw=99 ts=4 sts=4 sw=4 et:

// TODO(spinda): Can we get rid of all the unnecessary extra trait bounds
// brought on by Rust's broken derive parameter inference strategy?

use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

use derive_more::From;
use lazy_static::lazy_static;
use typed_index_collections::TiVec;

use crate::util::{box_from, chain_from, deref_from, typed_field_index, typed_index};

pub use crate::ast::hole::*;
pub use crate::ast::ident::*;
pub use crate::ast::span::*;

mod hole;
mod ident;
mod span;

pub trait Phase: Copy + Debug + PartialEq {
    type TypeKey: Copy + Debug + From<BuiltInType>;
    type VarKey: Copy + Debug;
    type LabelKey: Copy + Debug;
    type IrKey: Copy + Debug;
    type ParentKey<C: ParentConfig>: Copy + Debug;
    type FnKey: Copy + Debug;
    type OpKey: Copy + Debug;

    type CallableItemName: Copy + Debug;
    type CallableItemCanHaveParent: Flag;
    type BodyHasLocals: HoleConfig;
    type LocalVarHasType: HoleConfig;
    type LocalVarTypeHasSpan: HoleConfig;
    type LetStmtLhs: Clone + Debug;
    type ExprHasType: HoleConfig;
}

pub trait Typed {
    type Type;
    fn type_(&self) -> Self::Type;
}

impl<T: Typed> Typed for Box<T> {
    type Type = T::Type;
    fn type_(&self) -> Self::Type {
        (**self).type_()
    }
}

impl<T: Typed> Typed for Spanned<T> {
    type Type = T::Type;
    fn type_(&self) -> Self::Type {
        self.value.type_()
    }
}

pub const NUM_BUILT_IN_TYPES: usize = 4;

// TODO(spinda): It would be awesome if we could replace this with something
// like `TiArray<BuiltInType, NUM_BUILT_IN_TYPES>`.
pub type BuiltInTypeMap<T> = [T; NUM_BUILT_IN_TYPES];

lazy_static! {
    pub static ref UNIT_TYPE_IDENT: Ident = Ident::from("Unit");
    pub static ref BOOL_TYPE_IDENT: Ident = Ident::from("Bool");
    pub static ref INT32_TYPE_IDENT: Ident = Ident::from("Int32");
    pub static ref DOUBLE_TYPE_IDENT: Ident = Ident::from("Double");
    pub static ref BUILT_IN_TYPE_IDENTS: BuiltInTypeMap<Ident> = [
        *UNIT_TYPE_IDENT,
        *BOOL_TYPE_IDENT,
        *INT32_TYPE_IDENT,
        *DOUBLE_TYPE_IDENT,
    ];
    pub static ref UNIT_TYPE_PATH: Path = (*UNIT_TYPE_IDENT).into();
    pub static ref BOOL_TYPE_PATH: Path = (*BOOL_TYPE_IDENT).into();
    pub static ref INT32_TYPE_PATH: Path = (*INT32_TYPE_IDENT).into();
    pub static ref DOUBLE_TYPE_PATH: Path = (*DOUBLE_TYPE_IDENT).into();
    pub static ref BUILT_IN_TYPE_PATHS: BuiltInTypeMap<Path> = [
        *UNIT_TYPE_PATH,
        *BOOL_TYPE_PATH,
        *INT32_TYPE_PATH,
        *DOUBLE_TYPE_PATH,
    ];
}

impl From<BuiltInType> for Ident {
    fn from(built_in_type: BuiltInType) -> Self {
        built_in_type.ident()
    }
}

impl From<BuiltInType> for Path {
    fn from(built_in_type: BuiltInType) -> Self {
        built_in_type.path()
    }
}

// TODO(spinda): Implement TryFrom<Ident> and TryFrom<Path>.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInType {
    Unit,
    Bool,
    Int32,
    Double,
}

impl Typed for BuiltInType {
    type Type = TypeIndex;
    fn type_(&self) -> Self::Type {
        self.into()
    }
}

pub const BUILT_IN_TYPES: BuiltInTypeMap<BuiltInType> = [
    BuiltInType::Unit,
    BuiltInType::Bool,
    BuiltInType::Int32,
    BuiltInType::Double,
];

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

    pub fn path(self) -> Path {
        BUILT_IN_TYPE_PATHS[self]
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

typed_index!(pub EnumIndex);
typed_index!(pub StructIndex);
typed_index!(pub IrIndex);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum ParentIndex<C: ParentConfig> {
    Type(C::CanBeType, TypeIndex),
    #[from]
    Ir(IrIndex),
}

pub trait ParentConfig: Copy + Debug + PartialEq {
    type CanBeType: Flag;
}

impl<C: ParentConfig<CanBeType = Yes>> From<TypeIndex> for ParentIndex<C> {
    fn from(type_index: TypeIndex) -> Self {
        ParentIndex::Type((), type_index)
    }
}

chain_from!(
    BuiltInType => TypeIndex => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<TypeIndex>
);
chain_from!(
    EnumIndex => TypeIndex => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<TypeIndex>
);
chain_from!(
    StructIndex => TypeIndex => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<TypeIndex>
);

deref_from!(
    &TypeIndex => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<TypeIndex>
);
deref_from!(
    &BuiltInType => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<BuiltInType>
);
deref_from!(
    &EnumIndex => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<EnumIndex>
);
deref_from!(
    &StructIndex => ParentIndex<C>
    | <C> where C: ParentConfig, ParentIndex<C>: From<StructIndex>
);

deref_from!(&IrIndex => ParentIndex<C> | <C> where C: ParentConfig);

pub type AnyParentIndex = ParentIndex<AnyParentConfig>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct AnyParentConfig;

impl ParentConfig for AnyParentConfig {
    type CanBeType = Yes;
}

pub type OpParentIndex = ParentIndex<OpParentConfig>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct OpParentConfig;

impl ParentConfig for OpParentConfig {
    type CanBeType = No;
}

#[derive(Clone, Debug)]
pub struct CallableItem<C: CallableItemConfig, P: Phase> {
    pub name: Spanned<P::CallableItemName>,
    pub parent: Hole<
        (
            CanBeFull<P::CallableItemCanHaveParent>,
            CanBeEmpty<Not<C::MustHaveParent<P>>>,
        ),
        P::ParentKey<C::ParentConfig>,
    >,
    pub is_unsafe: bool,
    pub params: Params<C::ParamsConfig, P>,
    pub param_order: Vec<ParamIndex<C::ParamsConfig>>,
    pub ret: Hole<C::HasRet, Spanned<P::TypeKey>>,
    pub body: Spanned<Hole<C::HasBody, Body<P>>>,
}

pub trait CallableItemConfig: Copy + Debug + PartialEq {
    type MustHaveParent<P: Phase>: Flag;
    type ParentConfig: ParentConfig;
    type ParamsConfig: ParamsConfig;
    type HasRet: HoleConfig;
    type HasBody: HoleConfig;
}

impl<C: CallableItemConfig, P: Phase> Typed for CallableItem<C, P>
where
    C::HasRet: CanBeFullHoleConfig,
{
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        match self.ret {
            Hole::Full(_, ret) => ret.value,
            Hole::Empty(_) => BuiltInType::Unit.into(),
        }
    }
}

pub type FnItem<P> = CallableItem<FnCallableItemConfig, P>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct FnCallableItemConfig;

impl CallableItemConfig for FnCallableItemConfig {
    type MustHaveParent<P: Phase> = No;
    type ParentConfig = AnyParentConfig;
    type ParamsConfig = FnParamsConfig;
    type HasRet = Sometimes;
    type HasBody = Sometimes;
}

pub type OpItem<P> = CallableItem<OpCallableItemConfig, P>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct OpCallableItemConfig;

impl CallableItemConfig for OpCallableItemConfig {
    type MustHaveParent<P: Phase> = P::CallableItemCanHaveParent;
    type ParentConfig = OpParentConfig;
    type ParamsConfig = OpParamsConfig;
    type HasRet = Never;
    type HasBody = Always;
}

#[derive(Clone, Debug)]
pub struct Params<C: ParamsConfig, P: Phase> {
    pub var_params: TiVec<VarParamIndex, VarParam<P>>,
    pub out_var_params: Hole<C::HasOutVarParams, TiVec<OutVarParamIndex, OutVarParam<P>>>,
    pub label_params: TiVec<LabelParamIndex, Spanned<Ident>>,
}

impl<C: ParamsConfig, P: Phase> Default for Params<C, P>
where
    C::HasOutVarParams: ReachableHoleConfig,
{
    fn default() -> Self {
        Params {
            var_params: Default::default(),
            out_var_params: Default::default(),
            label_params: Default::default(),
        }
    }
}

typed_field_index!(
    Params<C, P>:var_params[pub VarParamIndex] => VarParam<P>
    | <C, P> where C: ParamsConfig, P: Phase
);
typed_field_index!(
    Params<C, P>:label_params[pub LabelParamIndex] => Spanned<Ident>
    | <C, P> where C: ParamsConfig, P: Phase
);

typed_index!(pub OutVarParamIndex);

impl<C: ParamsConfig, P: Phase> Index<OutVarParamIndex> for Params<C, P>
where
    C::HasOutVarParams: CanBeFullHoleConfig,
{
    type Output = OutVarParam<P>;

    fn index(&self, index: OutVarParamIndex) -> &Self::Output {
        &self.out_var_params.as_ref().unwrap()[index]
    }
}

impl<C: ParamsConfig, P: Phase> IndexMut<OutVarParamIndex> for Params<C, P>
where
    C::HasOutVarParams: CanBeFullHoleConfig,
{
    fn index_mut(&mut self, index: OutVarParamIndex) -> &mut Self::Output {
        &mut self.out_var_params.as_mut().unwrap()[index]
    }
}

pub trait ParamsConfig: Copy + Debug + PartialEq {
    type HasOutVarParams: HoleConfig;
}

pub type FnParams<P> = Params<FnParamsConfig, P>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct FnParamsConfig;

impl ParamsConfig for FnParamsConfig {
    type HasOutVarParams = Always;
}

pub type OpParams<P> = Params<OpParamsConfig, P>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct OpParamsConfig;

impl ParamsConfig for OpParamsConfig {
    type HasOutVarParams = Never;
}

#[derive(Clone, Copy, Debug, From, PartialEq)]
pub enum ParamIndex<C: ParamConfig> {
    Var(VarParamIndex),
    OutVar(C::CanBeOutVar, OutVarParamIndex),
    Label(LabelParamIndex),
}

pub trait ParamConfig: ParamsConfig {
    type CanBeOutVar: Flag;
}

impl<C: ParamsConfig> ParamConfig for C {
    type CanBeOutVar = <C::HasOutVarParams as HoleConfig>::CanBeFull;
}

impl<C> From<OutVarParamIndex> for ParamIndex<C>
where
    C: ParamConfig<CanBeOutVar = Yes>,
{
    fn from(out_var_param_index: OutVarParamIndex) -> Self {
        ParamIndex::OutVar(Default::default(), out_var_param_index)
    }
}

deref_from!(&VarParamIndex => ParamIndex<C> | <C> where C: ParamConfig);
deref_from!(
    &OutVarParamIndex => ParamIndex<C>
    | <C> where C: ParamConfig, ParamIndex<C>: From<OutVarParamIndex>
);
deref_from!(&LabelParamIndex => ParamIndex<C> | <C> where C: ParamConfig);

pub type FnParamIndex = ParamIndex<FnParamConfig>;
pub type FnParamConfig = FnParamsConfig;

pub type OpParamIndex = ParamIndex<OpParamConfig>;
pub type OpParamConfig = OpParamsConfig;

#[derive(Clone, Debug)]
pub struct VarParam<P: Phase> {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Spanned<P::TypeKey>,
}

impl<P: Phase> Typed for VarParam<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        self.type_.value
    }
}

#[derive(Clone, Debug)]
pub struct OutVarParam<P: Phase> {
    pub ident: Spanned<Ident>,
    pub type_: Spanned<P::TypeKey>,
}

impl<P: Phase> Typed for OutVarParam<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        self.type_.value
    }
}

#[derive(Clone, Debug)]
pub struct Call<C: CallConfig, P: Phase> {
    pub target: Spanned<C::Target<P>>,
    pub args: Spanned<Vec<Spanned<Arg<C::ParamConfig, P>>>>,
}

pub trait CallConfig: Copy + Debug + PartialEq {
    type Target<P: Phase>: Copy + Debug;
    type ParamConfig: ParamConfig;
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct FnCallConfig;

impl CallConfig for FnCallConfig {
    type Target<P: Phase> = P::FnKey;
    type ParamConfig = FnParamConfig;
}

pub type FnCall<P> = Call<FnCallConfig, P>;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct OpCallConfig;

impl CallConfig for OpCallConfig {
    type Target<P: Phase> = P::OpKey;
    type ParamConfig = OpParamConfig;
}

pub type OpCall<P> = Call<OpCallConfig, P>;

#[derive(Clone, Debug, From)]
pub enum Arg<C: ParamConfig, P: Phase> {
    #[from]
    Expr(Expr<P>),
    OutVar(C::CanBeOutVar, OutVar<P>),
    Label(Spanned<P::LabelKey>),
}

impl<C: ParamConfig<CanBeOutVar = Yes>, P: Phase> From<OutVar<P>> for Arg<C, P> {
    fn from(out_var: OutVar<P>) -> Self {
        Arg::OutVar(Default::default(), out_var)
    }
}

pub type FnArg<P> = Arg<FnParamConfig, P>;
pub type OpArg<P> = Arg<OpParamConfig, P>;

#[derive(Clone, Debug)]
pub enum OutVar<P: Phase> {
    Out(Spanned<P::VarKey>),
    OutLet(P::LetStmtLhs),
}

#[derive(Clone, Debug)]
pub struct Body<P: Phase> {
    pub locals: Hole<P::BodyHasLocals, Locals<P>>,
    pub block: Block<P>,
}

#[derive(Clone, Debug)]
pub struct Locals<P: Phase> {
    pub local_vars: TiVec<LocalVarIndex, LocalVar<P>>,
    pub local_labels: TiVec<LocalLabelIndex, Spanned<P::LabelKey>>,
}

impl<P: Phase> Default for Locals<P> {
    fn default() -> Self {
        Locals {
            local_vars: TiVec::default(),
            local_labels: TiVec::default(),
        }
    }
}

typed_field_index!(Locals<P>:local_vars[pub LocalVarIndex] => LocalVar<P> | <P> where P: Phase);
typed_field_index!(
    Locals<P>:local_labels[pub LocalLabelIndex] => Spanned<P::LabelKey> | <P> where P: Phase
);

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum LabelIndex {
    Param(LabelParamIndex),
    Local(LocalLabelIndex),
}

deref_from!(&LabelParamIndex => LabelIndex);
deref_from!(&LocalLabelIndex => LabelIndex);

#[derive(Clone, Debug)]
pub struct LocalVar<P: Phase> {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Hole<P::LocalVarHasType, HoleSpanned<P::LocalVarTypeHasSpan, P::TypeKey>>,
}

impl<P: Phase<LocalVarHasType = Always>> Typed for LocalVar<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        (*self.type_).value
    }
}

#[derive(Clone, Debug)]
pub struct Block<P: Phase> {
    pub stmts: Vec<Stmt<P>>,
    pub value: Option<Expr<P>>,
}

impl<P: Phase<ExprHasType = Always>> Typed for Block<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        match &self.value {
            Some(value) => value.type_(),
            None => BuiltInType::Unit.into(),
        }
    }
}

#[derive(Clone, Debug, From)]
pub enum Stmt<P: Phase> {
    #[from]
    Let(LetStmt<P>),
    #[from]
    If(IfStmt<P>),
    #[from]
    Check(CheckStmt<P>),
    #[from]
    Goto(GotoStmt<P>),
    #[from]
    Emit(EmitStmt<P>),
    #[from(types("Block<P>"))]
    Expr(Expr<P>),
}

deref_from!(&GotoStmt<P> => Stmt<P> | <P> where P: Phase);

#[derive(Clone, Debug)]
pub struct LetStmt<P: Phase> {
    pub lhs: P::LetStmtLhs,
    pub rhs: Spanned<Expr<P>>,
}

#[derive(Clone, Debug)]
pub struct IfStmt<P: Phase> {
    pub cond: Spanned<Expr<P>>,
    pub then: Block<P>,
    pub else_: Option<Block<P>>,
}

#[derive(Clone, Debug)]
pub struct CheckStmt<P: Phase> {
    pub kind: CheckStmtKind,
    pub cond: Spanned<Expr<P>>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CheckStmtKind {
    Assert,
    Assume,
}

#[derive(Clone, Copy, Debug)]
pub struct GotoStmt<P: Phase> {
    pub label: Spanned<P::LabelKey>,
}

pub type EmitStmt<P> = OpCall<P>;

#[derive(Clone, Debug, From)]
pub enum Expr<P: Phase> {
    Block(Box<BlockExpr<P>>),
    Var(VarExpr<P>),
    Call(CallExpr<P>),
    Negate(Box<NegateExpr<P>>),
    Cast(Box<CastExpr<P>>),
    Compare(Box<CompareExpr<P>>),
    Assign(Box<AssignExpr<P>>),
}

impl<P: Phase<ExprHasType = Always>> Typed for Expr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
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

box_from!(BlockExpr<P> => Expr<P> | <P> where P: Phase);
box_from!(NegateExpr<P> => Expr<P> | <P> where P: Phase);
box_from!(CastExpr<P> => Expr<P> | <P> where P: Phase);
box_from!(CompareExpr<P> => Expr<P> | <P> where P: Phase);
box_from!(AssignExpr<P> => Expr<P> | <P> where P: Phase);

impl<P: Phase> From<Block<P>> for Expr<P> {
    fn from(block: Block<P>) -> Self {
        BlockExpr::from(block).into()
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr<P: Phase> {
    pub kind: Option<BlockExprKind>,
    pub block: Block<P>,
}

impl<P: Phase<ExprHasType = Always>> Typed for BlockExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        self.block.type_()
    }
}

impl<P: Phase> From<Block<P>> for BlockExpr<P> {
    fn from(block: Block<P>) -> Self {
        Self { kind: None, block }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BlockExprKind {
    Unsafe,
}

#[derive(Clone, Debug)]
pub struct VarExpr<P: Phase> {
    pub var: Spanned<P::VarKey>,
    pub type_: Hole<P::ExprHasType, P::TypeKey>,
}

impl<P: Phase<ExprHasType = Always>> Typed for VarExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        *self.type_
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr<P: Phase> {
    pub call: FnCall<P>,
    pub type_: Hole<P::ExprHasType, P::TypeKey>,
}

impl<P: Phase<ExprHasType = Always>> Typed for CallExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        *self.type_
    }
}

#[derive(Clone, Debug)]
pub struct NegateExpr<P: Phase> {
    pub kind: Spanned<NegateExprKind>,
    pub expr: Spanned<Expr<P>>,
}

impl<P: Phase<ExprHasType = Always>> Typed for NegateExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        self.expr.type_()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum NegateExprKind {
    Arithmetic,
    Logical,
}

impl Display for NegateExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                NegateExprKind::Arithmetic => "-",
                NegateExprKind::Logical => "!",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr<P: Phase> {
    pub expr: Spanned<Expr<P>>,
    pub type_: Spanned<P::TypeKey>,
}

impl<P: Phase> Typed for CastExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        self.type_.value
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr<P: Phase> {
    pub kind: Spanned<CompareExprKind>,
    pub lhs: Spanned<Expr<P>>,
    pub rhs: Spanned<Expr<P>>,
}

impl<P: Phase> Typed for CompareExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        BuiltInType::Bool.into()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CompareExprKind {
    Eq,
    Neq,
    Lte,
    Gte,
    Lt,
    Gt,
}

impl CompareExprKind {
    pub fn is_numeric(&self) -> bool {
        match self {
            CompareExprKind::Lte
            | CompareExprKind::Gte
            | CompareExprKind::Lt
            | CompareExprKind::Gt => true,
            CompareExprKind::Eq | CompareExprKind::Neq => false,
        }
    }
}

impl Display for CompareExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                CompareExprKind::Eq => "==",
                CompareExprKind::Neq => "!=",
                CompareExprKind::Lte => "<=",
                CompareExprKind::Gte => ">=",
                CompareExprKind::Lt => "<",
                CompareExprKind::Gt => ">",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub struct AssignExpr<P: Phase> {
    pub lhs: Spanned<P::VarKey>,
    pub rhs: Spanned<Expr<P>>,
}

impl<P: Phase> Typed for AssignExpr<P> {
    type Type = P::TypeKey;
    fn type_(&self) -> Self::Type {
        BuiltInType::Unit.into()
    }
}

macro_rules! export_phase_type_aliases {
    ($phase:ident) => {
        ::paste::paste! {
            pub type [<$phase CallableItem>]<C> = CallableItem<C, [<$phase Phase>]>;
            pub type [<$phase FnItem>] = FnItem<[<$phase Phase>]>;
            pub type [<$phase OpItem>] = OpItem<[<$phase Phase>]>;

            pub type [<$phase Params>]<C> = Params<C, [<$phase Phase>]>;
            pub type [<$phase FnParams>] = FnParams<[<$phase Phase>]>;
            pub type [<$phase OpParams>] = OpParams<[<$phase Phase>]>;
            pub type [<$phase VarParam>] = VarParam<[<$phase Phase>]>;
            pub type [<$phase OutVarParam>] = OutVarParam<[<$phase Phase>]>;

            pub type [<$phase Call>]<C> = Call<C, [<$phase Phase>]>;
            pub type [<$phase FnCall>] = FnCall<[<$phase Phase>]>;
            pub type [<$phase OpCall>] = OpCall<[<$phase Phase>]>;

            pub type [<$phase Arg>]<C> = Arg<C, [<$phase Phase>]>;
            pub type [<$phase FnArg>] = FnArg<[<$phase Phase>]>;
            pub type [<$phase OpArg>] = OpArg<[<$phase Phase>]>;
            pub type [<$phase OutVar>] = OutVar<[<$phase Phase>]>;

            pub type [<$phase Body>] = Body<[<$phase Phase>]>;
            pub type [<$phase Locals>] = Locals<[<$phase Phase>]>;
            pub type [<$phase LocalVar>] = LocalVar<[<$phase Phase>]>;
            pub type [<$phase Block>] = Block<[<$phase Phase>]>;

            pub type [<$phase Stmt>] = Stmt<[<$phase Phase>]>;
            pub type [<$phase LetStmt>] = LetStmt<[<$phase Phase>]>;
            pub type [<$phase IfStmt>] = IfStmt<[<$phase Phase>]>;
            pub type [<$phase CheckStmt>] = CheckStmt<[<$phase Phase>]>;
            pub type [<$phase GotoStmt>] = GotoStmt<[<$phase Phase>]>;
            pub type [<$phase EmitStmt>] = EmitStmt<[<$phase Phase>]>;

            pub type [<$phase Expr>] = Expr<[<$phase Phase>]>;
            pub type [<$phase BlockExpr>] = BlockExpr<[<$phase Phase>]>;
            pub type [<$phase VarExpr>] = VarExpr<[<$phase Phase>]>;
            pub type [<$phase CallExpr>] = CallExpr<[<$phase Phase>]>;
            pub type [<$phase NegateExpr>] = NegateExpr<[<$phase Phase>]>;
            pub type [<$phase CastExpr>] = CastExpr<[<$phase Phase>]>;
            pub type [<$phase CompareExpr>] = CompareExpr<[<$phase Phase>]>;
            pub type [<$phase AssignExpr>] = AssignExpr<[<$phase Phase>]>;
        }
    };
}
pub(crate) use export_phase_type_aliases;
