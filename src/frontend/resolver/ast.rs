// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::{Index, IndexMut};

use crate::frontend::parser::{
    Path, BOOL_TYPE, DOUBLE_TYPE, FALSE_VAR, INT32_TYPE, TRUE_VAR, UNIT_TYPE, UNIT_VAR,
};

pub use crate::frontend::parser::{
    BlockExprKind, CheckStmtKind, CompareExprKind, EnumDef, Ident, Spanned,
};

#[derive(Clone, Debug)]
pub struct Env {
    pub enum_defs: Vec<EnumDef>,
    pub struct_defs: Vec<StructDef>,
    pub const_defs: Vec<ConstDef>,
    pub fn_defs: Vec<FnDef>,
    pub op_defs: Vec<OpDef>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInType {
    Unit,
    Bool,
    Int32,
    Double,
}

pub const NUM_BUILT_IN_TYPES: usize = 4;

pub const BUILT_IN_TYPES: [BuiltInType; NUM_BUILT_IN_TYPES] = [
    BuiltInType::Unit,
    BuiltInType::Bool,
    BuiltInType::Int32,
    BuiltInType::Double,
];

impl BuiltInType {
    pub const fn index(&self) -> usize {
        match self {
            BuiltInType::Unit => 0,
            BuiltInType::Bool => 1,
            BuiltInType::Int32 => 2,
            BuiltInType::Double => 3,
        }
    }

    pub const fn ident(&self) -> &'static str {
        match self {
            BuiltInType::Unit => UNIT_TYPE,
            BuiltInType::Bool => BOOL_TYPE,
            BuiltInType::Int32 => INT32_TYPE,
            BuiltInType::Double => DOUBLE_TYPE,
        }
    }

    pub const fn subtype_of(&self) -> Option<BuiltInType> {
        match self {
            BuiltInType::Unit => None,
            BuiltInType::Bool => Some(BuiltInType::Int32),
            BuiltInType::Int32 => None,
            BuiltInType::Double => None,
        }
    }

    pub const fn is_numeric(&self) -> bool {
        match self {
            BuiltInType::Bool | BuiltInType::Int32 | BuiltInType::Double => true,
            BuiltInType::Unit => false,
        }
    }

    pub fn from_ident(ident: &str) -> Option<BuiltInType> {
        match ident {
            UNIT_TYPE => Some(BuiltInType::Unit),
            BOOL_TYPE => Some(BuiltInType::Bool),
            INT32_TYPE => Some(BuiltInType::Int32),
            DOUBLE_TYPE => Some(BuiltInType::Double),
            _ => None,
        }
    }

    pub fn from_path(path: &Path) -> Option<BuiltInType> {
        match path.parent_type {
            Some(_) => None,
            None => BuiltInType::from_ident(&path.ident),
        }
    }
}

impl From<BuiltInType> for TypeIndex {
    fn from(built_in_type: BuiltInType) -> Self {
        TypeIndex::BuiltIn(built_in_type)
    }
}

impl From<&BuiltInType> for TypeIndex {
    fn from(built_in_type: &BuiltInType) -> Self {
        (*built_in_type).into()
    }
}

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

pub type EnumIndex = usize;
pub type VariantIndex = usize;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct EnumVariantIndex {
    pub enum_index: EnumIndex,
    pub variant_index: VariantIndex,
}

impl From<EnumVariantIndex> for VarIndex {
    fn from(index: EnumVariantIndex) -> Self {
        VarIndex::EnumVariant(index)
    }
}

impl From<&EnumVariantIndex> for VarIndex {
    fn from(index: &EnumVariantIndex) -> Self {
        (*index).into()
    }
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub ident: Spanned<Ident>,
    pub subtype_of: Option<TypeIndex>,
}

pub type StructIndex = usize;

#[derive(Clone, Debug)]
pub struct ConstDef {
    pub ident: Spanned<Ident>,
    pub parent_type: Option<TypeIndex>,
    pub type_: TypeIndex,
}

pub type ConstIndex = usize;

#[derive(Clone, Debug)]
pub struct FnDef {
    pub sig: Sig,
    pub parent_type: Option<TypeIndex>,
    pub is_unsafe: bool,
    pub body: Option<Body>,
}

pub type FnIndex = usize;

#[derive(Clone, Debug)]
pub struct OpDef {
    pub sig: Sig,
    pub body: Body,
}

pub type OpIndex = usize;

#[derive(Clone, Debug)]
pub struct Sig {
    pub ident: Spanned<Ident>,
    pub is_fallible: bool,
    pub param_vars: Vec<ParamVar>,
    pub ret: TypeIndex,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuiltInVar {
    Unit,
    True,
    False,
}

pub const NUM_BUILT_IN_VARS: usize = 3;

pub const BUILT_IN_VARS: [BuiltInVar; NUM_BUILT_IN_VARS] =
    [BuiltInVar::Unit, BuiltInVar::True, BuiltInVar::False];

impl BuiltInVar {
    pub const fn index(&self) -> usize {
        match self {
            BuiltInVar::Unit => 0,
            BuiltInVar::True => 1,
            BuiltInVar::False => 2,
        }
    }

    pub const fn ident(&self) -> &'static str {
        match self {
            BuiltInVar::Unit => UNIT_VAR,
            BuiltInVar::True => TRUE_VAR,
            BuiltInVar::False => FALSE_VAR,
        }
    }

    pub const fn built_in_type(&self) -> BuiltInType {
        match self {
            BuiltInVar::Unit => BuiltInType::Unit,
            BuiltInVar::True => BuiltInType::Bool,
            BuiltInVar::False => BuiltInType::Bool,
        }
    }

    pub fn from_path(path: &Path) -> Option<BuiltInVar> {
        match path.parent_type {
            Some(_) => None,
            None => BuiltInVar::from_ident(&path.ident),
        }
    }

    pub fn from_ident(ident: impl AsRef<str>) -> Option<BuiltInVar> {
        match ident.as_ref() {
            UNIT_VAR => Some(BuiltInVar::Unit),
            TRUE_VAR => Some(BuiltInVar::True),
            FALSE_VAR => Some(BuiltInVar::False),
            _ => None,
        }
    }
}

impl From<BuiltInVar> for VarIndex {
    fn from(built_in_var: BuiltInVar) -> Self {
        VarIndex::BuiltIn(built_in_var)
    }
}

impl From<&BuiltInVar> for VarIndex {
    fn from(built_in_var: &BuiltInVar) -> Self {
        (*built_in_var).into()
    }
}

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

#[derive(Clone, Debug)]
pub struct ParamVar {
    pub ident: Spanned<Ident>,
    pub is_out: bool,
    pub type_: TypeIndex,
}

pub type ParamVarIndex = usize;

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Ident,
    pub type_: Option<TypeIndex>,
}

pub type LocalVarIndex = usize;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ScopedVarIndex {
    ParamVar(ParamVarIndex),
    LocalVar(LocalVarIndex),
}

impl From<ScopedVarIndex> for VarIndex {
    fn from(index: ScopedVarIndex) -> Self {
        VarIndex::ScopedVar(index)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum VarIndex {
    BuiltIn(BuiltInVar),
    EnumVariant(EnumVariantIndex),
    Const(ConstIndex),
    ScopedVar(ScopedVarIndex),
}

#[derive(Clone, Debug)]
pub struct Body {
    pub local_vars: Vec<LocalVar>,
    pub block: Spanned<Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub value: Option<Expr>,
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
    pub rhs: Spanned<Expr>,
}

impl From<LetStmt> for Stmt {
    fn from(let_stmt: LetStmt) -> Self {
        Stmt::Let(let_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckStmtKind,
    pub cond: Spanned<Expr>,
}

impl From<CheckStmt> for Stmt {
    fn from(check_stmt: CheckStmt) -> Self {
        Stmt::Check(check_stmt)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Block(Box<BlockExpr>),
    Var(Spanned<VarIndex>),
    Call(CallExpr),
    Cast(Box<CastExpr>),
    Compare(Box<CompareExpr>),
    Assign(Box<AssignExpr>),
}

impl From<Spanned<VarIndex>> for Expr {
    fn from(index: Spanned<VarIndex>) -> Self {
        Expr::Var(index)
    }
}

impl From<Spanned<BuiltInVar>> for Expr {
    fn from(built_in_var: Spanned<BuiltInVar>) -> Self {
        Expr::Var(built_in_var.map(VarIndex::from))
    }
}

impl From<Spanned<&BuiltInVar>> for Expr {
    fn from(built_in_var: Spanned<&BuiltInVar>) -> Self {
        Expr::Var(built_in_var.map(VarIndex::from))
    }
}

impl From<Spanned<EnumVariantIndex>> for Expr {
    fn from(enum_variant_index: Spanned<EnumVariantIndex>) -> Self {
        Expr::Var(enum_variant_index.map(VarIndex::from))
    }
}

impl From<Spanned<&EnumVariantIndex>> for Expr {
    fn from(enum_variant_index: Spanned<&EnumVariantIndex>) -> Self {
        Expr::Var(enum_variant_index.map(VarIndex::from))
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
pub struct CallExpr {
    pub target: Spanned<FnIndex>,
    pub args: Spanned<Vec<CallExprArg>>,
}

impl From<CallExpr> for Expr {
    fn from(call_expr: CallExpr) -> Self {
        Expr::Call(call_expr)
    }
}

#[derive(Clone, Debug)]
pub enum CallExprArg {
    Expr(Spanned<Expr>),
    OutRef(Spanned<ScopedVarIndex>),
}

impl From<Spanned<Expr>> for CallExprArg {
    fn from(expr: Spanned<Expr>) -> Self {
        CallExprArg::Expr(expr)
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<TypeIndex>,
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

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: Spanned<CompareExprKind>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
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
    pub lhs: Spanned<ParamVarIndex>,
    pub rhs: Spanned<Expr>,
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
