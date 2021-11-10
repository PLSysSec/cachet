// vim: set tw=99 ts=4 sts=4 sw=4 et:

// TODO(spinda): Unify AST using trait associated items and void types.

// TODO(spinda): Define own `Span` type as enum with `Internal` and `External`
// variants.

use std::fmt;

use derive_more::{AsMut, AsRef, Deref, DerefMut, From, Into};
use lazy_static::lazy_static;
use typed_index_collections::TiVec;

use crate::ast::{Ident, Path, Spanned};
use crate::util::{box_from, deref_from, typed_index};

pub const NUM_BUILT_IN_TYPES: usize = 4;
pub const NUM_BUILT_IN_VARS: usize = 3;

lazy_static! {
    pub static ref UNIT_TYPE_IDENT: Ident = Ident::from("Unit");
    pub static ref UNIT_VAR_IDENT: Ident = Ident::from("unit");
    pub static ref BOOL_TYPE_IDENT: Ident = Ident::from("Bool");
    pub static ref TRUE_VAR_IDENT: Ident = Ident::from("true");
    pub static ref FALSE_VAR_IDENT: Ident = Ident::from("false");
    pub static ref INT32_TYPE_IDENT: Ident = Ident::from("Int32");
    pub static ref DOUBLE_TYPE_IDENT: Ident = Ident::from("Double");
    pub static ref BUILT_IN_TYPE_IDENTS: [Ident; NUM_BUILT_IN_TYPES] = [
        *UNIT_TYPE_IDENT,
        *BOOL_TYPE_IDENT,
        *INT32_TYPE_IDENT,
        *DOUBLE_TYPE_IDENT,
    ];
    pub static ref BUILT_IN_VAR_IDENTS: [Ident; NUM_BUILT_IN_VARS] =
        [*UNIT_VAR_IDENT, *TRUE_VAR_IDENT, *FALSE_VAR_IDENT];
}

#[derive(Clone, Debug, From)]
pub enum Item {
    Enum(EnumItem),
    Struct(StructItem),
    Ir(IrItem),
    Impl(ImplItem),
    GlobalVar(GlobalVarItem),
    Fn(FnItem),
    Op(OpItem),
}

#[derive(Clone, Debug)]
pub struct EnumItem {
    pub ident: Spanned<Ident>,
    // TODO(spinda): Make this `variant_idents`, and `variant_paths` after name
    // resolution.
    pub variants: TiVec<VariantIndex, Spanned<Ident>>,
}

typed_index!(EnumItem:variants[VariantIndex] => Spanned<Ident>);

#[derive(Clone, Debug)]
pub struct StructItem {
    pub ident: Spanned<Ident>,
    pub supertype: Option<Spanned<Path>>,
}

#[derive(Clone, Debug)]
pub struct GlobalVarItem {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct FnItem {
    pub ident: Spanned<Ident>,
    pub is_unsafe: bool,
    pub params: Vec<Param>,
    pub ret: Option<Spanned<Path>>,
    pub body: Spanned<Option<Block>>,
}

#[derive(Clone, Debug, From)]
pub enum Param {
    #[from]
    Var(VarParam),
    #[from]
    OutVar(OutVarParam),
    Label(Spanned<Ident>),
}

#[derive(Clone, Debug)]
pub struct VarParam {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct OutVarParam {
    pub ident: Spanned<Ident>,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct ImplItem {
    pub parent: Spanned<Path>,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Clone, Debug)]
pub struct IrItem {
    pub ident: Spanned<Ident>,
    pub emits: Option<Spanned<Path>>,
    pub items: Vec<Spanned<Item>>,
}

// TODO(spinda): Expand derives.
#[derive(AsMut, AsRef, Clone, Debug, Deref, DerefMut, From, Into)]
pub struct OpItem(pub FnItem);

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
    pub lhs: LocalVar,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Option<Spanned<Path>>,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Spanned<Expr>,
    pub then: Block,
    pub else_: Option<Block>,
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckStmtKind,
    pub cond: Spanned<Expr>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CheckStmtKind {
    Assert,
    Assume,
}

#[derive(Clone, Copy, Debug)]
pub struct GotoStmt {
    pub label: Spanned<Path>,
}

// TODO(spinda): Expand derives.
#[derive(AsMut, AsRef, Clone, Debug, Deref, DerefMut, From, Into)]
pub struct EmitStmt(pub CallExpr);

#[derive(Clone, Debug, From)]
pub enum Arg {
    /// Arguments that look like `bar` in `foo(bar)` could be either a variable
    /// expression argument or a label argument. They will have to be
    /// disambiguated during name resolution.
    VarExprOrLabel(Spanned<Path>),
    #[from]
    Expr(Expr),
    #[from]
    OutVar(OutVar),
}

#[derive(Clone, Debug)]
pub enum OutVar {
    Out(Spanned<Path>),
    OutLet(LocalVar),
}

#[derive(Clone, Debug, From)]
pub enum Expr {
    Block(Box<BlockExpr>),
    Var(Spanned<Path>),
    Call(CallExpr),
    Negate(Box<NegateExpr>),
    Cast(Box<CastExpr>),
    Compare(Box<CompareExpr>),
    Assign(Box<AssignExpr>),
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

impl From<Block> for BlockExpr {
    fn from(block: Block) -> Self {
        Self { kind: None, block }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BlockExprKind {
    Unsafe,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: Spanned<Path>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
}

#[derive(Clone, Debug)]
pub struct NegateExpr {
    pub kind: Spanned<NegateExprKind>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum NegateExprKind {
    Arithmetic,
    Logical,
}

impl fmt::Display for NegateExprKind {
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
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: Spanned<CompareExprKind>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
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

impl fmt::Display for CompareExprKind {
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
pub struct AssignExpr {
    pub lhs: Spanned<Path>,
    pub rhs: Spanned<Expr>,
}
