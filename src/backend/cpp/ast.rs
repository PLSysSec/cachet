// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::iter::FromIterator;

pub use crate::frontend::parser::{CompareExprKind, Ident, Path};

#[derive(Clone, Debug)]
pub struct Code {
    pub defs: Vec<Def>,
}

impl From<Vec<Def>> for Code {
    fn from(defs: Vec<Def>) -> Self {
        Code { defs }
    }
}

impl FromIterator<Def> for Code {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Def>,
    {
        Code {
            defs: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Def {
    Comment(CommentDef),
    Namespace(NamespaceDef),
    Fn(FnDef),
}

#[derive(Clone, Debug)]
pub struct CommentDef {
    pub text: String,
}

impl From<CommentDef> for Def {
    fn from(comment_def: CommentDef) -> Self {
        Def::Comment(comment_def)
    }
}

#[derive(Clone, Debug)]
pub struct NamespaceDef {
    pub ident: Ident,
    pub defs: Vec<Def>,
}

impl From<NamespaceDef> for Def {
    fn from(namespace_def: NamespaceDef) -> Self {
        Def::Namespace(namespace_def)
    }
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub path: Path,
    pub is_inline: bool,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Option<Block>,
}

impl From<FnDef> for Def {
    fn from(fn_def: FnDef) -> Self {
        Def::Fn(fn_def)
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub ident: Ident,
    pub type_: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    Void,
    Path(Path),
    Template(Box<TemplateType>),
    Const(Box<ConstType>),
    Ref(Box<RefType>),
}

impl From<Path> for Type {
    fn from(path: Path) -> Self {
        Type::Path(path)
    }
}

#[derive(Clone, Debug)]
pub struct TemplateType {
    pub inner: Type,
    pub args: Vec<Type>,
}

impl From<Box<TemplateType>> for Type {
    fn from(template_type: Box<TemplateType>) -> Self {
        Type::Template(template_type)
    }
}

impl From<TemplateType> for Type {
    fn from(template_type: TemplateType) -> Self {
        Box::new(template_type).into()
    }
}

#[derive(Clone, Debug)]
pub struct ConstType {
    pub inner: Type,
}

impl From<Box<ConstType>> for Type {
    fn from(const_type: Box<ConstType>) -> Self {
        Type::Const(const_type)
    }
}

impl From<ConstType> for Type {
    fn from(const_type: ConstType) -> Self {
        Box::new(const_type).into()
    }
}

#[derive(Clone, Debug)]
pub struct RefType {
    pub inner: Type,
    pub value_category: ValueCategory,
}

impl From<Box<RefType>> for Type {
    fn from(ref_type: Box<RefType>) -> Self {
        Type::Ref(ref_type)
    }
}

impl From<RefType> for Type {
    fn from(ref_type: RefType) -> Self {
        Box::new(ref_type).into()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ValueCategory {
    LValue,
    RValue,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl From<Vec<Stmt>> for Block {
    fn from(stmts: Vec<Stmt>) -> Self {
        Block { stmts }
    }
}

impl FromIterator<Stmt> for Block {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        Block {
            stmts: iter.into_iter().collect(),
        }
    }
}

impl From<Block> for Expr {
    fn from(block: Block) -> Self {
        Expr::Block(block.into())
    }
}

impl From<Block> for Stmt {
    fn from(block: Block) -> Self {
        Expr::from(block).into()
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Ret(RetStmt),
    Var(VarStmt),
    If(IfStmt),
}

impl From<Vec<Stmt>> for Stmt {
    fn from(stmts: Vec<Stmt>) -> Self {
        Block::from(stmts).into()
    }
}

impl FromIterator<Stmt> for Stmt {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        Block::from_iter(iter).into()
    }
}

#[derive(Clone, Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

impl From<Expr> for ExprStmt {
    fn from(expr: Expr) -> Self {
        ExprStmt { expr }
    }
}

impl From<ExprStmt> for Stmt {
    fn from(expr_stmt: ExprStmt) -> Self {
        Stmt::Expr(expr_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct RetStmt {
    pub value: Expr,
}

impl From<Expr> for RetStmt {
    fn from(value: Expr) -> Self {
        RetStmt { value }
    }
}

impl From<RetStmt> for Stmt {
    fn from(ret_stmt: RetStmt) -> Self {
        Stmt::Ret(ret_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct VarStmt {
    pub ident: Ident,
    pub type_: Type,
    pub init: Option<Expr>,
}

impl From<VarStmt> for Stmt {
    fn from(var_stmt: VarStmt) -> Self {
        Stmt::Var(var_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub body: Block,
}

impl From<IfStmt> for Stmt {
    fn from(if_stmt: IfStmt) -> Self {
        Stmt::If(if_stmt)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Block(BlockExpr),
    Path(Path),
    Template(Box<TemplateExpr>),
    Member(Box<MemberExpr>),
    Call(Box<CallExpr>),
    Cast(Box<CastExpr>),
    Unary(Box<UnaryExpr>),
    Compare(Box<CompareExpr>),
    Assign(Box<AssignExpr>),
    Comma(Box<CommaExpr>),
}

impl From<Path> for Expr {
    fn from(path: Path) -> Self {
        Expr::Path(path)
    }
}

impl From<Expr> for Stmt {
    fn from(expr: Expr) -> Self {
        ExprStmt::from(expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub block: Block,
}

impl From<Block> for BlockExpr {
    fn from(block: Block) -> Self {
        BlockExpr { block }
    }
}

impl From<BlockExpr> for Expr {
    fn from(block_expr: BlockExpr) -> Self {
        Expr::Block(block_expr)
    }
}

#[derive(Clone, Debug)]
pub struct TemplateExpr {
    pub inner: Expr,
    pub args: Vec<Type>,
}

impl From<Box<TemplateExpr>> for Expr {
    fn from(template_expr: Box<TemplateExpr>) -> Self {
        Expr::Template(template_expr)
    }
}

impl From<TemplateExpr> for Expr {
    fn from(template_expr: TemplateExpr) -> Self {
        Box::new(template_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct MemberExpr {
    pub parent: Expr,
    pub member: Ident,
}

impl From<Box<MemberExpr>> for Expr {
    fn from(member_expr: Box<MemberExpr>) -> Self {
        Expr::Member(member_expr)
    }
}

impl From<MemberExpr> for Expr {
    fn from(member_expr: MemberExpr) -> Self {
        Box::new(member_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: Expr,
    pub args: Vec<Expr>,
}

impl From<Box<CallExpr>> for Expr {
    fn from(call_expr: Box<CallExpr>) -> Self {
        Expr::Call(call_expr)
    }
}

impl From<CallExpr> for Expr {
    fn from(call_expr: CallExpr) -> Self {
        Box::new(call_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub kind: CastExprKind,
    pub expr: Expr,
    pub type_: Type,
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
    Functional(FunctionalCastExprKind), 
    CStyle,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FunctionalCastExprKind {
    Static,
    Dynamic,
    Const,
    Reinterpret,
}

impl From<FunctionalCastExprKind> for CastExprKind {
    fn from(kind: FunctionalCastExprKind) -> Self {
        CastExprKind::Functional(kind)
    }
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub kind: UnaryExprKind,
    pub inner: Expr,
}

impl From<Box<UnaryExpr>> for Expr {
    fn from(unary_expr: Box<UnaryExpr>) -> Self {
        Expr::Unary(unary_expr)
    }
}

impl From<UnaryExpr> for Expr {
    fn from(unary_expr: UnaryExpr) -> Self {
        Box::new(unary_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: CompareExprKind,
    pub lhs: Expr,
    pub rhs: Expr,
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryExprKind {
    BoolNot,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Expr,
    pub rhs: Expr,
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

#[derive(Clone, Debug)]
pub struct CommaExpr {
    pub lhs: Expr,
    pub rhs: Expr,
}

impl From<Box<CommaExpr>> for Expr {
    fn from(comma_expr: Box<CommaExpr>) -> Self {
        Expr::Comma(comma_expr)
    }
}

impl From<CommaExpr> for Expr {
    fn from(comma_expr: CommaExpr) -> Self {
        Box::new(comma_expr).into()
    }
}
