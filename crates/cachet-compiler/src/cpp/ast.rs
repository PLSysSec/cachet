// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::iter::FromIterator;

pub use cachet_lang::parser::{CompareExprKind, Path};

#[derive(Clone, Debug)]
pub struct Code<Ident = String> {
    pub defs: Vec<Def<Ident>>,
}

impl<Ident> From<Vec<Def<Ident>>> for Code<Ident> {
    fn from(defs: Vec<Def<Ident>>) -> Self {
        Code { defs }
    }
}

impl<Ident> FromIterator<Def<Ident>> for Code<Ident> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Def<Ident>>,
    {
        Code {
            defs: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Def<Ident = String> {
    Comment(CommentDef),
    Namespace(NamespaceDef<Ident>),
    Fn(FnDef<Ident>),
}

#[derive(Clone, Debug)]
pub struct CommentDef {
    pub text: String,
}

impl<Ident> From<CommentDef> for Def<Ident> {
    fn from(comment_def: CommentDef) -> Self {
        Def::Comment(comment_def)
    }
}

#[derive(Clone, Debug)]
pub struct NamespaceDef<Ident = String> {
    pub ident: Ident,
    pub defs: Vec<Def<Ident>>,
}

impl<Ident> From<NamespaceDef<Ident>> for Def<Ident> {
    fn from(namespace_def: NamespaceDef<Ident>) -> Self {
        Def::Namespace(namespace_def)
    }
}

#[derive(Clone, Debug)]
pub struct FnDef<Ident = String> {
    pub path: Path,
    pub is_inline: bool,
    pub params: Vec<Param<Ident>>,
    pub ret: Type<Ident>,
    pub body: Option<Block<Ident>>,
}

impl<Ident> From<FnDef<Ident>> for Def<Ident> {
    fn from(fn_def: FnDef<Ident>) -> Self {
        Def::Fn(fn_def)
    }
}

#[derive(Clone, Debug)]
pub struct Param<Ident = String> {
    pub ident: Ident,
    pub type_: Type<Ident>,
}

#[derive(Clone, Debug)]
pub enum Type<Ident = String> {
    Void,
    Path(Path<Ident>),
    Template(Box<TemplateType<Ident>>),
    Const(Box<ConstType<Ident>>),
    Ref(Box<RefType<Ident>>),
}

impl<Ident> From<Path<Ident>> for Type<Ident> {
    fn from(path: Path<Ident>) -> Self {
        Type::Path(path)
    }
}

#[derive(Clone, Debug)]
pub struct TemplateType<Ident = String> {
    pub inner: Type<Ident>,
    pub args: Vec<Type<Ident>>,
}

impl<Ident> From<Box<TemplateType<Ident>>> for Type<Ident> {
    fn from(template_type: Box<TemplateType<Ident>>) -> Self {
        Type::Template(template_type)
    }
}

impl<Ident> From<TemplateType<Ident>> for Type<Ident> {
    fn from(template_type: TemplateType<Ident>) -> Self {
        Box::new(template_type).into()
    }
}

#[derive(Clone, Debug)]
pub struct ConstType<Ident = String> {
    pub inner: Type<Ident>,
}

impl<Ident> From<Box<ConstType<Ident>>> for Type<Ident> {
    fn from(const_type: Box<ConstType<Ident>>) -> Self {
        Type::Const(const_type)
    }
}

impl<Ident> From<ConstType<Ident>> for Type<Ident> {
    fn from(const_type: ConstType<Ident>) -> Self {
        Box::new(const_type).into()
    }
}

#[derive(Clone, Debug)]
pub struct RefType<Ident = String> {
    pub inner: Type<Ident>,
    pub value_category: ValueCategory,
}

impl<Ident> From<Box<RefType<Ident>>> for Type<Ident> {
    fn from(ref_type: Box<RefType<Ident>>) -> Self {
        Type::Ref(ref_type)
    }
}

impl<Ident> From<RefType<Ident>> for Type<Ident> {
    fn from(ref_type: RefType<Ident>) -> Self {
        Box::new(ref_type).into()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ValueCategory {
    LValue,
    RValue,
}

#[derive(Clone, Debug)]
pub struct Block<Ident = String> {
    pub stmts: Vec<Stmt<Ident>>,
}

impl<Ident> From<Vec<Stmt<Ident>>> for Block<Ident> {
    fn from(stmts: Vec<Stmt<Ident>>) -> Self {
        Block { stmts }
    }
}

impl<Ident> FromIterator<Stmt<Ident>> for Block<Ident> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt<Ident>>,
    {
        Block {
            stmts: iter.into_iter().collect(),
        }
    }
}

impl<Ident> From<Block<Ident>> for Expr<Ident> {
    fn from(block: Block<Ident>) -> Self {
        Expr::Block(block.into())
    }
}

impl<Ident> From<Block<Ident>> for Stmt<Ident> {
    fn from(block: Block<Ident>) -> Self {
        Expr::from(block).into()
    }
}

#[derive(Clone, Debug)]
pub enum Stmt<Ident = String> {
    Expr(ExprStmt<Ident>),
    Ret(RetStmt<Ident>),
    Var(VarStmt<Ident>),
    If(IfStmt<Ident>),
}

impl<Ident> From<Vec<Stmt<Ident>>> for Stmt<Ident> {
    fn from(stmts: Vec<Stmt<Ident>>) -> Self {
        Block::from(stmts).into()
    }
}

impl<Ident> FromIterator<Stmt<Ident>> for Stmt<Ident> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt<Ident>>,
    {
        Block::from_iter(iter).into()
    }
}

#[derive(Clone, Debug)]
pub struct ExprStmt<Ident = String> {
    pub expr: Expr<Ident>,
}

impl<Ident> From<Expr<Ident>> for ExprStmt<Ident> {
    fn from(expr: Expr<Ident>) -> Self {
        ExprStmt { expr }
    }
}

impl<Ident> From<ExprStmt<Ident>> for Stmt<Ident> {
    fn from(expr_stmt: ExprStmt<Ident>) -> Self {
        Stmt::Expr(expr_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct RetStmt<Ident = String> {
    pub value: Expr<Ident>,
}

impl<Ident> From<Expr<Ident>> for RetStmt<Ident> {
    fn from(value: Expr<Ident>) -> Self {
        RetStmt { value }
    }
}

impl<Ident> From<RetStmt<Ident>> for Stmt<Ident> {
    fn from(ret_stmt: RetStmt<Ident>) -> Self {
        Stmt::Ret(ret_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct VarStmt<Ident = String> {
    pub ident: Ident,
    pub type_: Type<Ident>,
    pub init: Option<Expr<Ident>>,
}

impl<Ident> From<VarStmt<Ident>> for Stmt<Ident> {
    fn from(var_stmt: VarStmt<Ident>) -> Self {
        Stmt::Var(var_stmt)
    }
}

#[derive(Clone, Debug)]
pub struct IfStmt<Ident = String> {
    pub cond: Expr<Ident>,
    pub body: Block<Ident>,
}

impl<Ident> From<IfStmt<Ident>> for Stmt<Ident> {
    fn from(if_stmt: IfStmt<Ident>) -> Self {
        Stmt::If(if_stmt)
    }
}

#[derive(Clone, Debug)]
pub enum Expr<Ident = String> {
    Block(BlockExpr<Ident>),
    Path(Path<Ident>),
    Template(Box<TemplateExpr<Ident>>),
    Member(Box<MemberExpr<Ident>>),
    Call(Box<CallExpr<Ident>>),
    Cast(Box<CastExpr<Ident>>),
    Unary(Box<UnaryExpr<Ident>>),
    Compare(Box<CompareExpr<Ident>>),
    Assign(Box<AssignExpr<Ident>>),
    Comma(Box<CommaExpr<Ident>>),
}

impl<Ident> From<Path<Ident>> for Expr<Ident> {
    fn from(path: Path<Ident>) -> Self {
        Expr::Path(path)
    }
}

impl<Ident> From<Expr<Ident>> for Stmt<Ident> {
    fn from(expr: Expr<Ident>) -> Self {
        ExprStmt::from(expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr<Ident = String> {
    pub block: Block<Ident>,
}

impl<Ident> From<Block<Ident>> for BlockExpr<Ident> {
    fn from(block: Block<Ident>) -> Self {
        BlockExpr { block }
    }
}

impl<Ident> From<BlockExpr<Ident>> for Expr<Ident> {
    fn from(block_expr: BlockExpr<Ident>) -> Self {
        Expr::Block(block_expr)
    }
}

#[derive(Clone, Debug)]
pub struct TemplateExpr<Ident = String> {
    pub inner: Expr<Ident>,
    pub args: Vec<Type<Ident>>,
}

impl<Ident> From<Box<TemplateExpr<Ident>>> for Expr<Ident> {
    fn from(template_expr: Box<TemplateExpr<Ident>>) -> Self {
        Expr::Template(template_expr)
    }
}

impl<Ident> From<TemplateExpr<Ident>> for Expr<Ident> {
    fn from(template_expr: TemplateExpr<Ident>) -> Self {
        Box::new(template_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct MemberExpr<Ident = String> {
    pub parent: Expr<Ident>,
    pub member: Ident,
}

impl<Ident> From<Box<MemberExpr<Ident>>> for Expr<Ident> {
    fn from(member_expr: Box<MemberExpr<Ident>>) -> Self {
        Expr::Member(member_expr)
    }
}

impl<Ident> From<MemberExpr<Ident>> for Expr<Ident> {
    fn from(member_expr: MemberExpr<Ident>) -> Self {
        Box::new(member_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CallExpr<Ident = String> {
    pub target: Expr<Ident>,
    pub args: Vec<Expr<Ident>>,
}

impl<Ident> From<Box<CallExpr<Ident>>> for Expr<Ident> {
    fn from(call_expr: Box<CallExpr<Ident>>) -> Self {
        Expr::Call(call_expr)
    }
}

impl<Ident> From<CallExpr<Ident>> for Expr<Ident> {
    fn from(call_expr: CallExpr<Ident>) -> Self {
        Box::new(call_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr<Ident = String> {
    pub kind: CastExprKind,
    pub expr: Expr<Ident>,
    pub type_: Type<Ident>,
}

impl<Ident> From<Box<CastExpr<Ident>>> for Expr<Ident> {
    fn from(cast_expr: Box<CastExpr<Ident>>) -> Self {
        Expr::Cast(cast_expr)
    }
}

impl<Ident> From<CastExpr<Ident>> for Expr<Ident> {
    fn from(cast_expr: CastExpr<Ident>) -> Self {
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
pub struct UnaryExpr<Ident = String> {
    pub kind: UnaryExprKind,
    pub inner: Expr<Ident>,
}

impl<Ident> From<Box<UnaryExpr<Ident>>> for Expr<Ident> {
    fn from(unary_expr: Box<UnaryExpr<Ident>>) -> Self {
        Expr::Unary(unary_expr)
    }
}

impl<Ident> From<UnaryExpr<Ident>> for Expr<Ident> {
    fn from(unary_expr: UnaryExpr<Ident>) -> Self {
        Box::new(unary_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CompareExpr<Ident = String> {
    pub kind: CompareExprKind,
    pub lhs: Expr<Ident>,
    pub rhs: Expr<Ident>,
}

impl<Ident> From<Box<CompareExpr<Ident>>> for Expr<Ident> {
    fn from(compare_expr: Box<CompareExpr<Ident>>) -> Self {
        Expr::Compare(compare_expr)
    }
}

impl<Ident> From<CompareExpr<Ident>> for Expr<Ident> {
    fn from(compare_expr: CompareExpr<Ident>) -> Self {
        Box::new(compare_expr).into()
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryExprKind {
    BoolNot,
}

#[derive(Clone, Debug)]
pub struct AssignExpr<Ident = String> {
    pub lhs: Expr<Ident>,
    pub rhs: Expr<Ident>,
}

impl<Ident> From<Box<AssignExpr<Ident>>> for Expr<Ident> {
    fn from(assign_expr: Box<AssignExpr<Ident>>) -> Self {
        Expr::Assign(assign_expr)
    }
}

impl<Ident> From<AssignExpr<Ident>> for Expr<Ident> {
    fn from(assign_expr: AssignExpr<Ident>) -> Self {
        Box::new(assign_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CommaExpr<Ident = String> {
    pub lhs: Expr<Ident>,
    pub rhs: Expr<Ident>,
}

impl<Ident> From<Box<CommaExpr<Ident>>> for Expr<Ident> {
    fn from(comma_expr: Box<CommaExpr<Ident>>) -> Self {
        Expr::Comma(comma_expr)
    }
}

impl<Ident> From<CommaExpr<Ident>> for Expr<Ident> {
    fn from(comma_expr: CommaExpr<Ident>) -> Self {
        Box::new(comma_expr).into()
    }
}
