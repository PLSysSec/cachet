// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt;

use codespan::Span;

#[derive(Clone, Copy, Debug)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub const fn new(span: Span, value: T) -> Self {
        Spanned { span, value }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }

    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            span: self.span,
            value: &self.value,
        }
    }
}

impl<T: Clone> Spanned<&T> {
    pub fn cloned(self) -> Spanned<T> {
        self.map(|value| value.clone())
    }
}

impl<T: Copy> Spanned<&T> {
    pub fn copied(self) -> Spanned<T> {
        self.map(|value| *value)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.value, f)
    }
}

pub type Ident = String;

pub const UNIT_TYPE: &'static str = "Unit";
pub const UNIT_VAR: &'static str = "unit";

pub const BOOL_TYPE: &'static str = "Bool";
pub const TRUE_VAR: &'static str = "true";
pub const FALSE_VAR: &'static str = "false";

pub const INT32_TYPE: &'static str = "Int32";
pub const DOUBLE_TYPE: &'static str = "Double";

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Path<T = Ident> {
    pub parent_type: Option<T>,
    pub ident: T,
}

impl<T> Path<T> {
    pub fn new(parent_type: Option<T>, ident: T) -> Self {
        Path { parent_type, ident }
    }

    pub fn from_ident(ident: T) -> Self {
        Path {
            parent_type: None,
            ident,
        }
    }
}

impl<T: ToOwned + ?Sized> Path<&T> {
    pub fn to_owned(&self) -> Path<T::Owned> {
        Path {
            parent_type: self.parent_type.map(ToOwned::to_owned),
            ident: self.ident.to_owned(),
        }
    }
}

impl From<Spanned<Path>> for Expr {
    fn from(path: Spanned<Path>) -> Self {
        Expr::Var(path)
    }
}

impl<T: fmt::Display> fmt::Display for Path<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(parent_type) = &self.parent_type {
            write!(f, "{}::", parent_type)?;
        }
        write!(f, "{}", self.ident)
    }
}

pub type Spec = Vec<TopDef>;

#[derive(Clone, Debug)]
pub enum TopDef {
    Enum(EnumDef),
    Struct(StructDef),
    Impl(ImplDef),
    Op(OpDef),
    Def(Def),
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub ident: Spanned<Ident>,
    pub variants: Vec<Spanned<Ident>>,
}

impl From<EnumDef> for TopDef {
    fn from(enum_def: EnumDef) -> Self {
        TopDef::Enum(enum_def)
    }
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub ident: Spanned<Ident>,
    pub subtype_of: Option<Spanned<Path>>,
}

impl From<StructDef> for TopDef {
    fn from(struct_def: StructDef) -> Self {
        TopDef::Struct(struct_def)
    }
}

#[derive(Clone, Debug)]
pub struct ImplDef {
    pub parent_type: Spanned<Path>,
    pub defs: Vec<Def>,
}

impl From<ImplDef> for TopDef {
    fn from(impl_def: ImplDef) -> Self {
        TopDef::Impl(impl_def)
    }
}

#[derive(Clone, Debug)]
pub enum Def {
    Const(ConstDef),
    Fn(FnDef),
}

impl From<Def> for TopDef {
    fn from(def: Def) -> Self {
        TopDef::Def(def)
    }
}

#[derive(Clone, Debug)]
pub struct ConstDef {
    pub ident: Spanned<Ident>,
    pub type_: Spanned<Path>,
}

impl From<ConstDef> for Def {
    fn from(const_def: ConstDef) -> Self {
        Def::Const(const_def)
    }
}

impl From<ConstDef> for TopDef {
    fn from(const_def: ConstDef) -> Self {
        Def::from(const_def).into()
    }
}

#[derive(Clone, Debug)]
pub struct FnDef {
    pub sig: Sig,
    pub is_unsafe: bool,
    pub body: Option<Spanned<Block>>,
}

impl From<FnDef> for Def {
    fn from(fn_def: FnDef) -> Self {
        Def::Fn(fn_def)
    }
}

impl From<FnDef> for TopDef {
    fn from(fn_def: FnDef) -> Self {
        Def::from(fn_def).into()
    }
}

#[derive(Clone, Debug)]
pub struct OpDef {
    pub sig: Sig,
    pub body: Spanned<Block>,
}

impl From<OpDef> for TopDef {
    fn from(op_def: OpDef) -> Self {
        TopDef::Op(op_def)
    }
}

#[derive(Clone, Debug)]
pub struct Sig {
    pub ident: Spanned<Ident>,
    pub is_fallible: bool,
    pub param_vars: Vec<ParamVar>,
    pub ret: Option<Spanned<Path>>,
}

#[derive(Clone, Debug)]
pub struct ParamVar {
    pub ident: Spanned<Ident>,
    pub is_out: bool,
    pub type_: Spanned<Path>,
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
    pub lhs: Spanned<Ident>,
    pub rhs: Spanned<Expr>,
    pub type_: Option<Spanned<Path>>,
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CheckStmtKind {
    Assert,
    Guard,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Block(Box<BlockExpr>),
    Var(Spanned<Path>),
    Call(CallExpr),
    Cast(Box<CastExpr>),
    Compare(Box<CompareExpr>),
    Assign(Box<AssignExpr>),
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BlockExprKind {
    Fallible,
    Unsafe,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: Spanned<Path>,
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
    OutRef(Spanned<Path>),
    LetOutRef(LetOutRef),
}

impl From<Spanned<Expr>> for CallExprArg {
    fn from(expr: Spanned<Expr>) -> Self {
        CallExprArg::Expr(expr)
    }
}

#[derive(Clone, Debug)]
pub struct LetOutRef {
    pub ident: Spanned<Ident>,
    pub type_: Option<Spanned<Path>>,
}

impl From<LetOutRef> for CallExprArg {
    fn from(let_out_ref: LetOutRef) -> Self {
        CallExprArg::LetOutRef(let_out_ref)
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<Path>,
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
