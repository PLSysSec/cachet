// vim: set tw=99 ts=4 sts=4 sw=4 et:

use derive_more::From;
use typed_index_collections::TiVec;

use cachet_util::{box_from, deref_from, typed_field_index};

use crate::ast::{
    BlockKind, CheckKind, CompareKind, Ident, NegateKind, Path, Spanned, VarParamKind,
};

#[derive(Clone, Debug, From)]
pub enum Item {
    #[from]
    Enum(EnumItem),
    #[from]
    Struct(StructItem),
    #[from]
    Ir(IrItem),
    #[from]
    Impl(ImplItem),
    #[from]
    GlobalVar(GlobalVarItem),
    Fn(CallableItem),
    Op(CallableItem),
}

#[derive(Clone, Debug)]
pub struct EnumItem {
    pub ident: Spanned<Ident>,
    pub variants: TiVec<VariantIndex, Spanned<Ident>>,
}

typed_field_index!(EnumItem:variants[pub VariantIndex] => Spanned<Ident>);

#[derive(Clone, Debug)]
pub struct StructItem {
    pub ident: Spanned<Ident>,
    pub supertype: Option<Spanned<Path>>,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub ident: Spanned<Ident>,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct IrItem {
    pub ident: Spanned<Ident>,
    pub emits: Option<Spanned<Path>>,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Clone, Debug)]
pub struct ImplItem {
    pub parent: Spanned<Path>,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Clone, Debug)]
pub struct GlobalVarItem {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct CallableItem {
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
    #[from(types(Label))]
    Label(LabelParam),
}

#[derive(Clone, Debug)]
pub struct VarParam {
    pub ident: Spanned<Ident>,
    pub kind: VarParamKind,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct LabelParam {
    pub label: Label,
    pub is_out: bool,
}

impl From<Label> for LabelParam {
    fn from(label: Label) -> Self {
        Self {
            label,
            is_out: false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Label {
    pub ident: Spanned<Ident>,
    pub ir: Spanned<Path>,
}

#[derive(Clone, Debug, From)]
pub enum Arg {
    #[from]
    Expr(Expr),
    /// Arguments that look like `bar` in `foo(bar)` could be either a variable
    /// expression argument or a label argument. The same is true of arguments
    /// that look like `out bar)` in `foo(out bar)`. They will have to be
    /// disambiguated during name resolution.
    #[from]
    FreeVarOrLabel(FreeVarOrLabelArg),
    #[from]
    OutFreshVar(LocalVar),
    #[from]
    OutFreshLabel(LocalLabel),
}

#[derive(Clone, Debug)]
pub struct FreeVarOrLabelArg {
    pub path: Spanned<Path>,
    pub is_out: bool,
}

#[derive(Clone, Debug)]
pub struct Call {
    pub target: Spanned<Path>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
}

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Option<Spanned<Path>>,
}

#[derive(Clone, Debug)]
pub struct LocalLabel {
    pub ident: Spanned<Ident>,
    pub ir: Option<Spanned<Path>>,
}

impl From<Label> for LocalLabel {
    fn from(label: Label) -> Self {
        LocalLabel {
            ident: label.ident,
            ir: Some(label.ir),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct KindedBlock {
    pub kind: Option<BlockKind>,
    pub block: Block,
}

impl From<Block> for KindedBlock {
    fn from(block: Block) -> Self {
        Self { kind: None, block }
    }
}

#[derive(Clone, Debug, From)]
pub enum Stmt {
    /// Represents a freestanding block in the statement position, *without*
    /// a trailing semicolon. Requires that the block be unit-typed. A trailing
    /// semicolon should cause the block to be parsed as an expression
    /// statement, which ignores the type.
    #[from(types(Block))]
    Block(KindedBlock),
    #[from]
    Let(LetStmt),
    #[from]
    Label(LabelStmt),
    #[from]
    If(IfStmt),
    #[from]
    Check(CheckStmt),
    #[from]
    Goto(GotoStmt),
    #[from]
    Bind(BindStmt),
    Emit(Call),
    #[from]
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: LocalVar,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct LabelStmt {
    pub label: Label,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Spanned<Expr>,
    pub then: Block,
    pub else_: Option<ElseClause>,
}

#[derive(Clone, Debug, From)]
pub enum ElseClause {
    #[from]
    ElseIf(Box<IfStmt>),
    #[from]
    Else(Block),
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckKind,
    pub cond: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct GotoStmt {
    pub label: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct BindStmt {
    pub label: Spanned<Path>,
}

#[derive(Clone, Debug, From)]
pub enum Expr {
    #[from]
    Block(Box<KindedBlock>),
    #[from]
    Literal(Literal),
    #[from]
    Var(Spanned<Path>),
    Invoke(Call),
    #[from]
    FieldAccess(Box<FieldAccessExpr>),
    #[from]
    Negate(Box<NegateExpr>),
    #[from]
    Cast(Box<CastExpr>),
    #[from]
    Compare(Box<CompareExpr>),
    #[from]
    Assign(Box<AssignExpr>),
}

box_from!(KindedBlock => Expr);
box_from!(NegateExpr => Expr);
box_from!(FieldAccessExpr => Expr);
box_from!(CastExpr => Expr);
box_from!(CompareExpr => Expr);
box_from!(AssignExpr => Expr);

deref_from!(&Literal => Expr);
deref_from!(&Spanned<Path> => Expr);

impl From<Block> for Expr {
    fn from(block: Block) -> Self {
        KindedBlock::from(block).into()
    }
}

impl From<Spanned<&Path>> for Expr {
    fn from(path: Spanned<&Path>) -> Self {
        path.copied().into()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Literal {
    Int32(i32),
}

#[derive(Clone, Debug)]
pub struct FieldAccessExpr {
    pub parent: Spanned<Expr>,
    pub field: Spanned<Ident>,
}

#[derive(Clone, Debug)]
pub struct NegateExpr {
    pub kind: Spanned<NegateKind>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub kind: Spanned<CompareKind>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Spanned<Path>,
    pub rhs: Spanned<Expr>,
}
