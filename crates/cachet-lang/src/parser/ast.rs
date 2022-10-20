// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt::{self, Display, Write};
use std::iter::FromIterator;
use std::path::PathBuf;

use derive_more::{Display, From};
use typed_index_collections::TiVec;

use cachet_util::{
    box_from, deref_from, fmt_join, fmt_join_trailing, typed_field_index,
    AffixWriter,
};

use crate::ast::{BinOper, BlockKind, CheckKind, Ident, NegateKind, Path, Spanned, VarParamKind};

#[derive(Clone, Debug, Display)]
#[display(fmt = "#[{path}]")]
pub struct Attr {
    pub path: Spanned<Path>,
}

#[derive(Clone, Debug, Default, From)]
pub struct Mod {
    pub items: Vec<Spanned<Item>>,
}

impl Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join(f, "\n\n", self.items.iter())
    }
}

impl FromIterator<Spanned<Item>> for Mod {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Spanned<Item>>,
    {
        Mod {
            items: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum Item {
    #[from]
    Enum(EnumItem),
    #[from]
    Import(ImportItem),
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

impl Display for EnumItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "enum {} ", self.ident)?;

        let mut block = BlockWriter::start(f)?;
        fmt_join(&mut block, "\n", self.variants.iter().map(CommaSeparated))?;
        block.end()?;

        Ok(())
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "import {:?}", "self.file_path.value")]
pub struct ImportItem {
    pub file_path: Spanned<PathBuf>,
}

typed_field_index!(EnumItem:variants[pub VariantIndex] => Spanned<Ident>);

#[derive(Clone, Debug)]
pub struct StructItem {
    pub ident: Spanned<Ident>,
    pub supertype: Option<Spanned<Path>>,
    pub fields: TiVec<FieldIndex, Field>,
}

impl Display for StructItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "struct {} ", self.ident)?;
        if let Some(supertype) = &self.supertype {
            write!(f, "<: {supertype} ")?;
        }

        let mut block = BlockWriter::start(f)?;
        fmt_join(&mut block, "\n", self.fields.iter().map(CommaSeparated))?;
        block.end()?;

        Ok(())
    }
}

typed_field_index!(StructItem:fields[pub FieldIndex] => Field);

#[derive(Clone, Debug, Display)]
#[display(fmt = "{ident}: {type_}")]
pub struct Field {
    pub ident: Spanned<Ident>,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct IrItem {
    pub ident: Spanned<Ident>,
    pub emits: Option<Spanned<Path>>,
    pub items: Vec<Spanned<Item>>,
}

impl Display for IrItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "ir {} ", self.ident)?;
        if let Some(emits) = &self.emits {
            write!(f, "emits {emits} ")?;
        }

        let mut block = BlockWriter::start(f)?;
        fmt_join(&mut block, "\n\n", self.items.iter())?;
        block.end()?;

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ImplItem {
    pub parent: Spanned<Path>,
    pub items: Vec<Spanned<Item>>,
}

impl Display for ImplItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "impl {} ", self.parent)?;

        let mut block = BlockWriter::start(f)?;
        fmt_join(&mut block, "\n\n", self.items.iter())?;
        block.end()?;

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct GlobalVarItem {
    pub ident: Spanned<Ident>,
    pub attrs: Vec<Attr>,
    pub is_mut: bool,
    pub type_: Spanned<Path>,
    pub value: Option<Spanned<Expr>>,
}

impl Display for GlobalVarItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join_trailing(f, "\n", self.attrs.iter())?;

        write!(f, "var ")?;
        if self.is_mut {
            write!(f, "mut ")?;
        }
        write!(f, "{}: {}", self.ident, self.type_)?;

        if let Some(value) = &self.value {
            write!(f, " = {value}")?;
        }

        write!(f, ";")
    }
}

#[derive(Clone, Debug)]
pub struct CallableItem {
    pub ident: Spanned<Ident>,
    pub attrs: Vec<Attr>,
    pub is_unsafe: bool,
    pub params: Vec<Param>,
    pub emits: Option<Spanned<Path>>,
    pub ret: Option<Spanned<Path>>,
    pub body: Spanned<Option<Block>>,
}

impl Display for CallableItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join_trailing(f, "\n", self.attrs.iter())?;

        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }
        write!(f, "op {}(", self.ident)?;
        fmt_join(f, ", ", self.params.iter())?;
        write!(f, ")")?;

        if let Some(ret) = &self.ret {
            write!(f, " -> {ret}")?;
        }

        match &self.body.value {
            None => write!(f, ";"),
            Some(body) => write!(f, " {body}"),
        }
    }
}

#[derive(Clone, Debug, Display, From)]
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

impl Display for VarParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            VarParamKind::In => (),
            VarParamKind::Mut => write!(f, "mut ")?,
            VarParamKind::Out => write!(f, "out ")?,
        }

        write!(f, "{}: {}", self.ident, self.type_)
    }
}

#[derive(Clone, Debug)]
pub struct LabelParam {
    pub label: Label,
    pub is_out: bool,
}

impl Display for LabelParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_out {
            write!(f, "out ")?;
        }
        write!(f, "{}", self.label)
    }
}

impl From<Label> for LabelParam {
    fn from(label: Label) -> Self {
        Self {
            label,
            is_out: false,
        }
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "label {ident}: {ir}")]
pub struct Label {
    pub ident: Spanned<Ident>,
    pub ir: Spanned<Path>,
}

#[derive(Clone, Debug, Display, From)]
pub enum Arg {
    #[from]
    Expr(Expr),
    /// Arguments that look like `bar` in `foo(bar)` could be either a variable
    /// expression argument or a label argument. The same is true of arguments
    /// that look like `out bar)` in `foo(out bar)`. They will have to be
    /// disambiguated during name resolution.
    #[from]
    FreeVarOrLabel(FreeVarOrLabelArg),
    #[display(fmt = "out {_0}")]
    #[from]
    OutFreshVar(LocalVar),
    #[display(fmt = "out {_0}")]
    #[from]
    OutFreshLabel(LocalLabel),
}

#[derive(Clone, Debug)]
pub struct FreeVarOrLabelArg {
    pub path: Spanned<Path>,
    pub is_out: bool,
}

impl Display for FreeVarOrLabelArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_out {
            write!(f, "out ")?;
        }
        write!(f, "{}", self.path)
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    pub target: Spanned<Path>,
    pub args: Spanned<Vec<Spanned<Arg>>>,
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}(", self.target)?;
        fmt_join(f, ", ", self.args.value.iter())?;
        write!(f, ")")
    }
}

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub ident: Spanned<Ident>,
    pub is_mut: bool,
    pub type_: Option<Spanned<Path>>,
}

impl Display for LocalVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "let ")?;
        if self.is_mut {
            write!(f, "mut ")?;
        }
        write!(f, "{}", self.ident)?;
        if let Some(type_) = self.type_ {
            write!(f, ": {type_}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct LocalLabel {
    pub ident: Spanned<Ident>,
    pub ir: Option<Spanned<Path>>,
}

impl Display for LocalLabel {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "label {}", self.ident)?;
        if let Some(ir) = self.ir {
            write!(f, ": {ir}")?;
        }
        Ok(())
    }
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
    pub value: Spanned<Option<Expr>>,
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut block = BlockWriter::start(f)?;

        fmt_join(&mut block, "\n", self.stmts.iter())?;

        if let Some(value) = &self.value.value {
            if block.has_started_content {
                write!(block, "\n")?;
            }
            write!(block, "{value}")?;
        }

        block.end()?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct KindedBlock {
    pub kind: Option<BlockKind>,
    pub block: Block,
}

impl Display for KindedBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(kind) = self.kind {
            write!(f, "{kind} ")?;
        }
        write!(f, "{}", self.block)
    }
}

impl From<Block> for KindedBlock {
    fn from(block: Block) -> Self {
        Self { kind: None, block }
    }
}

#[derive(Clone, Debug, Display, From)]
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
    #[display(fmt = "emit {_0};")]
    Emit(Call),
    #[from]
    Ret(RetStmt),
    #[display(fmt = "{_0};")]
    #[from]
    Expr(Expr),
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{lhs} = {rhs};")]
pub struct LetStmt {
    pub lhs: LocalVar,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{label};")]
pub struct LabelStmt {
    pub label: Label,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Spanned<Expr>,
    pub then: Block,
    pub else_: Option<ElseClause>,
}

impl Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "if {} {}", self.cond, self.then)?;
        if let Some(else_) = &self.else_ {
            write!(f, " {else_}")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "else {}")]
pub enum ElseClause {
    #[from]
    ElseIf(Box<IfStmt>),
    #[from]
    Else(Block),
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{kind} {cond};")]
pub struct CheckStmt {
    pub kind: CheckKind,
    pub cond: Spanned<Expr>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "goto {label};")]
pub struct GotoStmt {
    pub label: Spanned<Path>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "bind {label};")]
pub struct BindStmt {
    pub label: Spanned<Path>,
}

#[derive(Clone, Debug)]
pub struct RetStmt {
    pub value: Spanned<Option<Expr>>,
}

impl Display for RetStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "return")?;
        if let Some(value) = &self.value.value {
            write!(f, " {value}")?;
        }
        write!(f, ";")
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum Expr {
    #[display(fmt = "({_0})")]
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
    BinOper(Box<BinOperExpr>),
    #[from]
    Assign(Box<AssignExpr>),
}

box_from!(KindedBlock => Expr);
box_from!(NegateExpr => Expr);
box_from!(FieldAccessExpr => Expr);
box_from!(CastExpr => Expr);
box_from!(BinOperExpr => Expr);
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

#[derive(Clone, Copy, Debug, Display)]
pub enum Literal {
    #[display(fmt = "{_0}_i8")]
    Int8(i8),
    #[display(fmt = "{_0}_i16")]
    Int16(i16),
    #[display(fmt = "{_0}_i32")]
    Int32(i32),
    #[display(fmt = "{_0}_i64")]
    Int64(i64),
    #[display(fmt = "{_0}_u8")]
    UInt8(u8),
    #[display(fmt = "{_0}_u16")]
    UInt16(u16),
    #[display(fmt = "{_0}_u32")]
    UInt32(u32),
    #[display(fmt = "{_0}_u64")]
    UInt64(u64),
    #[display(fmt = "{_0}")]
    Double(f64),
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}.{field}", "MaybeGrouped(&self.parent.value)")]
pub struct FieldAccessExpr {
    pub parent: Spanned<Expr>,
    pub field: Spanned<Ident>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{kind}{}", "MaybeGrouped(&self.expr.value)")]
pub struct NegateExpr {
    pub kind: Spanned<NegateKind>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{} as {type_}", "MaybeGrouped(&self.expr.value)")]
pub struct CastExpr {
    pub expr: Spanned<Expr>,
    pub type_: Spanned<Path>,
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "{} {oper} {}",
    "MaybeGrouped(&self.lhs.value)",
    "MaybeGrouped(&self.rhs.value)"
)]
pub struct BinOperExpr {
    pub oper: Spanned<BinOper>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{lhs} = {}", "MaybeGrouped(&self.rhs.value)")]
pub struct AssignExpr {
    pub lhs: Spanned<Path>,
    pub rhs: Spanned<Expr>,
}

// * Formatting Utilities

struct BlockWriter<W> {
    inner: AffixWriter<'static, W>,
    has_started_content: bool,
}

impl<W: Write> BlockWriter<W> {
    fn start(mut inner: W) -> Result<Self, fmt::Error> {
        inner.write_char('{')?;
        Ok(Self {
            inner: AffixWriter::new(inner, "  ", ""),
            has_started_content: false,
        })
    }

    fn end(self) -> Result<W, fmt::Error> {
        let mut inner = self.inner.into_inner();
        if self.has_started_content {
            inner.write_char('\n')?;
        }
        inner.write_char('}')?;
        Ok(inner)
    }

    fn ensure_content_started(&mut self) -> Result<(), fmt::Error> {
        if !self.has_started_content {
            self.inner.write_char('\n')?;
            self.has_started_content = true;
        }
        Ok(())
    }
}

impl<W: Write> Write for BlockWriter<W> {
    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        self.ensure_content_started()?;
        self.inner.write_str(s)
    }

    fn write_char(&mut self, c: char) -> Result<(), fmt::Error> {
        self.ensure_content_started()?;
        self.inner.write_char(c)
    }
}

struct CommaSeparated<'a, T>(&'a T);

impl<T: Display> Display for CommaSeparated<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{},", self.0)
    }
}

struct MaybeGrouped<'a>(&'a Expr);

impl Display for MaybeGrouped<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let needs_group = match self.0 {
            Expr::Block(_)
            | Expr::Literal(_)
            | Expr::Var(_)
            | Expr::Invoke(_)
            | Expr::FieldAccess(_) => false,
            Expr::Negate(_) | Expr::Cast(_) | Expr::BinOper(_) | Expr::Assign(_) => true,
        };

        if needs_group {
            write!(f, "({})", self.0)?;
        } else {
            Display::fmt(self.0, f)?;
        }
        Ok(())
    }
}
