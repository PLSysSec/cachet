// vim: set tw=99 ts=4 sts=4 sw=4 et:

#![allow(dead_code)]

use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Display, Write};
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

use derive_more::{Display, From};
use enum_map::Enum;

use cachet_lang::ast::{CastKind, CheckKind, CompareKind, Ident, NegateKind};
pub use cachet_lang::normalizer::{LocalLabelIndex, LocalVarIndex};
use cachet_util::{
    box_from, deref_from, fmt_join, fmt_join_leading, fmt_join_trailing, typed_index, AffixWriter,
};

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "#{}", ident)]
pub struct IrIdent {
    pub ident: Ident,
}

#[derive(Clone, Debug, Display, From)]
pub enum Type {
    #[from(types(
        NativeTypeIdent,
        BitVecTypeIdent,
        FloatTypeIdent,
        PreludeTypeIdent,
        IrMemberTypeIdent,
        UserTypeIdent
    ))]
    Ident(TypeIdent),
    #[from]
    Map(Box<MapType>),
}

box_from!(MapType => Type);

#[derive(Clone, Copy, Debug, Display, From)]
pub enum TypeIdent {
    #[from(types(BitVecTypeIdent, FloatTypeIdent))]
    Native(NativeTypeIdent),
    #[from]
    Prelude(PreludeTypeIdent),
    #[from]
    IrMember(IrMemberTypeIdent),
    #[from]
    User(UserTypeIdent),
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum NativeTypeIdent {
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "int")]
    Int,
    BitVec(BitVecTypeIdent),
    Float(FloatTypeIdent),
}

#[derive(Clone, Copy, Debug, Display)]
pub enum PreludeTypeIdent {
    #[display(fmt = "Pc")]
    Pc,
    #[display(fmt = "EmitPath")]
    EmitPath,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}^{}", ir_ident, selector)]
pub struct IrMemberTypeIdent {
    pub ir_ident: IrIdent,
    pub selector: IrMemberTypeSelector,
}

#[derive(Clone, Copy, Debug, Display)]
pub enum IrMemberTypeSelector {
    #[display(fmt = "Op")]
    Op,
    #[display(fmt = "Label")]
    Label,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "bv{}", width)]
pub struct BitVecTypeIdent {
    pub width: u8,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "float{}e{}", sig_width, exp_width)]
pub struct FloatTypeIdent {
    pub sig_width: u8,
    pub exp_width: u8,
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "#{}", ident)]
pub struct UserTypeIdent {
    pub ident: Ident,
}

#[derive(Clone, Debug)]
pub struct MapType {
    pub key_types: Vec<Type>,
    pub value_type: Type,
}

impl Display for MapType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for key_type in &self.key_types {
            write!(f, "[{}]", key_type)?;
        }
        write!(f, "{}", self.value_type)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum VarIdent {
    #[from(types(IrMemberGlobalVarIdent, UserGlobalVarIdent))]
    Global(GlobalVarIdent),
    #[from(types(UserParamVarIdent))]
    Param(ParamVarIdent),
    #[display(fmt = "ret")]
    Ret,
    // TODO(spinda): Move these under LocalVarIdent.
    #[display(fmt = "op")]
    Op,
    #[display(fmt = "pc")]
    Pc,
    #[from]
    Local(LocalVarIdent),
    #[from]
    LocalLabel(LocalLabelVarIdent),
    #[from]
    Synthetic(SyntheticVarIdent),
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum GlobalVarIdent {
    IrMember(IrMemberGlobalVarIdent),
    User(UserGlobalVarIdent),
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}^{}", ir_ident, selector)]
pub struct IrMemberGlobalVarIdent {
    pub ir_ident: IrIdent,
    pub selector: IrMemberGlobalVarSelector,
}

#[derive(Clone, Copy, Debug, Display)]
pub enum IrMemberGlobalVarSelector {
    #[display(fmt = "pc")]
    Pc,
    #[display(fmt = "ops")]
    Ops,
    #[display(fmt = "pcEmitPaths")]
    PcEmitPaths,
    #[display(fmt = "nextLabel")]
    NextLabel,
    #[display(fmt = "labelPcs")]
    LabelPcs,
}

#[derive(Clone, Copy, Debug, From)]
pub struct UserGlobalVarIdent {
    pub parent_ident: Option<Ident>,
    pub var_ident: Ident,
}

impl Display for UserGlobalVarIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "#")?;
        if let Some(parent_ident) = &self.parent_ident {
            write!(f, "{}~", parent_ident)?;
        }
        write!(f, "{}", self.var_ident)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum ParamVarIdent {
    #[display(fmt = "in")]
    In,
    #[display(fmt = "init")]
    Init,
    #[display(fmt = "last")]
    Last,
    #[display(fmt = "emitPath")]
    EmitPath,
    #[display(fmt = "op")]
    Op,
    #[display(fmt = "label")]
    Label,
    User(UserParamVarIdent),
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "${}", ident)]
pub struct UserParamVarIdent {
    pub ident: Ident,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "${}'v{}", ident, index)]
pub struct LocalVarIdent {
    pub ident: Ident,
    pub index: LocalVarIndex,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "${}'l{}", ident, index)]
pub struct LocalLabelVarIdent {
    pub ident: Ident,
    pub index: LocalLabelIndex,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}'{}", kind, index)]
pub struct SyntheticVarIdent {
    pub kind: SyntheticVarKind,
    pub index: usize,
}

#[derive(Clone, Copy, Debug, Display, Enum)]
pub enum SyntheticVarKind {
    #[display(fmt = "out")]
    Out,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum FnIdent {
    Prelude(PreludeFnIdent),
    TypeMember(TypeMemberFnIdent),
    IrMember(IrMemberFnIdent),
    User(UserFnIdent),
    ExternalUserFnHelper(ExternalUserFnHelperFnIdent),
    EntryPoint(EntryPointFnIdent),
    OpCtorField(OpCtorFieldFnIdent),
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum PreludeFnIdent {
    #[display(fmt = "NilEmitPath")]
    NilEmitPathCtor,
    #[display(fmt = "ConsEmitPath")]
    ConsEmitPathCtor,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}#{}", param_var_ident, op_ctor_ident)]
pub struct OpCtorFieldFnIdent {
    pub param_var_ident: ParamVarIdent,
    pub op_ctor_ident: IrMemberFnIdent,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}^{}", type_ident, selector)]
pub struct TypeMemberFnIdent {
    pub type_ident: UserTypeIdent,
    pub selector: TypeMemberFnSelector,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum TypeMemberFnSelector {
    #[display(fmt = "Negate")]
    Negate,
    Cast(CastTypeMemberFnSelector),
    Variant(VariantCtorTypeMemberFnSelector),
}

#[derive(Clone, Copy, Debug)]
pub struct CastTypeMemberFnSelector {
    pub kind: CastKind,
    pub supertype_ident: UserTypeIdent,
}

impl Display for CastTypeMemberFnSelector {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}{}",
            match self.kind {
                CastKind::Downcast => "from",
                CastKind::Upcast => "to",
            },
            self.supertype_ident
        )?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Variant~{}", variant_ident)]
pub struct VariantCtorTypeMemberFnSelector {
    pub variant_ident: Ident,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}^{}", ir_ident, selector)]
pub struct IrMemberFnIdent {
    pub ir_ident: IrIdent,
    pub selector: IrMemberFnSelector,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum IrMemberFnSelector {
    #[display(fmt = "step")]
    Step,
    #[display(fmt = "emit")]
    Emit,
    #[display(fmt = "label")]
    Label,
    #[display(fmt = "bind")]
    Bind,
    #[display(fmt = "goto")]
    Goto,
    Op(OpCtorIrMemberFnSelector),
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Op{}", op_selector)]
pub struct OpCtorIrMemberFnSelector {
    pub op_selector: OpSelector,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum OpSelector {
    #[display(fmt = "^External")]
    External,
    User(UserOpSelector),
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "~{}", op_ident)]
pub struct UserOpSelector {
    pub op_ident: Ident,
}

#[derive(Clone, Copy, Debug)]
pub struct UserFnIdent {
    pub parent_ident: Option<Ident>,
    pub fn_ident: Ident,
}

impl Display for UserFnIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "#")?;
        if let Some(parent_ident) = &self.parent_ident {
            write!(f, "{}~", parent_ident)?;
        }
        write!(f, "{}", self.fn_ident)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "{}^{}", fn_ident, ret_var_ident)]
pub struct ExternalUserFnHelperFnIdent {
    pub fn_ident: UserFnIdent,
    pub ret_var_ident: VarIdent,
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "EntryPoint{}{}", ir_ident, user_op_selector)]
pub struct EntryPointFnIdent {
    pub ir_ident: IrIdent,
    pub user_op_selector: UserOpSelector,
}

#[derive(Clone, Debug, Display, From)]
pub enum LabelIdent {
    Emit(EmitLabelIdent),
}

#[derive(Clone, Debug, Default, From)]
pub struct EmitLabelIdent {
    pub segments: Vec<EmitLabelSegment>,
}

impl Display for EmitLabelIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "emit")?;
        fmt_join_leading(f, "'", self.segments.iter())?;
        Ok(())
    }
}

impl FromIterator<EmitLabelSegment> for EmitLabelIdent {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = EmitLabelSegment>,
    {
        EmitLabelIdent {
            segments: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}{}{}", local_emit_index, ir_ident, op_selector)]
pub struct EmitLabelSegment {
    pub local_emit_index: LocalEmitIndex,
    pub ir_ident: IrIdent,
    pub op_selector: OpSelector,
}

typed_index!(pub LocalEmitIndex);

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}: {}", ident, type_)]
pub struct TypedVar {
    pub ident: VarIdent,
    pub type_: Type,
}

#[derive(Clone, Debug, Default, From)]
pub struct TypedVars {
    pub vars: Vec<TypedVar>,
}

impl AsRef<Vec<TypedVar>> for TypedVars {
    fn as_ref(&self) -> &Vec<TypedVar> {
        &*self
    }
}

impl AsMut<Vec<TypedVar>> for TypedVars {
    fn as_mut(&mut self) -> &mut Vec<TypedVar> {
        &mut *self
    }
}

impl Borrow<Vec<TypedVar>> for TypedVars {
    fn borrow(&self) -> &Vec<TypedVar> {
        &*self
    }
}

impl BorrowMut<Vec<TypedVar>> for TypedVars {
    fn borrow_mut(&mut self) -> &mut Vec<TypedVar> {
        &mut *self
    }
}

impl Deref for TypedVars {
    type Target = Vec<TypedVar>;

    fn deref(&self) -> &Self::Target {
        &self.vars
    }
}

impl DerefMut for TypedVars {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vars
    }
}

impl Display for TypedVars {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join(f, ", ", self.iter())?;
        Ok(())
    }
}

impl FromIterator<TypedVar> for TypedVars {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = TypedVar>,
    {
        TypedVars {
            vars: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug, Default, From)]
pub struct Code {
    pub items: Vec<Item>,
}

impl Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join(f, "\n\n", self.items.iter())
    }
}

impl FromIterator<Item> for Code {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Item>,
    {
        Code {
            items: iter.into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum Item {
    Type(TypeItem),
    Const(ConstItem),
    GlobalVar(GlobalVarItem),
    Axiom(AxiomItem),
    Fn(FnItem),
    Proc(ProcItem),
}

#[derive(Clone, Debug)]
pub struct TypeItem {
    pub ident: TypeIdent,
    pub attr: Option<TypeAttr>,
    pub type_: Option<Type>,
}

impl Display for TypeItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "type ")?;

        if let Some(attr) = &self.attr {
            write!(f, "{} ", attr)?;
        }

        write!(f, "{}", self.ident)?;

        if let Some(type_) = &self.type_ {
            write!(f, " = {}", type_)?;
        }

        write!(f, ";")?;
        Ok(())
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum TypeAttr {
    #[display(fmt = "{{:datatype}}")]
    DataType,
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "const {};", var)]
pub struct ConstItem {
    pub var: TypedVar,
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "var {};", var)]
pub struct GlobalVarItem {
    pub var: TypedVar,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "axiom {};", "MaybeGrouped(&self.cond)")]
pub struct AxiomItem {
    pub cond: Expr,
}

#[derive(Clone, Debug)]
pub struct FnItem {
    pub ident: FnIdent,
    pub attr: Option<FnAttr>,
    pub param_vars: TypedVars,
    pub ret: Type,
    pub value: Option<Expr>,
}

impl Display for FnItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "function ")?;

        if let Some(attr) = &self.attr {
            write!(f, "{} ", attr)?;
        }

        write!(f, "{}({}): {}", self.ident, self.param_vars, self.ret)?;

        match &self.value {
            None => {
                write!(f, ";")?;
            }
            Some(value) => {
                write!(f, "{{")?;

                let mut indented = AffixWriter::new(f, "  ", "");
                write!(indented, "{}", value)?;
                let f = indented.into_inner();

                write!(f, "}}")?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum FnAttr {
    #[display(fmt = "{{:constructor}}")]
    Ctor,
    #[display(fmt = "{{:inline}}")]
    Inline,
}

#[derive(Clone, Debug)]
pub struct ProcItem {
    pub ident: FnIdent,
    pub attr: Option<ProcAttr>,
    pub param_vars: TypedVars,
    pub ret_vars: TypedVars,
    pub body: Option<Body>,
}

impl Display for ProcItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "procedure ")?;

        if let Some(attr) = &self.attr {
            write!(f, "{} ", attr)?;
        }

        write!(f, "{}({})", self.ident, self.param_vars)?;

        if !self.ret_vars.is_empty() {
            write!(f, "\n  returns ({})", self.ret_vars)?;
        }

        match &self.body {
            None => {
                write!(f, ";")?;
            }
            Some(body) => {
                write!(f, "\n{}", body)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum ProcAttr {
    #[display(fmt = "{{:entrypoint}}")]
    EntryPoint,
    Inline(InlineProcAttr),
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{{:inline {}}}", depth)]
pub struct InlineProcAttr {
    pub depth: u8,
}

#[derive(Clone, Debug, Default)]
pub struct Body {
    pub local_vars: Vec<LocalVar>,
    pub stmts: Vec<Stmt>,
}

impl Display for Body {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{\n")?;

        let mut indented = AffixWriter::new(f, "  ", "");
        fmt_join_trailing(&mut indented, "\n", self.local_vars.iter())?;
        if !self.local_vars.is_empty() {
            write!(indented, "\n")?;
        }
        fmt_join_trailing(&mut indented, "\n", self.stmts.iter())?;
        let f = indented.into_inner();

        write!(f, "}}")?;
        Ok(())
    }
}

impl From<Vec<Stmt>> for Body {
    fn from(stmts: Vec<Stmt>) -> Self {
        Body {
            local_vars: Vec::new(),
            stmts,
        }
    }
}

impl From<Block> for Body {
    fn from(block: Block) -> Self {
        Body::from(block.stmts)
    }
}

impl FromIterator<Stmt> for Body {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        let stmts: Vec<Stmt> = iter.into_iter().collect();
        Body::from(stmts)
    }
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "var {};", var)]
pub struct LocalVar {
    pub var: TypedVar,
}

#[derive(Clone, Debug, Default, From)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{\n")?;

        let mut indented = AffixWriter::new(f, "  ", "");
        fmt_join_trailing(&mut indented, "\n", self.stmts.iter())?;
        let f = indented.into_inner();

        write!(f, "}}")
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

#[derive(Clone, Debug, Display, From)]
pub enum Stmt {
    #[from]
    If(IfStmt),
    #[from]
    Check(CheckStmt),
    #[from]
    Label(LabelStmt),
    #[from]
    Goto(GotoStmt),
    #[from(types(CallExpr))]
    Call(CallStmt),
    #[from]
    Assign(AssignStmt),
    #[display(fmt = "return;")]
    #[from]
    Ret,
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Block,
    pub else_: Option<ElseClause>,
}

impl Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "if ({}) {}", self.cond, self.then)?;
        if let Some(else_) = &self.else_ {
            write!(f, "{}", else_)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, From)]
pub enum ElseClause {
    #[from]
    ElseIf(Box<IfStmt>),
    #[from]
    Else(Block),
}

impl Display for ElseClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ElseClause::ElseIf(if_stmt) => write!(f, " else {}", *if_stmt),
            ElseClause::Else(block) => write!(f, " else {}", block),
        }?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct CheckStmt {
    pub kind: CheckKind,
    pub attr: Option<CheckAttr>,
    pub cond: Expr,
}

impl Display for CheckStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} ",
            match self.kind {
                CheckKind::Assert => "assert",
                CheckKind::Assume => "assume",
            }
        )?;

        if let Some(attr) = &self.attr {
            write!(f, "{} ", attr)?;
        }

        write!(f, "{};", MaybeGrouped(&self.cond))?;

        Ok(())
    }
}

#[derive(Clone, Debug, Display, From)]
pub enum CheckAttr {
    #[display(fmt = "{{:partition}}")]
    Partition,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "\n{}:", label_ident)]
pub struct LabelStmt {
    pub label_ident: LabelIdent,
}

#[derive(Clone, Debug)]
pub struct GotoStmt {
    pub labels: Vec<LabelIdent>,
}

impl Display for GotoStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "goto ")?;
        fmt_join(f, ", ", self.labels.iter())?;
        write!(f, ";")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct CallStmt {
    pub call: CallExpr,
    pub ret_var_idents: Vec<VarIdent>,
}

impl Display for CallStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "call ")?;
        if !self.ret_var_idents.is_empty() {
            fmt_join(f, ", ", self.ret_var_idents.iter())?;
            write!(f, " := ")?;
        }
        write!(f, "{};", self.call)?;
        Ok(())
    }
}

impl From<CallExpr> for CallStmt {
    fn from(call: CallExpr) -> Self {
        CallStmt {
            call,
            ret_var_idents: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "{} := {};",
    "MaybeGrouped(&self.lhs)",
    "MaybeGrouped(&self.rhs)"
)]
pub struct AssignStmt {
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug, Display, From)]
pub enum Expr {
    #[from(types(bool))]
    Literal(Literal),
    #[from(types(
        GlobalVarIdent,
        IrMemberGlobalVarIdent,
        UserGlobalVarIdent,
        ParamVarIdent,
        UserParamVarIdent,
        LocalVarIdent,
        LocalLabelVarIdent,
        SyntheticVarIdent
    ))]
    Var(VarIdent),
    #[from]
    Call(CallExpr),
    #[from]
    Index(Box<IndexExpr>),
    #[from]
    Negate(Box<NegateExpr>),
    #[from]
    Compare(Box<CompareExpr>),
    #[from]
    Arith(Box<ArithExpr>),
    #[from]
    ForAll(Box<ForAllExpr>),
}

box_from!(IndexExpr => Expr);
box_from!(NegateExpr => Expr);
box_from!(CompareExpr => Expr);
box_from!(ArithExpr => Expr);
box_from!(ForAllExpr => Expr);

deref_from!(&Literal => Expr);
deref_from!(&bool => Expr);

struct MaybeGrouped<'a>(&'a Expr);

impl Display for MaybeGrouped<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let needs_group = match self.0 {
            Expr::Literal(_) | Expr::Var(_) | Expr::Index(_) | Expr::Call(_) => false,
            Expr::Negate(_) | Expr::Compare(_) | Expr::Arith(_) | Expr::ForAll(_) => true,
        };

        if needs_group {
            write!(f, "({})", self.0)?;
        } else {
            Display::fmt(self.0, f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum Literal {
    #[display(fmt = "{:?}", _0)]
    #[from]
    Bool(bool),
    #[display(fmt = "{:?}", _0)]
    Int(usize),
    #[display(fmt = "{:?}bv32", _0)]
    Bv32(u32),
}

deref_from!(&bool => Literal);

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: FnIdent,
    pub arg_exprs: Vec<Expr>,
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}(", self.target)?;
        fmt_join(f, ", ", self.arg_exprs.iter())?;
        write!(f, ")")?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct IndexExpr {
    pub base: Expr,
    pub key: Expr,
    pub value: Option<Expr>,
}

impl Display for IndexExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}[{}",
            MaybeGrouped(&self.base),
            MaybeGrouped(&self.key)
        )?;
        if let Some(value) = &self.value {
            write!(f, " := {}", MaybeGrouped(value))?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}{}", kind, "MaybeGrouped(&self.expr)")]
pub struct NegateExpr {
    pub kind: NegateKind,
    pub expr: Expr,
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "{} {} {}",
    "MaybeGrouped(&self.lhs)",
    kind,
    "MaybeGrouped(&self.rhs)"
)]
pub struct CompareExpr {
    pub kind: CompareKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "{} {} {}",
    "MaybeGrouped(&self.lhs)",
    kind,
    "MaybeGrouped(&self.rhs)"
)]
pub struct ArithExpr {
    pub kind: ArithKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Copy, Debug, Display)]
pub enum ArithKind {
    #[display(fmt = "+")]
    Add,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "forall {} :: {}", vars, "MaybeGrouped(&self.expr)")]
pub struct ForAllExpr {
    pub vars: TypedVars,
    pub expr: Expr,
}
