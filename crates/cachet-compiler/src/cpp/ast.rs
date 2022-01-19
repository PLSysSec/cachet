// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt::{self, Write};
use std::iter::FromIterator;

use derive_more::{Display, From};
use enumset::EnumSetType;
use indent_write::fmt::IndentWriter;

use cachet_lang::ast::Ident;
pub use cachet_lang::normalizer::{
    CastExprKind, CompareExprKind, LocalLabelIndex, LocalVarIndex, NegateExprKind,
};

use crate::util::{fmt_join, fmt_join_trailing};

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}_{}", kind, ident)]
pub struct NamespaceIdent {
    pub kind: NamespaceKind,
    pub ident: Ident,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum NamespaceKind {
    #[display(fmt = "Type")]
    Type,
    #[display(fmt = "IR")]
    Ir,
    #[display(fmt = "Impl")]
    Impl,
}

#[derive(Clone, Debug, Display, From)]
pub enum Type {
    #[display(fmt = "void")]
    Void,
    #[from(types(HelperTypeIdent, TypeMemberTypePath, IrMemberTypePath))]
    Path(TypePath),
    #[from]
    Template(Box<TemplateType>),
    #[from]
    Const(Box<ConstType>),
    #[from]
    Ref(Box<RefType>),
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum TypePath {
    Helper(HelperTypeIdent),
    TypeMember(TypeMemberTypePath),
    IrMember(IrMemberTypePath),
}

impl TypePath {
    pub const fn ident(self) -> TypeIdent {
        match self {
            TypePath::Helper(helper_type_ident) => TypeIdent::Helper(helper_type_ident),
            TypePath::TypeMember(type_member_type_path) => {
                TypeIdent::TypeMember(type_member_type_path.ident)
            }
            TypePath::IrMember(ir_member_type_path) => {
                TypeIdent::IrMember(ir_member_type_path.ident)
            }
        }
    }

    pub const fn parent_namespace(self) -> Option<NamespaceIdent> {
        match self {
            TypePath::Helper(_) => None,
            TypePath::TypeMember(type_member_type_path) => {
                Some(type_member_type_path.parent_namespace())
            }
            TypePath::IrMember(ir_member_type_path) => {
                Some(ir_member_type_path.parent_namespace())
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum TypeIdent {
    Helper(HelperTypeIdent),
    TypeMember(TypeMemberTypeIdent),
    IrMember(IrMemberTypeIdent),
}

impl TypeIdent {
    pub const fn parent_namespace_kind(self) -> Option<NamespaceKind> {
        match self {
            TypeIdent::Helper(_) => None,
            TypeIdent::TypeMember(_) => Some(TypeMemberTypeIdent::PARENT_NAMESPACE_KIND),
            TypeIdent::IrMember(_) => Some(IrMemberTypeIdent::PARENT_NAMESPACE_KIND),
        }
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum HelperTypeIdent {
    #[display(fmt = "Cachet_ContextRef")]
    ContextRef,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}::{}", "self.parent_namespace()", ident)]
pub struct TypeMemberTypePath {
    pub parent: Ident,
    pub ident: TypeMemberTypeIdent,
}

impl TypeMemberTypePath {
    pub const fn parent_namespace(self) -> NamespaceIdent {
        NamespaceIdent {
            kind: TypeMemberTypeIdent::PARENT_NAMESPACE_KIND,
            ident: self.parent,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, From, Hash, PartialEq)]
pub enum TypeMemberTypeIdent {
    ExprTag(ExprTag),
}

impl TypeMemberTypeIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Type;
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}::{}", "self.parent_namespace()", ident)]
pub struct IrMemberTypePath {
    pub parent: Ident,
    pub ident: IrMemberTypeIdent,
}

impl IrMemberTypePath {
    pub const fn parent_namespace(self) -> NamespaceIdent {
        NamespaceIdent {
            kind: IrMemberTypeIdent::PARENT_NAMESPACE_KIND,
            ident: self.parent,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum IrMemberTypeIdent {
    #[display(fmt = "LabelRef")]
    LabelRef,
    #[display(fmt = "InterpreterRef")]
    InterpreterRef,
    #[display(fmt = "CompilerRef")]
    CompilerRef,
}

impl IrMemberTypeIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Ir;
}

#[derive(Clone, Debug)]
pub struct TemplateType {
    pub inner: Type,
    pub args: Vec<Type>,
}

impl fmt::Display for TemplateType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}<", self.inner)?;
        fmt_join(f, ", ", self.args.iter())?;
        write!(f, ">")
    }
}

impl From<TemplateType> for Type {
    fn from(template_type: TemplateType) -> Self {
        Box::new(template_type).into()
    }
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "{} const", inner)]
pub struct ConstType {
    inner: Type,
}

impl From<ConstType> for Type {
    fn from(const_type: ConstType) -> Self {
        Box::new(const_type).into()
    }
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "{}{}", inner, value_category)]
pub struct RefType {
    pub inner: Type,
    pub value_category: ValueCategory,
}

impl From<RefType> for Type {
    fn from(ref_type: RefType) -> Self {
        Box::new(ref_type).into()
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum ValueCategory {
    #[display(fmt = "&")]
    LValue,
    #[display(fmt = "&&")]
    RValue,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum VarIdent {
    #[from(types(IrContextParamIdent, UserParamIdent))]
    Param(ParamIdent),
    #[from]
    Local(LocalVarIdent),
    #[from]
    LocalLabel(LocalLabelVarIdent),
    #[from]
    Synthetic(SyntheticVarIdent),
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "local_{}_{}", ident, index)]
pub struct LocalVarIdent {
    pub ident: Ident,
    pub index: LocalVarIndex,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "label_{}_{}", ident, index)]
pub struct LocalLabelVarIdent {
    pub ident: Ident,
    pub index: LocalLabelIndex,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}_{}_{}", kind, ident, index)]
pub struct SyntheticVarIdent {
    pub kind: SyntheticVarKind,
    pub ident: Ident,
    pub index: usize,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum SyntheticVarKind {
    #[display(fmt = "result")]
    Result,
    #[display(fmt = "ret")]
    Ret,
    #[display(fmt = "out")]
    Out,
    #[display(fmt = "tmp")]
    Tmp,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum FnPath {
    Helper(HelperFnIdent),
    TypeMember(TypeMemberFnPath),
    IrMember(IrMemberFnPath),
    Variant(VariantFnPath),
    GlobalVar(GlobalVarFnPath),
    User(UserFnPath),
    Op(OpFnPath),
}

impl FnPath {
    pub const fn ident(self) -> FnIdent {
        match self {
            FnPath::Helper(helper_fn_ident) => FnIdent::Helper(helper_fn_ident),
            FnPath::TypeMember(type_member_fn_path) => {
                FnIdent::TypeMember(type_member_fn_path.ident)
            }
            FnPath::IrMember(ir_member_fn_path) => FnIdent::IrMember(ir_member_fn_path.ident),
            FnPath::Variant(variant_fn_path) => FnIdent::Variant(variant_fn_path.ident),
            FnPath::GlobalVar(global_var_fn_path) => FnIdent::GlobalVar(global_var_fn_path.ident),
            FnPath::User(user_fn_path) => FnIdent::User(user_fn_path.ident),
            FnPath::Op(op_fn_path) => FnIdent::Op(op_fn_path.ident),
        }
    }

    pub const fn parent_namespace(self) -> Option<NamespaceIdent> {
        match self {
            FnPath::Helper(_) => None,
            FnPath::TypeMember(type_member_fn_path) => {
                Some(type_member_fn_path.parent_namespace())
            }
            FnPath::IrMember(ir_member_fn_path) => Some(ir_member_fn_path.parent_namespace()),
            FnPath::Variant(variant_fn_path) => Some(variant_fn_path.parent_namespace()),
            FnPath::GlobalVar(global_var_fn_path) => global_var_fn_path.parent_namespace(),
            FnPath::User(user_fn_path) => user_fn_path.parent_namespace(),
            FnPath::Op(op_fn_path) => Some(op_fn_path.parent_namespace()),
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum FnIdent {
    Helper(HelperFnIdent),
    TypeMember(TypeMemberFnIdent),
    IrMember(IrMemberFnIdent),
    Variant(VariantFnIdent),
    GlobalVar(GlobalVarFnIdent),
    User(UserFnIdent),
    Op(OpFnIdent),
}

impl FnIdent {
    pub const fn parent_namespace_kind(self) -> Option<NamespaceKind> {
        match self {
            FnIdent::Helper(_) => None,
            FnIdent::TypeMember(type_member_fn_ident) => {
                Some(type_member_fn_ident.parent_namespace_kind())
            }
            FnIdent::IrMember(_) => Some(IrMemberFnIdent::PARENT_NAMESPACE_KIND),
            FnIdent::Variant(_) => Some(VariantFnIdent::PARENT_NAMESPACE_KIND),
            FnIdent::GlobalVar(_) => Some(GlobalVarFnIdent::PARENT_NAMESPACE_KIND),
            FnIdent::User(_) => Some(UserFnIdent::PARENT_NAMESPACE_KIND),
            FnIdent::Op(_) => Some(OpFnIdent::PARENT_NAMESPACE_KIND),
        }
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum HelperFnIdent {
    #[display(fmt = "Cachet_Assert")]
    Assert,
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "{}::{}", "self.parent_namespace()", ident)]
pub struct TypeMemberFnPath {
    pub parent: Ident,
    pub ident: TypeMemberFnIdent,
}

impl TypeMemberFnPath {
    pub const fn parent_namespace(self) -> NamespaceIdent {
        NamespaceIdent {
            kind: self.ident.parent_namespace_kind(),
            ident: self.parent,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum TypeMemberFnIdent {
    #[display(fmt = "EmptyLocal")]
    EmptyLocal,
    ToTag(ToTagTypeMemberFnIdent),
    #[display(fmt = "SetMutRef")]
    SetMutRef,
    Cast(CastTypeMemberFnIdent),
    Compare(CompareTypeMemberFnIdent),
}

impl TypeMemberFnIdent {
    pub const fn parent_namespace_kind(self) -> NamespaceKind {
        match self {
            TypeMemberFnIdent::EmptyLocal | TypeMemberFnIdent::SetMutRef => NamespaceKind::Type,
            TypeMemberFnIdent::ToTag(_) => ToTagTypeMemberFnIdent::PARENT_NAMESPACE_KIND,
            TypeMemberFnIdent::Cast(_) => CastTypeMemberFnIdent::PARENT_NAMESPACE_KIND,
            TypeMemberFnIdent::Compare(_) => CompareTypeMemberFnIdent::PARENT_NAMESPACE_KIND,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "To{}", tag)]
pub struct ToTagTypeMemberFnIdent {
    pub tag: ExprTag,
}

impl ToTagTypeMemberFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Type;
}

#[derive(Clone, Copy, Debug)]
pub struct CastTypeMemberFnIdent {
    pub kind: CastExprKind,
    pub supertype: Ident,
}

impl CastTypeMemberFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Impl;
}

impl fmt::Display for CastTypeMemberFnIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}_{}",
            match self.kind {
                CastExprKind::Downcast => "From",
                CastExprKind::Upcast => "To",
            },
            self.supertype
        )
    }
}

#[derive(Clone, Copy, Debug, From)]
pub struct CompareTypeMemberFnIdent {
    pub kind: CompareExprKind,
}

impl CompareTypeMemberFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Type;
}

impl fmt::Display for CompareTypeMemberFnIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "Compare{}",
            match self.kind {
                CompareExprKind::Eq => "Eq",
                CompareExprKind::Neq => "Neq",
                CompareExprKind::Lte => "Lte",
                CompareExprKind::Gte => "Gte",
                CompareExprKind::Lt => "Lt",
                CompareExprKind::Gt => "Gt",
            }
        )
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "{}::{}", "self.parent_namespace()", ident)]
pub struct IrMemberFnPath {
    pub parent: Ident,
    pub ident: IrMemberFnIdent,
}

impl IrMemberFnPath {
    pub const fn parent_namespace(self) -> NamespaceIdent {
        NamespaceIdent {
            kind: IrMemberFnIdent::PARENT_NAMESPACE_KIND,
            ident: self.parent,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum IrMemberFnIdent {
    #[display(fmt = "GetOutput")]
    GetOutput,
    EmitOp(EmitOpIrMemberFnIdent),
    #[display(fmt = "Goto")]
    Goto,
}

impl IrMemberFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Ir;
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Emit{}", op)]
pub struct EmitOpIrMemberFnIdent {
    pub op: OpFnIdent,
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}::{}", "self.parent_namespace()", ident)]
pub struct VariantFnPath {
    pub parent: Ident,
    pub ident: VariantFnIdent,
}

impl VariantFnPath {
    pub const fn parent_namespace(self) -> NamespaceIdent {
        NamespaceIdent {
            kind: VariantFnIdent::PARENT_NAMESPACE_KIND,
            ident: self.parent,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Variant_{}", ident)]
pub struct VariantFnIdent {
    pub ident: Ident,
}

impl VariantFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Impl;
}

#[derive(Clone, Copy, Debug)]
pub struct GlobalVarFnPath {
    pub parent: Option<Ident>,
    pub ident: GlobalVarFnIdent,
}

impl GlobalVarFnPath {
    pub const fn parent_namespace(self) -> Option<NamespaceIdent> {
        match self.parent {
            Some(parent_ident) => Some(NamespaceIdent {
                kind: GlobalVarFnIdent::PARENT_NAMESPACE_KIND,
                ident: parent_ident,
            }),
            None => None,
        }
    }
}

impl fmt::Display for GlobalVarFnPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(parent_namespace_ident) = self.parent_namespace() {
            write!(f, "{}::", parent_namespace_ident)?;
        }
        write!(f, "{}", self.ident)
    }
}

impl From<GlobalVarFnIdent> for GlobalVarFnPath {
    fn from(global_var_fn_ident: GlobalVarFnIdent) -> Self {
        GlobalVarFnPath {
            parent: None,
            ident: global_var_fn_ident,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Var_{}", ident)]
pub struct GlobalVarFnIdent {
    pub ident: Ident,
}

impl GlobalVarFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Impl;
}

#[derive(Clone, Copy, Debug)]
pub struct UserFnPath {
    pub parent: Option<Ident>,
    pub ident: UserFnIdent,
}

impl UserFnPath {
    pub const fn parent_namespace(self) -> Option<NamespaceIdent> {
        match self.parent {
            Some(parent_ident) => Some(NamespaceIdent {
                kind: UserFnIdent::PARENT_NAMESPACE_KIND,
                ident: parent_ident,
            }),
            None => None,
        }
    }
}

impl fmt::Display for UserFnPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(parent_namespace_ident) = self.parent_namespace() {
            write!(f, "{}::", parent_namespace_ident)?;
        }
        write!(f, "{}", self.ident)
    }
}

impl From<UserFnIdent> for UserFnPath {
    fn from(user_fn_ident: UserFnIdent) -> Self {
        UserFnPath {
            parent: None,
            ident: user_fn_ident,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Fn_{}", ident)]
pub struct UserFnIdent {
    pub ident: Ident,
}

impl UserFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Impl;
}

#[derive(Clone, Copy, Debug, Display)]
#[display(fmt = "{}::{}", "self.parent_namespace()", ident)]
pub struct OpFnPath {
    pub parent: Ident,
    pub ident: OpFnIdent,
}

impl OpFnPath {
    pub const fn parent_namespace(self) -> NamespaceIdent {
        NamespaceIdent {
            kind: OpFnIdent::PARENT_NAMESPACE_KIND,
            ident: self.parent,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "Op_{}", ident)]
pub struct OpFnIdent {
    pub ident: Ident,
}

impl OpFnIdent {
    pub const PARENT_NAMESPACE_KIND: NamespaceKind = NamespaceKind::Impl;
}

#[derive(Clone, Debug, From)]
pub struct Code {
    pub items: Vec<Item>,
}

impl fmt::Display for Code {
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
    Comment(CommentItem),
    Namespace(NamespaceItem),
    Fn(FnItem),
}

#[derive(Clone, Debug)]
pub struct CommentItem {
    pub text: String,
}

impl fmt::Display for CommentItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(IndentWriter::new("// ", f), "{}", self.text)
    }
}

#[derive(Clone, Debug)]
pub struct NamespaceItem {
    pub ident: NamespaceIdent,
    pub items: Vec<Item>,
}

impl fmt::Display for NamespaceItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "namespace {} {{\n\n", self.ident)?;
        fmt_join_trailing(f, "\n\n", self.items.iter())?;
        write!(f, "}};  // namespace {}", self.ident)
    }
}

#[derive(Clone, Debug)]
pub struct FnItem {
    pub path: FnPath,
    pub is_fully_qualified: bool,
    pub is_inline: bool,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Option<Block>,
}

impl fmt::Display for FnItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_inline {
            write!(f, "inline ")?;
        }
        write!(f, "{} ", self.ret)?;

        if self.is_fully_qualified {
            write!(f, "{}", self.path)?;
        } else {
            write!(f, "{}", self.path.ident())?;
        }

        write!(f, "(")?;
        fmt_join(f, ", ", self.params.iter())?;
        write!(f, ")")?;

        match &self.body {
            None => write!(f, ";"),
            Some(body) => write!(f, " {}", body),
        }
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{} {}", type_, ident)]
pub struct Param {
    pub ident: ParamIdent,
    pub type_: Type,
}

#[derive(Clone, Copy, Debug, Display, From)]
pub enum ParamIdent {
    #[display(fmt = "cx")]
    Context,
    IrContext(IrContextParamIdent),
    User(UserParamIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum IrContextParamIdent {
    #[display(fmt = "interpreter")]
    Interpreter,
    #[display(fmt = "compiler")]
    Compiler,
}

impl IrContextParamIdent {
    pub fn type_ident(self) -> IrMemberTypeIdent {
        match self {
            IrContextParamIdent::Interpreter => IrMemberTypeIdent::InterpreterRef,
            IrContextParamIdent::Compiler => IrMemberTypeIdent::CompilerRef,
        }
    }
}

#[derive(Clone, Copy, Debug, Display, From)]
#[display(fmt = "param_{}", ident)]
pub struct UserParamIdent {
    pub ident: Ident,
}

#[derive(Clone, Debug, From)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{\n")?;

        let mut indent_writer = IndentWriter::new("  ", f);
        fmt_join_trailing(&mut indent_writer, "\n", self.stmts.iter())?;
        let f = indent_writer.into_inner();

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
    #[from(types(Block, "Vec<Stmt>"))]
    Block(BlockStmt),
    #[from]
    Let(LetStmt),
    #[from]
    If(IfStmt),
    #[from]
    Ret(RetStmt),
    #[from(types(Expr))]
    Expr(ExprStmt),
}

impl FromIterator<Stmt> for Stmt {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        Block::from_iter(iter).into()
    }
}

#[derive(Clone, Debug, Display, From)]
#[from(types("Vec<Stmt>"))]
pub struct BlockStmt {
    pub block: Block,
}

impl FromIterator<Stmt> for BlockStmt {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        Block::from_iter(iter).into()
    }
}

#[derive(Clone, Debug)]
pub struct LetStmt {
    pub lhs: VarIdent,
    pub type_: Type,
    pub rhs: Option<Expr>,
}

impl fmt::Display for LetStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {}", self.type_, self.lhs)?;
        if let Some(rhs) = &self.rhs {
            write!(f, "({})", rhs)?;
        }
        write!(f, ";")
    }
}

#[derive(Clone, Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub then: Block,
    pub else_: Option<Block>,
}

impl fmt::Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "if ({}) {}", self.cond, self.then)?;
        if let Some(else_) = &self.else_ {
            write!(f, " else {}", else_)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct RetStmt {
    pub value: Option<Expr>,
}

impl fmt::Display for RetStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "return")?;
        if let Some(value) = &self.value {
            write!(f, " {}", value)?;
        }
        write!(f, ";")
    }
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "{};", expr)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Clone, Debug, Display, From)]
pub enum Expr {
    #[from(types(Block, "Vec<Stmt>"))]
    Block(BlockExpr),
    #[from(types(
        ParamIdent,
        IrContextParamIdent,
        UserParamIdent,
        LocalVarIdent,
        LocalLabelVarIdent,
        SyntheticVarIdent
    ))]
    Var(VarIdent),
    #[from(types(
        HelperFnIdent,
        TypeMemberFnPath,
        IrMemberFnPath,
        VariantFnPath,
        GlobalVarFnPath,
        UserFnPath,
        OpFnPath
    ))]
    Fn(FnPath),
    #[from]
    Template(Box<TemplateExpr>),
    #[from]
    Member(Box<MemberExpr>),
    #[from]
    Call(Box<CallExpr>),
    #[from]
    Cast(Box<CastExpr>),
    #[from]
    Negate(Box<NegateExpr>),
    #[from]
    Compare(Box<CompareExpr>),
    #[from]
    Assign(Box<AssignExpr>),
    #[from]
    Comma(Box<CommaExpr>),
}

impl FromIterator<Stmt> for Expr {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        Block::from_iter(iter).into()
    }
}

struct MaybeGrouped<'a>(&'a Expr);

impl fmt::Display for MaybeGrouped<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let needs_group = match self.0 {
            // Note: BlockExpr and CommaExpr are always inherently grouped for
            // C++ syntax reasons, so there's no need for us to group them again
            // here.
            Expr::Block(_)
            | Expr::Var(_)
            | Expr::Fn(_)
            | Expr::Template(_)
            | Expr::Member(_)
            | Expr::Call(_)
            | Expr::Negate(_)
            | Expr::Comma(_) => false,
            Expr::Compare(_) | Expr::Assign(_) => true,
            Expr::Cast(cast_expr) => match cast_expr.kind {
                CastStyle::Functional(_) => false,
                CastStyle::C => true,
            },
        };

        if needs_group {
            write!(f, "({})", self.0)
        } else {
            fmt::Display::fmt(self.0, f)
        }
    }
}

#[derive(Debug, Display, EnumSetType, Hash, Ord, PartialOrd)]
pub enum ExprTag {
    #[display(fmt = "MutRef")]
    MutRef,
    #[display(fmt = "Ref")]
    Ref,
    #[display(fmt = "Local")]
    Local,
    #[display(fmt = "Val")]
    Val,
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "({})", block)]
#[from(types("Vec<Stmt>"))]
pub struct BlockExpr {
    pub block: Block,
}

impl FromIterator<Stmt> for BlockExpr {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Stmt>,
    {
        Block::from_iter(iter).into()
    }
}

#[derive(Clone, Debug)]
pub struct TemplateExpr {
    pub inner: Expr,
    pub args: Vec<Type>,
}

impl fmt::Display for TemplateExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}<", self.inner)?;
        fmt_join(f, ", ", self.args.iter())?;
        write!(f, ">")
    }
}

impl From<TemplateExpr> for Expr {
    fn from(template_expr: TemplateExpr) -> Self {
        Box::new(template_expr).into()
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}.{}", "MaybeGrouped(&self.parent)", member)]
pub struct MemberExpr {
    pub parent: Expr,
    pub member: Ident,
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

impl fmt::Display for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}(", MaybeGrouped(&self.target))?;
        fmt_join(f, ", ", self.args.iter())?;
        write!(f, ")")
    }
}

impl From<CallExpr> for Expr {
    fn from(call_expr: CallExpr) -> Self {
        Box::new(call_expr).into()
    }
}

#[derive(Clone, Debug)]
pub struct CastExpr {
    pub kind: CastStyle,
    pub expr: Expr,
    pub type_: Type,
}

impl fmt::Display for CastExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            CastStyle::Functional(kind) => write!(f, "{}<{}>({})", kind, self.type_, self.expr),
            CastStyle::C => write!(f, "({}) {}", self.type_, MaybeGrouped(&self.expr)),
        }
    }
}

impl From<CastExpr> for Expr {
    fn from(cast_expr: CastExpr) -> Self {
        Box::new(cast_expr).into()
    }
}

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum CastStyle {
    Functional(FunctionalCastStyle),
    C,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, PartialEq)]
pub enum FunctionalCastStyle {
    #[display(fmt = "static_cast")]
    Static,
    #[display(fmt = "dynamic_cast")]
    Dynamic,
    #[display(fmt = "const_cast")]
    Const,
    #[display(fmt = "reinterpret_cast")]
    Reinterpret,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}{}", kind, "MaybeGrouped(&self.expr)")]
pub struct NegateExpr {
    pub kind: NegateExprKind,
    pub expr: Expr,
}

impl From<NegateExpr> for Expr {
    fn from(negate_expr: NegateExpr) -> Self {
        Box::new(negate_expr).into()
    }
}

#[derive(Clone, Debug, Display)]
#[display(
    fmt = "{} {} {}",
    "MaybeGrouped(&self.lhs)",
    kind,
    "MaybeGrouped(&self.rhs)"
)]
pub struct CompareExpr {
    pub kind: CompareExprKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl From<CompareExpr> for Expr {
    fn from(compare_expr: CompareExpr) -> Self {
        Box::new(compare_expr).into()
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{} = {}", "MaybeGrouped(&self.lhs)", "MaybeGrouped(&self.rhs)")]
pub struct AssignExpr {
    pub lhs: Expr,
    pub rhs: Expr,
}

impl From<AssignExpr> for Expr {
    fn from(assign_expr: AssignExpr) -> Self {
        Box::new(assign_expr).into()
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "({}, {})", "MaybeGrouped(&self.lhs)", "MaybeGrouped(&self.rhs)")]
pub struct CommaExpr {
    pub lhs: Expr,
    pub rhs: Expr,
}

impl From<CommaExpr> for Expr {
    fn from(comma_expr: CommaExpr) -> Self {
        Box::new(comma_expr).into()
    }
}