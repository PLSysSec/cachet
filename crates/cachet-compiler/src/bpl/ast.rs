// vim: set tw=99 ts=4 sts=4 sw=4 et:

#![allow(dead_code)]

use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug, Display, Write};
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};

use derive_more::{Display, From};
use enum_map::Enum;

use cachet_lang::ast::{
    ArithBinOper, BitwiseBinOper, CastKind, CheckKind, CompareBinOper, Ident, LogicalBinOper,
    NegateKind, NumericCompareBinOper, Path,
};
pub use cachet_lang::normalizer::{LocalCallIndex, LocalLabelIndex, LocalVarIndex};
use cachet_util::{box_from, fmt_join, fmt_join_leading, fmt_join_trailing, AffixWriter};

#[derive(Clone, Copy, Display, Eq, From, PartialEq)]
#[display(fmt = "${}", _0)]
pub struct UserIdent(pub Ident);

impl Debug for UserIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Clone, Copy, Eq, From, PartialEq)]
#[from(types(Ident))]
pub struct PathIdent(pub Path);

impl Debug for PathIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Debug::fmt(&format!("{}", self), f)
    }
}

impl Display for PathIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for ident in self.0.into_iter() {
            write!(f, "{}", UserIdent(ident))?;
        }
        Ok(())
    }
}

impl From<UserIdent> for PathIdent {
    fn from(UserIdent(ident): UserIdent) -> Self {
        ident.into()
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[from(types(Ident, Path, UserIdent))]
pub struct GlobalVarPathIdent(pub PathIdent);

#[derive(Clone, Debug, Display, From)]
pub enum Type {
    #[from(types(
        NativeTypeIdent,
        BitVecTypeIdent,
        FloatTypeIdent,
        PreludeTypeIdent,
        TypeMemberTypeIdent,
        IrMemberTypeIdent,
        UserTypeIdent
    ))]
    Ident(TypeIdent),
    #[from]
    Map(Box<MapType>),
}

box_from!(MapType => Type);

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum TypeIdent {
    #[from(types(BitVecTypeIdent, FloatTypeIdent))]
    Native(NativeTypeIdent),
    #[from]
    Prelude(PreludeTypeIdent),
    #[from]
    TypeMember(TypeMemberTypeIdent),
    #[from]
    IrMember(IrMemberTypeIdent),
    #[from]
    User(UserTypeIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum NativeTypeIdent {
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "int")]
    Int,
    BitVec(BitVecTypeIdent),
    Float(FloatTypeIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
pub enum PreludeTypeIdent {
    #[display(fmt = "CallPath")]
    CallPath,
    #[display(fmt = "Pc")]
    Pc,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}{}", type_ident, selector)]
pub struct TypeMemberTypeIdent {
    pub type_ident: UserTypeIdent,
    pub selector: TypeMemberTypeSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
pub enum TypeMemberTypeSelector {
    #[display(fmt = "^Ref")]
    Ref,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}{}", ir_ident, selector)]
pub struct IrMemberTypeIdent {
    pub ir_ident: UserIdent,
    pub selector: IrMemberTypeSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
pub enum IrMemberTypeSelector {
    #[display(fmt = "^Op")]
    Op,
    #[display(fmt = "^Label")]
    Label,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "bv{}", width)]
pub struct BitVecTypeIdent {
    pub width: u8,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "float{}e{}", sig_width, exp_width)]
pub struct FloatTypeIdent {
    pub sig_width: u8,
    pub exp_width: u8,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum UserTypeIdent {
    #[display(fmt = "Struct")]
    Struct,
    #[from(types(Ident))]
    Other(UserIdent),
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

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum ConstIdent {
    GlobalVar(GlobalVarPathIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum VarIdent {
    #[from]
    TypeMember(TypeMemberVarIdent),
    #[from]
    IrMember(IrMemberVarIdent),
    #[from(types(UserParamVarIdent))]
    Param(ParamVarIdent),
    #[display(fmt = "ret")]
    Ret,
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

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}{}", type_ident, selector)]
pub struct TypeMemberVarIdent {
    pub type_ident: UserTypeIdent,
    pub selector: TypeMemberVarSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
pub enum TypeMemberVarSelector {
    #[display(fmt = "^refs")]
    Refs,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}{}", ir_ident, selector)]
pub struct IrMemberVarIdent {
    pub ir_ident: UserIdent,
    pub selector: IrMemberVarSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
pub enum IrMemberVarSelector {
    #[display(fmt = "^pc")]
    Pc,
    #[display(fmt = "^ops")]
    Ops,
    #[display(fmt = "^pcEmitCallPaths")]
    PcEmitCallPaths,
    #[display(fmt = "^nextLabel")]
    NextLabel,
    #[display(fmt = "^labelPcs")]
    LabelPcs,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum ParamVarIdent {
    #[display(fmt = "callPath")]
    CallPath,
    #[display(fmt = "frame")]
    Frame,
    #[display(fmt = "in")]
    In,
    #[display(fmt = "init")]
    Init,
    #[display(fmt = "instance")]
    Instance,
    #[display(fmt = "label")]
    Label,
    #[display(fmt = "last")]
    Last,
    #[display(fmt = "localCallIndex")]
    LocalCallIndex,
    #[display(fmt = "op")]
    Op,
    User(UserParamVarIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "{}'p", ident)]
#[from(types(Ident))]
pub struct UserParamVarIdent {
    pub ident: UserIdent,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}'v{}", ident, index)]
pub struct LocalVarIdent {
    pub ident: UserIdent,
    pub index: LocalVarIndex,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}'l{}", ident, index)]
pub struct LocalLabelVarIdent {
    pub ident: UserIdent,
    pub index: LocalLabelIndex,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}'{}", kind, index)]
pub struct SyntheticVarIdent {
    pub kind: SyntheticVarKind,
    pub index: usize,
}

#[derive(Clone, Copy, Debug, Display, Enum, Eq, PartialEq)]
pub enum SyntheticVarKind {
    #[display(fmt = "ret")]
    Ret,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum FnIdent {
    #[display(fmt = "init")]
    Init,
    Prelude(PreludeFnIdent),
    TypeMember(TypeMemberFnIdent),
    IrMember(IrMemberFnIdent),
    User(UserFnIdent),
    Helper(HelperFnIdent),
    OpCtorField(OpCtorFieldFnIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum PreludeFnIdent {
    #[display(fmt = "NilCallPath")]
    NilCallPathCtor,
    #[display(fmt = "ConsCallPath")]
    ConsCallPathCtor,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}#{}{}", param_var_ident, ir_ident, op_ctor_selector)]
pub struct OpCtorFieldFnIdent {
    pub param_var_ident: ParamVarIdent,
    pub ir_ident: UserIdent,
    pub op_ctor_selector: OpCtorIrMemberFnSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}{}", type_ident, selector)]
pub struct TypeMemberFnIdent {
    pub type_ident: UserTypeIdent,
    pub selector: TypeMemberFnSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum TypeMemberFnSelector {
    #[display(fmt = "^negate")]
    Negate,
    #[from(types(RefCtorSelector))]
    RefCtor(RefCtorTypeMemberFnSelector),
    #[from]
    Cast(CastTypeMemberFnSelector),
    #[from]
    Variant(VariantCtorTypeMemberFnSelector),
    #[from]
    Field(FieldTypeMemberFnSelector),
    #[from]
    BinOper(BinOperTypeMemberFnSelector),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "^Ref{selector}")]
pub struct RefCtorTypeMemberFnSelector {
    pub selector: RefCtorSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum RefCtorSelector {
    #[display(fmt = "^Local")]
    Local,
    EnumVariant(EnumVariantRefCtorSelector),
    GlobalVar(GlobalVarPathIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "{enum_ident}{variant_selector}")]
pub struct EnumVariantRefCtorSelector {
    pub enum_ident: UserTypeIdent,
    pub variant_selector: VariantCtorTypeMemberFnSelector,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CastTypeMemberFnSelector {
    pub kind: CastKind,
    pub supertype_ident: UserTypeIdent,
}

impl Display for CastTypeMemberFnSelector {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "^{}{}",
            match self.kind {
                CastKind::Downcast => "from",
                CastKind::Upcast => "to",
            },
            self.supertype_ident
        )?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "^Variant{}", variant_ident)]
#[from(types(Ident))]
pub struct VariantCtorTypeMemberFnSelector {
    pub variant_ident: UserIdent,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "^field{}", field_ident)]
#[from(types(Ident))]
pub struct FieldTypeMemberFnSelector {
    pub field_ident: UserIdent,
}

#[derive(Clone, Copy, Debug, Eq, From, PartialEq)]
pub enum BinOperTypeMemberFnSelector {
    Arith(ArithBinOper),
    Bitwise(BitwiseBinOper),
    NumericCompare(NumericCompareBinOper),
}

impl Display for BinOperTypeMemberFnSelector {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "^{}",
            match self {
                BinOperTypeMemberFnSelector::Arith(arith_bin_oper) => match arith_bin_oper {
                    ArithBinOper::Mul => "mul",
                    ArithBinOper::Div => "div",
                    ArithBinOper::Add => "add",
                    ArithBinOper::Sub => "sub",
                },
                BinOperTypeMemberFnSelector::Bitwise(bitwise_bin_oper) => match bitwise_bin_oper {
                    BitwiseBinOper::Shl => "shl",
                    BitwiseBinOper::And => "bitAnd",
                    BitwiseBinOper::Xor => "xor",
                    BitwiseBinOper::Or => "bitOr",
                },
                BinOperTypeMemberFnSelector::NumericCompare(numeric_compare_bin_oper) =>
                    match numeric_compare_bin_oper {
                        NumericCompareBinOper::Lt => "lt",
                        NumericCompareBinOper::Gt => "gt",
                        NumericCompareBinOper::Lte => "lte",
                        NumericCompareBinOper::Gte => "gte",
                    },
            }
        )?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{}{}", ir_ident, selector)]
pub struct IrMemberFnIdent {
    pub ir_ident: UserIdent,
    pub selector: IrMemberFnSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum IrMemberFnSelector {
    #[display(fmt = "^step")]
    Step,
    #[display(fmt = "^emit")]
    Emit,
    #[display(fmt = "^label")]
    Label,
    #[display(fmt = "^bind")]
    Bind,
    #[display(fmt = "^bindExit")]
    BindExit,
    #[display(fmt = "^goto")]
    Goto,
    OpCtor(OpCtorIrMemberFnSelector),
    EntryPoint(EntryPointIrMemberFnSelector),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "^Op{}", op_selector)]
#[from(types(Ident, UserIdent))]
pub struct OpCtorIrMemberFnSelector {
    pub op_selector: OpSelector,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
pub enum OpSelector {
    #[display(fmt = "^Exit")]
    Exit,
    #[from(types(Ident))]
    User(UserIdent),
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "^entryPoint{op_ident}")]
#[from(types(Ident))]
pub struct EntryPointIrMemberFnSelector {
    pub op_ident: UserIdent,
}

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[from(types(Ident, Path, UserIdent))]
pub struct UserFnIdent(pub PathIdent);

#[derive(Clone, Copy, Debug, Display, Eq, From, PartialEq)]
#[display(fmt = "{fn_ident}^{var_ident}")]
pub struct HelperFnIdent {
    pub fn_ident: UserFnIdent,
    pub var_ident: VarIdent,
}

#[derive(Clone, Debug, Display, Eq, From, PartialEq)]
pub enum LabelIdent {
    Emit(EmitLabelIdent),
}

#[derive(Clone, Debug, Default, Eq, From, PartialEq)]
pub struct EmitLabelIdent {
    pub call_path: CallPath,
}

impl Display for EmitLabelIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        #[derive(Display)]
        #[display(fmt = "{}{}", "_0.local_call_index", "_0.target")]
        struct EmitLabelSegment<'a>(&'a CallSegment);

        write!(f, "emit")?;
        if self.call_path.is_empty() {
            write!(f, "{}", OpSelector::Exit)?;
        } else {
            fmt_join_leading(f, "'", self.call_path.iter().map(EmitLabelSegment))?;
        }
        Ok(())
    }
}

impl FromIterator<CallSegment> for EmitLabelIdent {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = CallSegment>,
    {
        CallPath::from_iter(iter).into()
    }
}

pub type CallPath = Vec<CallSegment>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CallSegment {
    pub local_call_index: LocalCallIndex,
    pub target: UserFnIdent,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}: {}", ident, type_)]
pub struct TypedVar<I = VarIdent> {
    pub ident: I,
    pub type_: Type,
}

#[derive(Clone, Debug, From)]
pub struct TypedVars<I = VarIdent> {
    pub vars: Vec<TypedVar<I>>,
}

impl<I> AsRef<Vec<TypedVar<I>>> for TypedVars<I> {
    fn as_ref(&self) -> &Vec<TypedVar<I>> {
        &*self
    }
}

impl<I> AsMut<Vec<TypedVar<I>>> for TypedVars<I> {
    fn as_mut(&mut self) -> &mut Vec<TypedVar<I>> {
        &mut *self
    }
}

impl<I> Borrow<Vec<TypedVar<I>>> for TypedVars<I> {
    fn borrow(&self) -> &Vec<TypedVar<I>> {
        &*self
    }
}

impl<I> BorrowMut<Vec<TypedVar<I>>> for TypedVars<I> {
    fn borrow_mut(&mut self) -> &mut Vec<TypedVar<I>> {
        &mut *self
    }
}

impl<I> Default for TypedVars<I> {
    fn default() -> Self {
        TypedVars {
            vars: Vec::default(),
        }
    }
}

impl<I> Deref for TypedVars<I> {
    type Target = Vec<TypedVar<I>>;

    fn deref(&self) -> &Self::Target {
        &self.vars
    }
}

impl<I> DerefMut for TypedVars<I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vars
    }
}

impl<I: Display> Display for TypedVars<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join(f, ", ", self.iter())?;
        Ok(())
    }
}

impl<I> Extend<TypedVar<I>> for TypedVars<I> {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = TypedVar<I>>,
    {
        self.vars.extend(iter);
    }
}

impl<I> FromIterator<TypedVar<I>> for TypedVars<I> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = TypedVar<I>>,
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
    Comment(CommentItem),
    Type(TypeItem),
    Const(ConstItem),
    Var(VarItem),
    Axiom(AxiomItem),
    Fn(FnItem),
    Proc(ProcItem),
}

#[derive(Clone, Debug)]
pub struct CommentItem {
    pub text: String,
}

impl Display for CommentItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(AffixWriter::new(f, "// ", ""), "{}", self.text)
    }
}

impl<T: Into<String>> From<T> for CommentItem {
    fn from(text: T) -> Self {
        CommentItem { text: text.into() }
    }
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
    pub var: TypedVar<ConstIdent>,
}

#[derive(Clone, Debug, Display, From)]
#[display(fmt = "var {};", var)]
pub struct VarItem {
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
                write!(f, " {{\n")?;

                let mut indented = AffixWriter::new(f, "  ", "");
                write!(indented, "{}", value)?;
                let f = indented.into_inner();

                write!(f, "\n}}")?;
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
    #[from(types(GlobalVarPathIdent))]
    Const(ConstIdent),
    #[from(types(
        TypeMemberVarIdent,
        IrMemberVarIdent,
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
    BinOper(Box<BinOperExpr>),
    #[from]
    ForAll(Box<ForAllExpr>),
}

box_from!(IndexExpr => Expr);
box_from!(NegateExpr => Expr);
box_from!(BinOperExpr => Expr);
box_from!(ForAllExpr => Expr);

struct MaybeGrouped<'a>(&'a Expr);

impl Display for MaybeGrouped<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let needs_group = match self.0 {
            Expr::Literal(_) | Expr::Const(_) | Expr::Var(_) | Expr::Index(_) | Expr::Call(_) => {
                false
            }
            Expr::Negate(_) | Expr::BinOper(_) | Expr::ForAll(_) => true,
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
    #[display(fmt = "{:?}bv16", _0)]
    Bv16(u16),
    #[display(fmt = "{:?}bv32", _0)]
    Bv32(u32),
    #[display(fmt = "{:?}bv64", _0)]
    Bv64(u64),
    #[display(fmt = "{}", "fmt_f64(*_0)")]
    Float64(f64),
}

fn fmt_f64(f: f64) -> String {
    const SIGNIFICAND_SIZE: u8 = 53;
    const EXPONENT_SIZE: u8 = 11;

    if f.is_nan() {
        // Don't forward the NaN code.
        format!("0NaN{SIGNIFICAND_SIZE}e{EXPONENT_SIZE}")
    } else if f == f64::INFINITY {
        format!("0+oo{SIGNIFICAND_SIZE}e{EXPONENT_SIZE}")
    } else if f == f64::NEG_INFINITY {
        format!("0-oo{SIGNIFICAND_SIZE}e{EXPONENT_SIZE}")
    } else {
        let bits: u64 = f.to_bits();
        let sign = if bits >> 63 == 0 { "" } else { "-" };
        let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
        let mut mantissa = if exponent == 0 {
            (bits & 0xfffffffffffff) << 1
        } else {
            (bits & 0xfffffffffffff) | 0x10000000000000
        };

        const EXPONENT_BIAS: i16 = 1023;
        const MANTISSA_SHIFT: i16 = 52;
        exponent -= EXPONENT_BIAS + MANTISSA_SHIFT;

        // If Boogie had a sane floating point implementation, we would stop
        // here. But while a normal floating point representation is m * 2 ^ e,
        // Boogie uses m * 16 ^ e, which means we need to divide e by four.
        // Except that would leave us with a fractional exponent, which is
        // verboten. So we split the fractional bit off and get:
        //
        // m * 16 ^ ((e % 4) / 4) * 16 ^ (e/4)
        // m * 2  ^  (e % 4)      * 16 ^ (e/4)
        //
        // or:
        //
        // (m << (e % 4)) * 16 ^ (e/4)
        //
        // This is why we use an i128 for the mantissa, to allow us to freely
        // shift left (at most 3). But what about when e is negative? We could
        // shift right, but this could result in data loss. Instead, we shift
        // left and decrement the exponent just enough so that it's evenly
        // divisible by 4.

        let shift = exponent % 4;

        if shift >= 0 {
            mantissa = mantissa << shift.abs();
        } else {
            let diff = 4 + shift;
            mantissa = mantissa << diff;
            exponent -= diff;
        }

        exponent /= 4;

        format!("{sign}0x{mantissa:x}.0e{exponent}f53e11")
    }
}

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
    oper,
    "MaybeGrouped(&self.rhs)"
)]
pub struct BinOperExpr {
    pub oper: BplBinOper,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Copy, Debug, Display, Eq, Hash, From, PartialEq)]
pub enum BplBinOper {
    #[from]
    Arith(ArithBinOper),
    #[from(types(NumericCompareBinOper))]
    Compare(CompareBinOper),
    #[from]
    Logical(LogicalBinOper),
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "forall {} :: {}", vars, "MaybeGrouped(&self.expr)")]
pub struct ForAllExpr {
    pub vars: TypedVars,
    pub expr: Expr,
}
