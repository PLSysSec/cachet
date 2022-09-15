// vim: set tw=99 ts=4 sts=4 sw=4 et:

use derive_more::{Display, From};
use strum_macros::{EnumIter, IntoStaticStr};

pub use cachet_lang::ast::Ident;

pub type Word = u64;
pub type Addr = Word;

#[derive(Clone, Debug)]
pub struct Stub {
    pub kind: Ident,
    pub engine: Engine,
    pub input_operands: Vec<InputOperand>,
    pub ops: Vec<Op>,
    pub fields: Vec<Field>,
    pub facts: Vec<Fact>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Engine {
    Baseline,
    IonIC,
}

pub type InputOperand = Arg<OperandId>;

#[derive(Clone, Debug)]
pub struct Op {
    pub ident: Ident,
    pub args: Vec<OpArg>,
}

#[derive(Clone, Debug)]
pub struct Arg<T> {
    pub ident: Ident,
    pub data: T,
}

pub type OpArg = Arg<OpArgData>;

#[derive(Clone, Debug, From)]
pub enum OpArgData {
    OperandId(OperandId),
    FieldOffset(FieldOffset),
    ImmValue(ImmValue),
}

impl OpArgData {
    pub const fn type_(&self) -> OpArgType {
        match self {
            Self::OperandId(data) => OpArgType::OperandId(data.type_),
            Self::FieldOffset(data) => OpArgType::FieldOffset(data.type_),
            Self::ImmValue(data) => OpArgType::ImmValue(data.type_()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct OperandId {
    pub id: u16,
    pub type_: OperandType,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct FieldOffset {
    pub offset: u32,
    pub type_: FieldType,
}

#[derive(Clone, Copy, Debug, Eq, From, Hash, PartialEq)]
pub enum OpArgType {
    OperandId(OperandType),
    FieldOffset(FieldType),
    ImmValue(ImmType),
}

#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, IntoStaticStr, PartialEq)]
pub enum OperandType {
    ValueTag,
    Val,
    Obj,
    String,
    Symbol,
    Boolean,
    Int32,
    Number,
    BigInt,
    IntPtr,
    Raw,
}

#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, IntoStaticStr, PartialEq)]
pub enum FieldType {
    Shape,
    GetterSetter,
    Object,
    String,
    Atom,
    PropertyName,
    Symbol,
    BaseScript,
    RawInt32,
    RawPointer,
    Id,
    Value,
    RawInt64,
    AllocSite,
}

#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, IntoStaticStr, PartialEq)]
pub enum ImmType {
    JSOp,
    Bool,
    Byte,
    GuardClassKind,
    ValueType,
    JSWhyMagic,
    CallFlags,
    ScalarType,
    UnaryMathFunction,
    WasmValType,
    Int32,
    UInt32,
    JSNative,
    //StaticString,
    AllocKind,
}

#[derive(Clone, Debug, From)]
pub enum ImmValue {
    JSOp(Ident),
    Bool(bool),
    Byte(u8),
    GuardClassKind(Ident),
    ValueType(Ident),
    JSWhyMagic(Ident),
    #[from]
    CallFlags(CallFlags),
    ScalarType(Ident),
    UnaryMathFunction(Ident),
    WasmValType(Ident),
    Int32(i32),
    UInt32(u32),
    JSNative(Addr),
    // TODO(spinda): We don't have a string representation in Cachet yet.
    //StaticString(String),
    AllocKind(Ident),
}

impl ImmValue {
    pub const fn type_(&self) -> ImmType {
        match self {
            Self::JSOp(_) => ImmType::JSOp,
            Self::Bool(_) => ImmType::Bool,
            Self::Byte(_) => ImmType::Byte,
            Self::GuardClassKind(_) => ImmType::GuardClassKind,
            Self::ValueType(_) => ImmType::ValueType,
            Self::JSWhyMagic(_) => ImmType::JSWhyMagic,
            Self::CallFlags(_) => ImmType::CallFlags,
            Self::ScalarType(_) => ImmType::ScalarType,
            Self::UnaryMathFunction(_) => ImmType::UnaryMathFunction,
            Self::WasmValType(_) => ImmType::WasmValType,
            Self::Int32(_) => ImmType::Int32,
            Self::UInt32(_) => ImmType::UInt32,
            Self::JSNative(_) => ImmType::JSNative,
            //Self::StaticString(_) => ImmType::StaticString,
            Self::AllocKind(_) => ImmType::AllocKind,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CallFlags {
    pub arg_format: Ident,
    pub is_constructing: bool,
    pub is_same_realm: bool,
    pub needs_uninitialized_this: bool,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub offset: u32,
    pub data: FieldData,
}

impl Field {
    pub const fn type_(&self) -> FieldType {
        self.data.type_()
    }
}

#[derive(Clone, Debug)]
pub enum FieldData {
    Shape(Addr),
    GetterSetter(Addr),
    Object(Addr),
    String(Addr),
    Atom(Addr),
    PropertyName(Word),
    Symbol(Addr),
    BaseScript(Addr),
    RawInt32(i32),
    RawPointer(Addr),
    Id(Word),
    Value(u64),
    RawInt64(i64),
    AllocSite(Addr),
}

impl FieldData {
    pub const fn type_(&self) -> FieldType {
        match self {
            Self::Shape(_) => FieldType::Shape,
            Self::GetterSetter(_) => FieldType::GetterSetter,
            Self::Object(_) => FieldType::Object,
            Self::String(_) => FieldType::String,
            Self::Atom(_) => FieldType::Atom,
            Self::PropertyName(_) => FieldType::PropertyName,
            Self::Symbol(_) => FieldType::Symbol,
            Self::BaseScript(_) => FieldType::BaseScript,
            Self::RawInt32(_) => FieldType::RawInt32,
            Self::RawPointer(_) => FieldType::RawPointer,
            Self::Id(_) => FieldType::Id,
            Self::Value(_) => FieldType::Value,
            Self::RawInt64(_) => FieldType::RawInt64,
            Self::AllocSite(_) => FieldType::AllocSite,
        }
    }
}

#[derive(Clone, Debug, From)]
pub enum Fact {
    ShapeClass(ShapeClassFact),
    ShapeNumFixedSlots(ShapeNumFixedSlotsFact),
    ShapeSlotSpan(ShapeSlotSpanFact),
    ClassIsNativeObjectFact(ClassIsNativeObjectFact),
}

#[derive(Clone, Debug)]
pub struct ShapeClassFact {
    pub shape: Addr,
    pub class: Addr,
}

#[derive(Clone, Debug)]
pub struct ShapeNumFixedSlotsFact {
    pub shape: Addr,
    pub num_fixed_slots: u32,
}

#[derive(Clone, Debug)]
pub struct ShapeSlotSpanFact {
    pub shape: Addr,
    pub slot_span: u32,
}

#[derive(Clone, Debug)]
pub struct ClassIsNativeObjectFact {
    pub class: Addr,
}
