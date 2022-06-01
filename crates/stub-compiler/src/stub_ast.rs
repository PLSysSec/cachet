use derive_more::Display;

#[derive(Debug)]
pub struct IC {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub facts: Vec<Fact>
}

#[derive(Debug)]
pub struct Instruction {
    pub name: String,
    pub args: Vec<Arg>
}

#[derive(Debug)]
pub struct Arg {
    pub type_: Type,
    pub hint: String,
    pub value: Value 
}

#[derive(Debug, Display)]
pub enum Value {
    Index(usize),
    Bool(bool)
}

#[derive(Debug)]
pub enum Fact {
    ShapeNumFixedSlots { address: usize, num_slots: usize },
    ShapeSlotSpan { address: usize, span: usize },
    Field { kind: FieldKind, offset: usize, address: usize }
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
pub enum Type {
    Field(FieldKind),
    Id(String)
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash)]
pub enum FieldKind {
    Shape,
    RawInt32,
    String,
    JSObject
}