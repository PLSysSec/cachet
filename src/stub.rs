use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser, "/stub/parser.rs");

pub fn parse_stub(stub_txt: &str) -> Result<Stub, String> {
    let p = parser::StubParser::new();
    p.parse(stub_txt).map_err(|e| e.to_string())
}

pub struct Stub {
    pub instrs: Vec<Instr>,
    pub context: Vec<ContextInfo>,
}

pub struct Instr {
    pub name: String,
    pub args: Vec<Arg>,
}

pub struct Arg {
    pub type_: String,
    pub name: String,
    pub idx: usize,
}

pub struct ContextInfo {
    pub type_: String,
    pub idx: usize,
    // TODO: Handle parsing hex vs decimal may require modifying the stub emitter
    pub value: String,
}
