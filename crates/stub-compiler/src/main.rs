use lalrpop_util::lalrpop_mod;
use std::{io::{stdin, Read}, collections::HashSet};
lalrpop_mod!(grammar, "/grammar.rs");

pub mod stub_ast;
use stub_ast::*;

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let IC = grammar::ICParser::new();
    println!("{}", compile(IC.parse(&input).unwrap()));
}


fn var_from_arg(arg: &Arg) -> String {
    format!("{}_{}", arg.type_, arg.value)
}

fn collect_vars(ic: &IC) -> HashSet<(String, Type)> {
    ic.instructions.iter().flat_map(|i| {
        i.args.iter().map(|a| (var_from_arg(a), a.type_.clone()))
    }).collect()
}

fn compile(ic: IC) -> String {
    let var_decls = {
        let mut out = String::new();
        for (name, type_) in collect_vars(&ic) {
            out += &format!("{}: {},\n", name, type_);
        }
        out
    };

    let emits = {
        let mut out = String::new();

        for instr in &ic.instructions {
            let name = &instr.name;
            let args = {
                let mut out = String::new();
                for a in &instr.args {
                    let name = var_from_arg(a);
                    out += &format!("{name},");
                }
                out
            };
            out += &format!("emit CacheIR::{name}({args});\n");
        }
        out
    };

    let name = ic.name;
    format!(r#"
import "../cacheir.cachet"

ir CacheStub emits CacheIr {{
    op {name} (
{var_decls}
    ) {{
{emits}
    }}
}}
    "#)
}

