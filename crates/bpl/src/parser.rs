// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod error;
mod helpers;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar, "/parser/grammar.rs");
use lazy_static::lazy_static;

use crate::ast::BoogieProgram;

pub use crate::parser::error::ParseError;

// * Parsing Entry Point

pub fn parse_boogie_program(src: &str) -> Result<BoogieProgram, ParseError> {
    BOOGIE_PROGRAM_PARSER.parse(src).map_err(ParseError::new)
}

lazy_static! {
    static ref BOOGIE_PROGRAM_PARSER: grammar::BoogieProgramParser =
        grammar::BoogieProgramParser::new();
}
