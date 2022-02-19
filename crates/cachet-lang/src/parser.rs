// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;
mod helpers;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar, "/parser/grammar.rs");
use lazy_static::lazy_static;

use crate::ast::Spanned;

pub use crate::parser::ast::*;
pub use crate::parser::error::*;

pub fn parse<'a>(src: &'a str) -> Result<Vec<Spanned<Item>>, ParseError> {
    lazy_static! {
        static ref PARSER: grammar::ItemsParser = grammar::ItemsParser::new();
    }
    PARSER
        .parse(src)
        .map_err(|error| ParseError(error.map_token(|token| token.to_string())))
}
