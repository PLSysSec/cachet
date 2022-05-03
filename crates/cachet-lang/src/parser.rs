// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;
mod helpers;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar, "/parser/grammar.rs");
use lazy_static::lazy_static;

use crate::ast::FileId;
use crate::ast::Spanned;
pub use crate::parser::ast::*;
pub use crate::parser::error::*;




lazy_static! {
    static ref PARSER: grammar::ItemsParser = grammar::ItemsParser::new();
}

pub type Files = codespan::Files<String>;

pub struct Parser {
    pub files: Files,
    pub items: Vec<Spanned<Item>>
}

impl Parser {
    pub fn new() -> Self {
        Self {
            files: Files::new(),
            items: vec![]
        }
    }

    pub fn parse(&mut self, name: String, src: &str) -> Result<(), ParseError> {
        let file_id = self.files.add(name, src.into());
        let items = parse(file_id, src)?;
        
        self.items.splice(0..0, items);
        Ok(())
    }
}

pub fn parse<'a>(file_id: FileId, src: &str) -> Result<Vec<Spanned<Item>>, ParseError> {
    PARSER
        .parse(file_id, src)
        .map_err(|error| ParseError::new(file_id, error))
}