// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod error;
mod helpers;

use anyhow::Context;

use std::collections::HashSet;
use std::ffi::OsString;
use std::fs::read_to_string;
use std::path::Path;
use std::path::PathBuf;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar, "/parser/grammar.rs");
use lazy_static::lazy_static;

use crate::ast::FileId;

use crate::ast::Spanned;

pub use crate::parser::ast::*;
pub use crate::parser::error::*;

use self::helpers::RawParseError;

// * Parsing Entry Point

pub fn parse(file_id: FileId, src: &str) -> Result<Mod, ParseError> {
    PARSER
        .parse(file_id, src)
        .map_err(|error| ParseError::new(file_id, error))
}

lazy_static! {
    static ref PARSER: grammar::ModParser = grammar::ModParser::new();
}

// * Multi-File Parsing

pub type Files = codespan::Files<String>;

pub struct Parser {
    pub files: Files,
    pub file_paths: HashSet<PathBuf>,
    pub mod_: Mod,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            files: Files::new(),
            file_paths: HashSet::new(),
            mod_: Mod::default(),
        }
    }

    fn import(&mut self, from: FileId, path: Spanned<&Path>) -> Result<(), ParseError> {
        let src_path = Path::new(self.files.name(from))
            .parent()
            .expect("all files should have a parent");
        let canon_path = src_path.join(path.value).canonicalize().map_err(|_| {
            ParseError::new::<&str>(
                from,
                RawParseError::User {
                    error: Spanned::new(path.span, "invalid file path"),
                },
            )
        })?;

        if !self.file_paths.insert(canon_path.clone()) {
            return Ok(());
        }

        let src = read_to_string(&canon_path).map_err(|_| {
            ParseError::new::<&str>(
                from,
                RawParseError::User {
                    error: Spanned::new(path.span, "could not open file"),
                },
            )
        })?;

        self.parse_str(canon_path, &src)
    }

    pub fn parse_str(&mut self, name: impl Into<OsString>, src: &str) -> Result<(), ParseError> {
        let file_id = self.files.add(name.into(), src.into());
        let mod_ = parse(file_id, src)?;
        let imports = mod_.items.iter().filter_map(|item| match &item.value {
            Item::Import(item) => Some(&item.file_path),
            _ => None,
        });

        for import in imports {
            self.import(file_id, import.as_ref().map(|pb| pb.as_path()))?
        }

        self.mod_.items.extend(mod_.items);
        Ok(())
    }

    pub fn parse_file(&mut self, path: impl AsRef<Path>) -> Result<(), anyhow::Error> {
        let canon_path = path
            .as_ref()
            .canonicalize()
            .with_context(|| "invalid path provided")?;
        self.file_paths.insert(canon_path.clone());
        let contents = read_to_string(&canon_path)
            .with_context(|| format!("Failed to read {}", canon_path.display()))?;
        self.parse_str(canon_path, &contents)
            .with_context(|| "parse error")
    }
}
