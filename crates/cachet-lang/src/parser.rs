// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;
mod helpers;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar, "/parser/grammar.rs");

use std::error::Error;
use std::fmt;

use codespan::{RawIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lazy_static::lazy_static;

use crate::util::fmt_join_or;

use crate::FrontendError;

pub use crate::parser::ast::*;

pub fn parse<'a>(spec_src: &'a str) -> Result<Spec, ParseError> {
    lazy_static! {
        static ref SPEC_PARSER: grammar::SpecParser = grammar::SpecParser::new();
    }
    SPEC_PARSER
        .parse(spec_src)
        .map_err(|error| ParseError(error.map_token(|token| token.to_string())))
}

#[derive(Clone, Debug)]
pub struct ParseError(helpers::ParseError<String>);

fn fmt_expected(f: &mut fmt::Formatter, expected: &[String]) -> Result<(), fmt::Error> {
    if !expected.is_empty() {
        write!(f, "; expected ")?;
        fmt_join_or(f, expected.iter(), |f, item| match item.as_str() {
            "r#\"[A-Za-z_][A-Za-z0-9_]*\"#" => write!(f, "identifier"),
            item => write!(f, "{}", item.replace("\"", "`")),
        })?;
    }
    Ok(())
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.0 {
            helpers::ParseError::InvalidToken { .. } => write!(f, "invalid token")?,
            helpers::ParseError::UnrecognizedEOF { expected, .. } => {
                write!(f, "unexpected end of input")?;
                fmt_expected(f, expected)?;
            }
            helpers::ParseError::UnrecognizedToken {
                token: (_, token, _),
                expected,
                ..
            } => {
                write!(f, "unexpected token `{}`", token)?;
                fmt_expected(f, expected)?;
            }
            helpers::ParseError::ExtraToken {
                token: (_, token, _),
                ..
            } => {
                write!(f, "extra token `{}`", token)?;
            }
            helpers::ParseError::User { error } => write!(f, "{}", error.value)?,
        }
        Ok(())
    }
}

impl Error for ParseError {}

impl FrontendError for ParseError {
    fn span(&self) -> Span {
        match self.0 {
            helpers::ParseError::InvalidToken { location } => {
                Span::new(location as RawIndex, location as RawIndex)
            }
            helpers::ParseError::UnrecognizedEOF { location, .. } => {
                Span::new(location as RawIndex, location as RawIndex)
            }
            helpers::ParseError::UnrecognizedToken {
                token: (start, _, end),
                ..
            } => Span::new(start as RawIndex, end as RawIndex),
            helpers::ParseError::ExtraToken {
                token: (start, _, end),
            } => Span::new(start as RawIndex, end as RawIndex),
            helpers::ParseError::User { error } => error.span,
        }
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(vec![Label::primary(file_id, self.span())])
    }
}
