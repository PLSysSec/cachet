// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt::{self, Display};

use codespan::RawIndex;
use codespan_reporting::diagnostic::{Diagnostic, Label};

use cachet_util::fmt_join_or;

use crate::ast::{FileId, Span};
use crate::FrontendError;

use crate::parser::helpers;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub file_id: FileId,
    pub(super) underlying_error: helpers::ParseError<String>,
}

impl ParseError {
    pub fn new<T: ToString>(file_id: FileId, underlying_error: helpers::ParseError<T>) -> Self {
        Self {
            file_id,
            underlying_error: underlying_error.map_token(|t| t.to_string()),
        }
    }
}

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

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.underlying_error {
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
        match &self.underlying_error {
            helpers::ParseError::InvalidToken { location } => {
                Span::new(self.file_id, *location as RawIndex..*location as RawIndex)
            }
            helpers::ParseError::UnrecognizedEOF { location, .. } => {
                Span::new(self.file_id, *location as RawIndex..*location as RawIndex)
            }
            helpers::ParseError::UnrecognizedToken {
                token: (start, _, end),
                ..
            } => Span::new(self.file_id, *start as RawIndex..*end as RawIndex),

            helpers::ParseError::ExtraToken {
                token: (start, _, end),
            } => Span::new(self.file_id, *start as RawIndex..*end as RawIndex),

            helpers::ParseError::User { error } => return error.span,
        }
    }

    fn build_diagnostic(&self) -> Diagnostic<FileId> {
        let d = Diagnostic::error().with_message(self.to_string());

        match self.span() {
            Span::Internal => d,
            Span::External(file, range) => d.with_labels(vec![Label::primary(file, range)]),
        }
    }
}
