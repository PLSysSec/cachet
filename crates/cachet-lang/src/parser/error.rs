// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt::{self, Display};

use codespan::RawIndex;
use codespan_reporting::diagnostic::{Diagnostic, Label};

use cachet_util::fmt_join_or;

use crate::ast::Span;
use crate::FrontendError;

use crate::parser::helpers;

#[derive(Clone, Debug)]
pub struct ParseError(pub(super) helpers::ParseError<String>);

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
        match &self.0 {
            helpers::ParseError::InvalidToken { location } => {
                (*location as RawIndex..*location as RawIndex).into()
            }
            helpers::ParseError::UnrecognizedEOF { location, .. } => {
                (*location as RawIndex..*location as RawIndex).into()
            }
            helpers::ParseError::UnrecognizedToken {
                token: (start, _, end),
                ..
            } => (*start as RawIndex..*end as RawIndex).into(),

            helpers::ParseError::ExtraToken {
                token: (start, _, end),
            } => (*start as RawIndex..*end as RawIndex).into(),

            helpers::ParseError::User { error } => error.span,
        }
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(vec![Label::primary(file_id, self.span())])
    }
}
