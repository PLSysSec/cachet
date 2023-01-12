// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt::{self, Display};

use codespan::{RawIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle};
use phf::phf_map;

use crate::parser::helpers::RawParseError;

#[derive(Clone, Debug)]
pub struct ParseError {
    error: RawParseError<String>,
}

impl ParseError {
    pub(super) fn new<T: ToString>(error: RawParseError<T>) -> Self {
        Self {
            error: error.map_token(|token| token.to_string()),
        }
    }

    pub fn span(&self) -> Span {
        match &self.error {
            RawParseError::InvalidToken { location } => {
                Span::new(*location as RawIndex, *location as RawIndex)
            }
            RawParseError::UnrecognizedEOF { location, .. } => {
                Span::new(*location as RawIndex, *location as RawIndex)
            }
            RawParseError::UnrecognizedToken {
                token: (start, _, end),
                ..
            } => Span::new(*start as RawIndex, *end as RawIndex),
            RawParseError::ExtraToken {
                token: (start, _, end),
            } => Span::new(*start as RawIndex, *end as RawIndex),
            RawParseError::User { error } => return error.span,
        }
    }

    pub fn build_diagnostic(&self) -> Diagnostic<()> {
        self.build_diagnostic_for_file(())
    }

    pub fn build_diagnostic_for_file<T>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(vec![Label::new(LabelStyle::Primary, file_id, self.span())])
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.error {
            RawParseError::InvalidToken { .. } => write!(f, "invalid token")?,
            RawParseError::UnrecognizedEOF { expected, .. } => {
                write!(f, "unexpected end of input")?;
                fmt_expected(f, expected)?;
            }
            RawParseError::UnrecognizedToken {
                token: (_, token, _),
                expected,
                ..
            } => {
                write!(f, "unexpected token `{}`", token)?;
                fmt_expected(f, expected)?;
            }
            RawParseError::ExtraToken {
                token: (_, token, _),
                ..
            } => {
                write!(f, "extra token `{}`", token)?;
            }
            RawParseError::User { error } => write!(f, "{}", error.value)?,
        }
        Ok(())
    }
}

impl Error for ParseError {}

static TOKEN_NAMES: phf::Map<&'static str, &'static str> = phf_map! {
    "r#\"\\\\\\\\?[A-Za-z'~#$^_.?`][A-Za-z0-9'~#$^_.?`]*\"#" => "identifier",
    "r#\"[0-9]+\"#" => "digits",
    "r#\"[0-9]+bv[0-9]+\"#" => "bitvector literal",
    "r#\"[0-9]+(e-?[0-9]+|\\\\.[0-9]+(e-?[0-9]+)?)\"#" => "decimal",
    "r#\"(-?0x[0-9A-Fa-f]+\\\\.[0-9A-Fa-f]+e-?[0-9]+f|0(NaN|nan|[+-]oo))[0-9]+e[0-9]+\"#" =>
        "float",
};

fn fmt_expected(f: &mut fmt::Formatter, expected: &[String]) -> Result<(), fmt::Error> {
    if !expected.is_empty() {
        write!(f, "; expected ")?;

        let mut tokens = expected.iter().enumerate().peekable();
        while let Some((token_index, token)) = tokens.next() {
            if token_index > 0 {
                if tokens.peek().is_some() {
                    write!(f, ", ")
                } else if token_index > 1 {
                    write!(f, ", or ")
                } else {
                    write!(f, " or ")
                }?;
            }

            match TOKEN_NAMES.get(token.as_str()) {
                Some(token_name) => write!(f, "{}", token_name),
                None => write!(f, "{}", token.replace("\"", "`")),
            }?;
        }
    }
    Ok(())
}
