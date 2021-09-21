// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use enumset::{EnumSet, EnumSetType};
use iterate::iterate;
use thiserror::Error;

use crate::util::fmt_join_or;

use crate::ast::{Path, Span, Spanned};
use crate::FrontendError;

#[derive(Clone, Debug, Error)]
pub enum ResolveError {
    #[error(transparent)]
    NonMemberItem(#[from] NonMemberItemError),
    #[error(transparent)]
    DuplicateDef(#[from] DuplicateDefError),
    #[error(transparent)]
    Undefined(#[from] UndefinedError),
    #[error(transparent)]
    WrongKind(#[from] WrongKindError),
}

impl FrontendError for ResolveError {
    fn span(&self) -> Span {
        match self {
            ResolveError::NonMemberItem(error) => error.span(),
            ResolveError::DuplicateDef(error) => error.span(),
            ResolveError::Undefined(error) => error.span(),
            ResolveError::WrongKind(error) => error.span(),
        }
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        match self {
            ResolveError::NonMemberItem(error) => error.build_diagnostic(file_id),
            ResolveError::DuplicateDef(error) => error.build_diagnostic(file_id),
            ResolveError::Undefined(error) => error.build_diagnostic(file_id),
            ResolveError::WrongKind(error) => error.build_diagnostic(file_id),
        }
    }
}

#[derive(Debug)]
pub struct ResolveErrors(pub Vec<ResolveError>);

impl fmt::Display for ResolveErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "name resolution failed")
    }
}

impl Error for ResolveErrors {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.0.first() {
            Some(error) => Some(error),
            None => None,
        }
    }
}

#[derive(Clone, Debug, Error)]
#[error("items of this kind can't be members of other items")]
pub struct NonMemberItemError {
    pub span: Span,
    pub parent: Spanned<Path>,
}

impl FrontendError for NonMemberItemError {
    fn span(&self) -> Span {
        self.span
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(vec![
                Label::primary(file_id, self.span()).with_message(format!(
                    "top-level item can't be a member of `{}`",
                    self.parent
                )),
                Label::secondary(file_id, self.parent.span).with_message("parent item"),
            ])
    }
}

#[derive(Clone, Debug, Error)]
#[error(
    "duplicate definition of {}`{}`",
    match .first_defined_at {
        None => "built-in ",
        Some(_) => "",
    },
    .path,
)]
pub struct DuplicateDefError {
    pub path: Path,
    pub first_defined_at: Option<Span>,
    pub redefined_at: Span,
}

impl FrontendError for DuplicateDefError {
    fn span(&self) -> Span {
        self.redefined_at
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(
                iterate![
                    Label::primary(file_id, self.span())
                        .with_message(format!("`{}` redefined here", self.path)),
                    ..self.first_defined_at.map(|first_defined_at| {
                        Label::secondary(file_id, first_defined_at).with_message(format!(
                            "original definition of `{}` is here",
                            self.path
                        ))
                    }),
                ]
                .collect(),
            )
    }
}

#[derive(Clone, Debug, Error)]
#[error("undefined {} `{path}`", NameKinds(.expected))]
pub struct UndefinedError {
    pub path: Spanned<Path>,
    pub expected: EnumSet<NameKind>,
}

impl FrontendError for UndefinedError {
    fn span(&self) -> Span {
        self.path.span
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(vec![
                Label::primary(file_id, self.span()).with_message("not found in this scope"),
            ])
    }
}

#[derive(Clone, Debug, Error)]
#[error("expected {}, found {} `{path}`", NameKinds(.expected), NameKinds(.found))]
pub struct WrongKindError {
    pub path: Spanned<Path>,
    pub expected: EnumSet<NameKind>,
    pub found: EnumSet<NameKind>,
    pub defined_at: Option<Span>,
}

impl FrontendError for WrongKindError {
    fn span(&self) -> Span {
        self.path.span
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(
                iterate![
                    Label::primary(file_id, self.span())
                        .with_message(format!("expected {}", NameKinds(&self.expected))),
                    ..self.defined_at.map(|defined_at| {
                        Label::secondary(file_id, defined_at)
                            .with_message(format!("`{}` defined here", self.path))
                    }),
                ]
                .collect(),
            )
    }
}

#[derive(EnumSetType, Debug, Hash)]
pub enum NameKind {
    Type,
    Ir,
    Op,
    Var,
    Fn,
    Label,
}

impl fmt::Display for NameKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            NameKind::Type => write!(f, "type"),
            NameKind::Ir => write!(f, "IR"),
            NameKind::Op => write!(f, "op"),
            NameKind::Var => write!(f, "variable"),
            NameKind::Fn => write!(f, "function"),
            NameKind::Label => write!(f, "label"),
        }
    }
}

struct NameKinds<'a>(&'a EnumSet<NameKind>);

impl<'a> fmt::Display for NameKinds<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join_or(f, self.0.iter(), |f, kind| kind.fmt(f))
    }
}
