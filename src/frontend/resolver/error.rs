// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt;

use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use enumset::{EnumSet, EnumSetType};
use thiserror::Error;

use crate::util::fmt_join_or;

use crate::frontend::parser::Path;
use crate::frontend::FrontendError;

use crate::frontend::resolver::ast::{Ident, Spanned};

#[derive(Debug, Error)]
pub enum ResolveError {
    #[error(
        "duplicate definition of `{}`",
        Path::new(
            .parent_type.as_ref().map(|parent_type| &parent_type.value),
            &.ident.value,
        ),
    )]
    DuplicateDef {
        parent_type: Option<Spanned<Ident>>,
        ident: Spanned<Ident>,
        first_defined_at: Option<Span>,
    },
    #[error("undefined {} `{path}`", ItemKinds(.expected))]
    Undefined {
        path: Spanned<Path>,
        expected: EnumSet<ItemKind>,
    },
    #[error("expected {}, found {} `{path}`", ItemKinds(.expected), ItemKinds(.found))]
    WrongKind {
        path: Spanned<Path>,
        expected: EnumSet<ItemKind>,
        found: EnumSet<ItemKind>,
        defined_at: Option<Span>,
    },
    #[error("can't write to `{path}`: all variables except out-parameters are immutable")]
    WriteToReadOnlyVar {
        path: Spanned<Path>,
        defined_at: Option<Span>,
    },
}

impl FrontendError for ResolveError {
    fn span(&self) -> Span {
        match self {
            ResolveError::DuplicateDef { ident, .. } => ident.span,
            ResolveError::Undefined { path, .. } => path.span,
            ResolveError::WrongKind { path, .. } => path.span,
            ResolveError::WriteToReadOnlyVar { path, .. } => path.span,
        }
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        let mut labels = vec![
            Label::primary(file_id, self.span()).with_message(match self {
                ResolveError::DuplicateDef {
                    parent_type, ident, ..
                } => format!(
                    "`{}` redefined here",
                    Path::new(
                        parent_type.as_ref().map(|parent_type| &parent_type.value),
                        &ident.value,
                    )
                ),
                ResolveError::Undefined { .. } => "not found in this scope".to_owned(),
                ResolveError::WrongKind { expected, .. } => {
                    format!("expected {}", ItemKinds(expected))
                }
                ResolveError::WriteToReadOnlyVar { .. } => "read-only variable".to_owned(),
            }),
        ];

        match self {
            ResolveError::DuplicateDef {
                parent_type,
                ident,
                first_defined_at,
                ..
            } => {
                if let Some(parent_type) = parent_type {
                    labels.push(
                        Label::secondary(file_id, parent_type.span).with_message("parent type"),
                    );
                }
                if let Some(first_defined_at) = first_defined_at {
                    labels.push(Label::secondary(file_id, *first_defined_at).with_message(
                        format!(
                            "original definition of `{}` is here",
                            Path::new(
                                parent_type.as_ref().map(|parent_type| &parent_type.value),
                                &ident.value,
                            )
                        ),
                    ));
                }
            }
            ResolveError::Undefined { .. } => (),
            ResolveError::WrongKind {
                path, defined_at, ..
            } => {
                if let Some(defined_at) = defined_at {
                    labels.push(
                        Label::secondary(file_id, *defined_at)
                            .with_message(format!("`{}` defined here", path)),
                    );
                }
            }
            ResolveError::WriteToReadOnlyVar {
                path, defined_at, ..
            } => {
                if let Some(defined_at) = defined_at {
                    labels.push(
                        Label::secondary(file_id, *defined_at)
                            .with_message(format!("variable `{}` defined here", path)),
                    );
                }
            }
        }

        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(labels)
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

#[derive(EnumSetType, Debug, Hash)]
pub enum ItemKind {
    Type,
    Fn,
    Var,
    Op,
}

impl fmt::Display for ItemKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ItemKind::Type => write!(f, "type"),
            ItemKind::Fn => write!(f, "function"),
            ItemKind::Var => write!(f, "variable"),
            ItemKind::Op => write!(f, "op"),
        }
    }
}

struct ItemKinds<'a>(&'a EnumSet<ItemKind>);

impl<'a> fmt::Display for ItemKinds<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join_or(f, self.0.iter(), |f, kind| kind.fmt(f))
    }
}
