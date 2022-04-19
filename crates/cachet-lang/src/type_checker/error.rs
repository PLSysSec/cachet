// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use thiserror::Error;

use crate::ast::{Ident, Path, Span, Spanned};
use crate::FrontendError;

// TODO(spinda): Break up TypeCheckError like ResolveError.
#[derive(Debug, Error)]
pub enum TypeCheckError {
    #[error("found cycle in subtype relationships")]
    SubtypeCycle {
        first_cycle_type: Spanned<Ident>,
        other_cycle_types: Vec<Spanned<Ident>>,
    },
    #[error("recursive calls are not allowed")]
    CallCycle {
        first_cycle_callable: Spanned<Path>,
        other_cycle_callables: Vec<Spanned<Path>>,
    },
    #[error("op `{op}` can't have out-parameter `{out_param}`")]
    OpHasOutParam { op: Path, out_param: Spanned<Ident> },
    #[error("op `{op}` can't return a value")]
    OpReturnsValue { op: Path, ret_span: Span },
    #[error("op `{op}` is missing a body")]
    MissingOpBody { op: Path, body_span: Span },
    #[error("can't jump to label `{label}` inside `{callable}")]
    GotoIrMismatch {
        label: Spanned<Ident>,
        callable: Path,
        expected_ir: Option<Spanned<Ident>>,
        found_ir: Ident,
    },
    #[error("can't bind label `{label}` inside `{callable}")]
    BindIrMismatch {
        label: Spanned<Ident>,
        callable: Path,
        expected_ir: Option<Spanned<Ident>>,
        found_ir: Ident,
    },
    #[error("can't emit op `{op}` inside `{callable}`")]
    EmitIrMismatch {
        op: Spanned<Path>,
        callable: Path,
        expected_ir: Option<Spanned<Ident>>,
        found_ir: Ident,
    },
    #[error("mismatched types")]
    TypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        span: Span,
    },
    #[error("casting `{source_type}` to `{target_type}` is invalid")]
    InvalidCast {
        source_type: Ident,
        target_type: Ident,
        expr_span: Span,
    },
    #[error(
        "calls to `{target}` are unsafe and can only appear inside an unsafe function, op, or \
        block"
    )]
    UnsafeCallInSafeContext {
        target: Spanned<Path>,
        target_defined_at: Span,
    },
    #[error(
        "casts from `{source_type}` to subtype `{target_type}` are unsafe and can only appear \
        inside an unsafe function, op, or block"
    )]
    UnsafeCastInSafeContext {
        source_type: Ident,
        target_type: Spanned<Ident>,
    },
    #[error("wrong number of arguments to `{target}`")]
    ArgCountMismatch {
        expected_arg_count: usize,
        found_arg_count: usize,
        target: Spanned<Path>,
        target_defined_at: Span,
        args_span: Span,
    },
    #[error("mismatched argument kind for parameter `{param}` to `{target}`")]
    ArgKindMismatch {
        expected_arg_kind: ArgKind,
        found_arg_kind: ArgKind,
        arg_span: Span,
        target: Path,
        param: Spanned<Ident>,
    },
    #[error("mismatched argument type for parameter `{param}` to `{target}`")]
    ArgTypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        arg_span: Span,
        target: Path,
        param: Spanned<Ident>,
    },
    #[error("IR of label argument doesn't match parameter `{param}` to `{target}`")]
    ArgIrMismatch {
        expected_ir: Ident,
        found_ir: Ident,
        arg_span: Span,
        target: Path,
        param: Spanned<Ident>,
    },
    #[error("left- and right-hand sides of operator have mismatched types")]
    BinaryOperatorTypeMismatch {
        operator_span: Span,
        lhs_span: Span,
        lhs_type: Ident,
        rhs_span: Span,
        rhs_type: Ident,
    },
    #[error("mismatched operand type for numeric operator")]
    NumericOperatorTypeMismatch {
        operand_span: Span,
        operand_type: Ident,
        operator_span: Span,
    },
    #[error("mismatched value type for assignment to `{lhs}`")]
    AssignTypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        lhs: Path,
        lhs_defined_at: Option<Span>,
        rhs_span: Span,
    },
    #[error("attempted to access of field `{field}` of non-struct type `{type_}`")]
    NonStructFieldAccess {
        field: Spanned<Ident>,
        type_: Ident,
        parent_span: Span,
    },
    #[error("field `{field}` does not exist on type `{type_}`")]
    FieldNotFound {
        field: Spanned<Ident>,
        type_: Spanned<Ident>,
    },
}

impl FrontendError for TypeCheckError {
    fn span(&self) -> Span {
        match self {
            TypeCheckError::SubtypeCycle {
                first_cycle_type, ..
            } => first_cycle_type.span,
            TypeCheckError::CallCycle {
                first_cycle_callable,
                ..
            } => first_cycle_callable.span,
            TypeCheckError::OpHasOutParam { out_param, .. } => out_param.span,
            TypeCheckError::OpReturnsValue { ret_span, .. } => *ret_span,
            TypeCheckError::MissingOpBody { body_span, .. } => *body_span,
            TypeCheckError::GotoIrMismatch { label, .. } => label.span,
            TypeCheckError::BindIrMismatch { label, .. } => label.span,
            TypeCheckError::EmitIrMismatch { op, .. } => op.span,
            TypeCheckError::TypeMismatch { span: expr_span, .. } => *expr_span,
            TypeCheckError::InvalidCast { expr_span, .. } => *expr_span,
            TypeCheckError::UnsafeCallInSafeContext { target, .. } => target.span,
            TypeCheckError::UnsafeCastInSafeContext { target_type, .. } => target_type.span,
            TypeCheckError::ArgCountMismatch { target, .. } => target.span,
            TypeCheckError::ArgKindMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::ArgTypeMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::ArgIrMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::BinaryOperatorTypeMismatch { operator_span, .. } => *operator_span,
            TypeCheckError::NumericOperatorTypeMismatch { operand_span, .. } => *operand_span,
            TypeCheckError::AssignTypeMismatch { rhs_span, .. } => *rhs_span,
            TypeCheckError::NonStructFieldAccess { field, .. } => field.span,
            TypeCheckError::FieldNotFound { field, .. } => field.span,
        }
    }

    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T> {
        let mut label = Label::primary(file_id, self.span());

        match self {
            TypeCheckError::SubtypeCycle {
                first_cycle_type,
                other_cycle_types,
            } => {
                label.message = format!(
                    "{}`{}` is a subtype of `{}`",
                    if other_cycle_types.is_empty() {
                        ""
                    } else {
                        "(1) "
                    },
                    first_cycle_type,
                    other_cycle_types.last().unwrap_or(first_cycle_type)
                );
            }
            TypeCheckError::CallCycle {
                first_cycle_callable,
                other_cycle_callables,
            } => {
                label.message = format!(
                    "{}`{}` calls `{}`",
                    if other_cycle_callables.is_empty() {
                        ""
                    } else {
                        "(1) "
                    },
                    other_cycle_callables.last().unwrap_or(first_cycle_callable),
                    first_cycle_callable
                );
            }
            TypeCheckError::OpHasOutParam { .. }
            | TypeCheckError::OpReturnsValue { .. }
            | TypeCheckError::MissingOpBody { .. } => {
                label.message = "allowed for functions but not for ops".to_owned();
            }
            TypeCheckError::GotoIrMismatch {
                callable,
                expected_ir,
                found_ir,
                ..
            } => {
                label.message = match expected_ir {
                    Some(expected_ir) => {
                        format!(
                            "expected `{}` label, found `{}` label",
                            expected_ir, found_ir
                        )
                    }
                    None => format!(
                        "unexpected jump to `{}` label inside non-interpreter `{}`",
                        found_ir, callable
                    ),
                };
            }
            TypeCheckError::BindIrMismatch {
                callable,
                expected_ir,
                found_ir,
                ..
            } => {
                label.message = match expected_ir {
                    Some(expected_ir) => {
                        format!(
                            "expected `{}` label, found `{}` label",
                            expected_ir, found_ir
                        )
                    }
                    None => format!(
                        "unexpected bind of `{}` label inside non-emitter `{}`",
                        found_ir, callable
                    ),
                };
            }
            TypeCheckError::EmitIrMismatch {
                callable,
                expected_ir,
                found_ir,
                ..
            } => {
                label.message = match expected_ir {
                    Some(expected_ir) => {
                        format!("expected `{}` op, found `{}` op", expected_ir, found_ir)
                    }
                    None => format!(
                        "unexpected emit of `{}` op inside non-emitter `{}`",
                        found_ir, callable
                    ),
                };
            }
            TypeCheckError::TypeMismatch {
                expected_type,
                found_type,
                ..
            } => {
                label.message = format!("expected `{}`, found `{}`", expected_type, found_type);
            }
            TypeCheckError::InvalidCast {
                source_type,
                target_type,
                ..
            } => {
                label.message = format!(
                    "`{}` isn't a subtype of `{}', and vice-versa",
                    source_type, target_type
                );
            }
            TypeCheckError::UnsafeCallInSafeContext { .. } => {
                label.message = "call to unsafe function".to_owned();
            }
            TypeCheckError::UnsafeCastInSafeContext { .. } => {
                label.message = "unsafe cast".to_owned();
            }
            TypeCheckError::ArgCountMismatch {
                expected_arg_count, ..
            } => {
                label.message = format!(
                    "expected {} argument{}",
                    expected_arg_count,
                    if *expected_arg_count == 1 { "" } else { "s" }
                );
            }
            TypeCheckError::ArgKindMismatch {
                expected_arg_kind,
                found_arg_kind,
                ..
            } => {
                label.message =
                    format!("expected {}, found {}", expected_arg_kind, found_arg_kind);
            }
            TypeCheckError::ArgTypeMismatch {
                expected_type: expected,
                found_type: found,
                ..
            }
            | TypeCheckError::ArgIrMismatch {
                expected_ir: expected,
                found_ir: found,
                ..
            } => {
                label.message = format!("expected `{}`, found `{}`", expected, found);
            }
            TypeCheckError::BinaryOperatorTypeMismatch { .. } => (),
            TypeCheckError::NumericOperatorTypeMismatch { operand_type, .. } => {
                label.message = format!("expected numeric type, found `{}`", operand_type);
            }
            TypeCheckError::AssignTypeMismatch {
                expected_type,
                found_type,
                ..
            } => {
                label.message = format!("expected `{}`, found `{}`", expected_type, found_type);
            }
            TypeCheckError::NonStructFieldAccess { .. } => {
                label.message = format!("fields can only be accessed on structs")
            }
            TypeCheckError::FieldNotFound { .. } => {
                label.message = format!("field must be declared on the struct")
            }
        }

        let mut labels = vec![label];

        match self {
            TypeCheckError::SubtypeCycle {
                first_cycle_type,
                other_cycle_types,
            } => {
                let mut prev_cycle_type = first_cycle_type;
                labels.extend(other_cycle_types.iter().enumerate().map(
                    |(curr_cycle_type_index, curr_cycle_type)| {
                        let label =
                            Label::secondary(file_id, curr_cycle_type.span).with_message(format!(
                                "({}) `{}` is a subtype of `{}`",
                                curr_cycle_type_index + 2,
                                curr_cycle_type,
                                prev_cycle_type
                            ));
                        prev_cycle_type = curr_cycle_type;
                        label
                    },
                ));
            }
            TypeCheckError::CallCycle {
                first_cycle_callable,
                other_cycle_callables,
            } => {
                let mut prev_cycle_callable = first_cycle_callable;
                labels.extend(other_cycle_callables.iter().enumerate().map(
                    |(curr_cycle_callable_index, curr_cycle_callable)| {
                        let label = Label::secondary(file_id, curr_cycle_callable.span)
                            .with_message(format!(
                                "({}) `{}` calls `{}`",
                                curr_cycle_callable_index + 2,
                                prev_cycle_callable,
                                curr_cycle_callable
                            ));
                        prev_cycle_callable = curr_cycle_callable;
                        label
                    },
                ));
            }
            TypeCheckError::OpHasOutParam { .. } => (),
            TypeCheckError::OpReturnsValue { .. } => (),
            TypeCheckError::MissingOpBody { .. } => (),
            TypeCheckError::GotoIrMismatch {
                callable,
                expected_ir,
                ..
            } => {
                if let Some(expected_ir) = expected_ir {
                    labels.push(
                        Label::secondary(file_id, expected_ir.span).with_message(format!(
                            "`{}` interprets `{}` ops",
                            callable, expected_ir
                        )),
                    );
                }
            }
            TypeCheckError::BindIrMismatch {
                callable,
                expected_ir,
                ..
            }
            | TypeCheckError::EmitIrMismatch {
                callable,
                expected_ir,
                ..
            } => {
                if let Some(expected_ir) = expected_ir {
                    labels.push(
                        Label::secondary(file_id, expected_ir.span)
                            .with_message(format!("`{}` emits `{}` ops", callable, expected_ir)),
                    );
                }
            }
            TypeCheckError::TypeMismatch { .. } => (),
            TypeCheckError::InvalidCast { .. } => (),
            TypeCheckError::UnsafeCallInSafeContext {
                target,
                target_defined_at,
                ..
            } => {
                labels.push(
                    Label::secondary(file_id, *target_defined_at)
                        .with_message(format!("function `{}` defined here", target)),
                );
            }
            TypeCheckError::UnsafeCastInSafeContext { .. } => (),
            TypeCheckError::ArgCountMismatch {
                found_arg_count,
                target,
                target_defined_at,
                args_span,
                ..
            } => {
                labels.extend([
                    Label::secondary(file_id, *args_span).with_message(format!(
                        "found {} argument{}",
                        found_arg_count,
                        if *found_arg_count == 1 { "" } else { "s" }
                    )),
                    Label::secondary(file_id, *target_defined_at)
                        .with_message(format!("function `{}` defined here", target)),
                ]);
            }
            TypeCheckError::ArgKindMismatch { param, .. } => {
                labels.push(
                    Label::secondary(file_id, param.span)
                        .with_message(format!("parameter `{}` defined here", param)),
                );
            }
            TypeCheckError::ArgTypeMismatch { param, .. }
            | TypeCheckError::ArgIrMismatch { param, .. } => {
                labels.push(
                    Label::secondary(file_id, param.span)
                        .with_message(format!("parameter `{}` defined here", param)),
                );
            }
            TypeCheckError::BinaryOperatorTypeMismatch {
                lhs_span,
                lhs_type,
                rhs_span,
                rhs_type,
                ..
            } => {
                labels.extend([
                    Label::secondary(file_id, *lhs_span)
                        .with_message(format!("type `{}`", lhs_type)),
                    Label::secondary(file_id, *rhs_span)
                        .with_message(format!("type `{}`", rhs_type)),
                ]);
            }
            TypeCheckError::NumericOperatorTypeMismatch { operator_span, .. } => {
                labels.push(
                    Label::secondary(file_id, *operator_span)
                        .with_message("operator is only defined for numeric types"),
                );
            }
            TypeCheckError::AssignTypeMismatch {
                lhs,
                lhs_defined_at,
                ..
            } => {
                if let Some(lhs_defined_at) = lhs_defined_at {
                    labels.push(
                        Label::secondary(file_id, *lhs_defined_at)
                            .with_message(format!("variable `{}` defined here", lhs)),
                    );
                }
            }
            TypeCheckError::NonStructFieldAccess {
                parent_span, type_, ..
            } => labels.push(
                Label::secondary(file_id, *parent_span)
                    .with_message(format!("expression is of type `{}`", type_)),
            ),
            TypeCheckError::FieldNotFound { type_, .. } => {
                labels.push(
                    Label::secondary(file_id, type_.span)
                        .with_message(format!("struct `{}` declared here", type_.value)),
                );
            }
        }

        Diagnostic::error()
            .with_message(self.to_string())
            .with_labels(labels)
    }
}

#[derive(Debug)]
pub struct TypeCheckErrors(pub Vec<TypeCheckError>);

impl fmt::Display for TypeCheckErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "type checking failed")
    }
}

impl Error for TypeCheckErrors {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self.0.first() {
            Some(error) => Some(error),
            None => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ArgKind {
    Expr,
    OutVar,
    Label,
    OutLabel,
}

impl fmt::Display for ArgKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ArgKind::Expr => write!(f, "expression"),
            ArgKind::OutVar => write!(f, "variable out-parameter"),
            ArgKind::Label => write!(f, "label"),
            ArgKind::OutLabel => write!(f, "label out-parameter"),
        }
    }
}
