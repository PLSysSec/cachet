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
    #[error("op `{}` must be nested under an IR", .op)]
    MisplacedOpItem { op: Spanned<Path> },
    #[error("op `{}` can't have out-parameter `{}`", .op, .out_param)]
    OpHasOutParam { op: Path, out_param: Spanned<Ident> },
    #[error("op `{}` can't return a value", .op)]
    OpReturnsValue { op: Path, ret_span: Span },
    #[error("op `{}` is missing a body", .op)]
    MissingOpBody { op: Path, body_span: Span },
    #[error("mismatched types")]
    ExprTypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        expr_span: Span,
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
    /// This error should only arise from mistakes in internal code or misuse of
    /// this crate's API, and shouldn't be possible to hit from user source code
    /// input.
    #[error("local variable used before its definition")]
    LocalVarUsedBeforeDef {
        var: Spanned<Ident>,
        defined_at: Span,
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
            TypeCheckError::MisplacedOpItem { op } => op.span,
            TypeCheckError::OpHasOutParam { out_param, .. } => out_param.span,
            TypeCheckError::OpReturnsValue { ret_span, .. } => *ret_span,
            TypeCheckError::MissingOpBody { body_span, .. } => *body_span,
            TypeCheckError::ExprTypeMismatch { expr_span, .. } => *expr_span,
            TypeCheckError::InvalidCast { expr_span, .. } => *expr_span,
            TypeCheckError::UnsafeCallInSafeContext { target, .. } => target.span,
            TypeCheckError::UnsafeCastInSafeContext { target_type, .. } => target_type.span,
            TypeCheckError::ArgCountMismatch { target, .. } => target.span,
            TypeCheckError::ArgKindMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::ArgTypeMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::BinaryOperatorTypeMismatch { operator_span, .. } => *operator_span,
            TypeCheckError::NumericOperatorTypeMismatch { operand_span, .. } => *operand_span,
            TypeCheckError::AssignTypeMismatch { rhs_span, .. } => *rhs_span,
            TypeCheckError::LocalVarUsedBeforeDef { var, .. } => var.span,
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
            TypeCheckError::MisplacedOpItem { .. } => {
                label.message = "misplaced `op` item".to_owned();
            }
            TypeCheckError::OpHasOutParam { .. }
            | TypeCheckError::OpReturnsValue { .. }
            | TypeCheckError::MissingOpBody { .. } => {
                label.message = "allowed for functions but not for ops".to_owned();
            }
            TypeCheckError::ExprTypeMismatch {
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
                expected_type,
                found_type,
                ..
            } => {
                label.message = format!("expected `{}`, found `{}`", expected_type, found_type);
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
            TypeCheckError::LocalVarUsedBeforeDef { var, .. } => {
                label.message = format!("variable `{}` used here", var);
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
            TypeCheckError::ExprTypeMismatch { .. } => (),
            TypeCheckError::MisplacedOpItem { .. } => (),
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
            TypeCheckError::ArgTypeMismatch { param, .. } => {
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
            TypeCheckError::LocalVarUsedBeforeDef {
                var,
                defined_at,
                ..
            } => {
                labels.push(
                    Label::secondary(file_id, *defined_at)
                        .with_message(format!("variable `{}` defined here", var)),
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
    Label,
    OutVar,
}

impl fmt::Display for ArgKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ArgKind::Expr => write!(f, "expression"),
            ArgKind::Label => write!(f, "label"),
            ArgKind::OutVar => write!(f, "out-parameter"),
        }
    }
}