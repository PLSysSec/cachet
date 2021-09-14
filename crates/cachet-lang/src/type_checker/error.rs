// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;
use std::fmt;

use codespan::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use thiserror::Error;

use crate::parser::Path;
use crate::FrontendError;

use crate::type_checker::ast::{Ident, Spanned};

#[derive(Debug, Error)]
pub enum TypeCheckError {
    #[error("found cycle in subtype relationships")]
    SubtypeCycle {
        first_cycle_type: Spanned<Ident>,
        other_cycle_types: Vec<Spanned<Ident>>,
    },
    #[error("found cycle in function calls; recursion is not allowed")]
    FnCallCycle {
        first_cycle_fn: Spanned<Path>,
        other_cycle_fns: Vec<Spanned<Path>>,
    },
    #[error("mismatched types")]
    ExprTypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        expr_span: Span,
    },
    #[error("can't read from out-parameter `{ident}`")]
    ReadFromOutParamVar {
        ident: Spanned<Ident>,
        defined_at: Span,
    },
    #[error("can't write to `{ident}`: all variables except out-parameters are immutable")]
    WriteToReadOnlyVar {
        ident: Spanned<Ident>,
        defined_at: Span,
    },
    #[error("casting `{source_type}` to `{target_type}` is invalid")]
    InvalidCast {
        source_type: Ident,
        target_type: Ident,
        expr_span: Span,
    },
    #[error("call to fallible function `{target}` requires fallible function or block")]
    FallibleCallInInfallibleContext {
        target: Spanned<Path>,
        target_defined_at: Span,
    },
    #[error("call to unsafe function `{target}` requires unsafe function or block")]
    UnsafeCallInSafeContext {
        target: Spanned<Path>,
        target_defined_at: Span,
    },
    #[error(
        "cast from `{source_type}` to subtype `{target_type}` requires unsafe function or block"
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
    #[error("mismatched argument kind for parameter `{param_var}` of function `{target}`")]
    ArgKindMismatch {
        expected_kind: ArgKind,
        found_kind: ArgKind,
        arg_span: Span,
        target: Path,
        param_var: Spanned<Ident>,
    },
    #[error("mismatched argument type for parameter `{param_var}` of function `{target}`")]
    ArgTypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        arg_span: Span,
        target: Path,
        param_var: Spanned<Ident>,
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
    #[error("mismatched value type for assignment to out-parameter `{param_var}`")]
    AssignTypeMismatch {
        expected_type: Ident,
        found_type: Ident,
        rhs_span: Span,
        param_var: Spanned<Ident>,
    },
    #[error("`{context}` can write to out-parameter `{param_var}` more than once")]
    ExtraOutParamVarWrite {
        extra_write_span: Span,
        first_write_span: Span,
        context: Path,
        param_var: Spanned<Ident>,
    },
    #[error("return from `{context}` without writing to out-parameter `{param_var}`")]
    UnwrittenOutParamVar {
        ret_span: Span,
        context: Path,
        param_var: Spanned<Ident>,
    },
}

impl FrontendError for TypeCheckError {
    fn span(&self) -> Span {
        match self {
            TypeCheckError::SubtypeCycle {
                first_cycle_type, ..
            } => first_cycle_type.span,
            TypeCheckError::FnCallCycle { first_cycle_fn, .. } => first_cycle_fn.span,
            TypeCheckError::ExprTypeMismatch { expr_span, .. } => *expr_span,
            TypeCheckError::ReadFromOutParamVar { ident, .. } => ident.span,
            TypeCheckError::WriteToReadOnlyVar { ident, .. } => ident.span,
            TypeCheckError::InvalidCast { expr_span, .. } => *expr_span,
            TypeCheckError::FallibleCallInInfallibleContext { target, .. } => target.span,
            TypeCheckError::UnsafeCallInSafeContext { target, .. } => target.span,
            TypeCheckError::UnsafeCastInSafeContext { target_type, .. } => target_type.span,
            TypeCheckError::ArgCountMismatch { target, .. } => target.span,
            TypeCheckError::ArgKindMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::ArgTypeMismatch { arg_span, .. } => *arg_span,
            TypeCheckError::BinaryOperatorTypeMismatch { operator_span, .. } => *operator_span,
            TypeCheckError::NumericOperatorTypeMismatch { operand_span, .. } => *operand_span,
            TypeCheckError::AssignTypeMismatch { rhs_span, .. } => *rhs_span,
            TypeCheckError::UnwrittenOutParamVar { ret_span, .. } => *ret_span,
            TypeCheckError::ExtraOutParamVarWrite {
                extra_write_span, ..
            } => *extra_write_span,
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
                    "{}type `{}` is a subtype of `{}`",
                    if other_cycle_types.is_empty() {
                        ""
                    } else {
                        "(1) "
                    },
                    first_cycle_type,
                    other_cycle_types.last().unwrap_or(first_cycle_type)
                );
            }
            TypeCheckError::FnCallCycle {
                first_cycle_fn,
                other_cycle_fns,
            } => {
                label.message = format!(
                    "{}function `{}` calls `{}`",
                    if other_cycle_fns.is_empty() {
                        ""
                    } else {
                        "(1) "
                    },
                    other_cycle_fns.last().unwrap_or(first_cycle_fn),
                    first_cycle_fn
                );
            }
            TypeCheckError::ExprTypeMismatch {
                expected_type,
                found_type,
                ..
            } => {
                label.message = format!("expected `{}`, found `{}`", expected_type, found_type);
            }
            TypeCheckError::ReadFromOutParamVar { .. } => {
                label.message = "write-only variable".to_owned();
            }
            TypeCheckError::WriteToReadOnlyVar { .. } => {
                label.message = "read-only variable".to_owned();
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
            TypeCheckError::FallibleCallInInfallibleContext { .. } => {
                label.message = "call to fallible function".to_owned();
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
                expected_kind,
                found_kind,
                ..
            } => {
                label.message = format!("expected {}, found {}", expected_kind, found_kind);
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
            TypeCheckError::UnwrittenOutParamVar { param_var, .. } => {
                label.message = format!(
                    "all return paths must write to `{}` exactly once",
                    param_var
                );
            }
            TypeCheckError::ExtraOutParamVarWrite { param_var, .. } => {
                label.message = format!(
                    "all return paths must write to `{}` exactly once",
                    param_var
                );
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
                                "({}) type `{}` is a subtype of `{}`",
                                curr_cycle_type_index + 2,
                                curr_cycle_type,
                                prev_cycle_type
                            ));
                        prev_cycle_type = curr_cycle_type;
                        label
                    },
                ));
            }
            TypeCheckError::FnCallCycle {
                first_cycle_fn,
                other_cycle_fns,
            } => {
                let mut prev_cycle_fn = first_cycle_fn;
                labels.extend(other_cycle_fns.iter().enumerate().map(
                    |(curr_cycle_fn_index, curr_cycle_fn)| {
                        let label =
                            Label::secondary(file_id, curr_cycle_fn.span).with_message(format!(
                                "({}) function `{}` calls `{}`",
                                curr_cycle_fn_index + 2,
                                prev_cycle_fn,
                                curr_cycle_fn
                            ));
                        prev_cycle_fn = curr_cycle_fn;
                        label
                    },
                ));
            }
            TypeCheckError::ExprTypeMismatch { .. } => (),
            TypeCheckError::ReadFromOutParamVar { ident, defined_at } => {
                labels.push(
                    Label::secondary(file_id, *defined_at)
                        .with_message(format!("out-parameter `{}` defined here", ident)),
                );
            }
            TypeCheckError::WriteToReadOnlyVar { ident, defined_at } => {
                labels.push(
                    Label::secondary(file_id, *defined_at)
                        .with_message(format!("variable `{}` defined here", ident)),
                );
            }
            TypeCheckError::InvalidCast { .. } => (),
            TypeCheckError::FallibleCallInInfallibleContext {
                target,
                target_defined_at,
                ..
            } => {
                labels.push(
                    Label::secondary(file_id, *target_defined_at)
                        .with_message(format!("function `{}` defined here", target)),
                );
            }
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
            TypeCheckError::ArgKindMismatch { param_var, .. } => {
                labels.push(
                    Label::secondary(file_id, param_var.span)
                        .with_message(format!("parameter `{}` defined here", param_var)),
                );
            }
            TypeCheckError::ArgTypeMismatch { param_var, .. } => {
                labels.push(
                    Label::secondary(file_id, param_var.span)
                        .with_message(format!("parameter `{}` defined here", param_var)),
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
            TypeCheckError::AssignTypeMismatch { param_var, .. } => {
                labels.push(
                    Label::secondary(file_id, param_var.span)
                        .with_message(format!("out-parameter `{}` defined here", param_var)),
                );
            }
            TypeCheckError::UnwrittenOutParamVar { param_var, .. } => {
                labels.push(
                    Label::secondary(file_id, param_var.span)
                        .with_message(format!("out-parameter `{}` defined here", param_var)),
                );
            }
            TypeCheckError::ExtraOutParamVarWrite {
                first_write_span,
                param_var,
                ..
            } => {
                labels.extend([
                    Label::secondary(file_id, *first_write_span)
                        .with_message(format!("earlier write to `{}` can happen here", param_var)),
                    Label::secondary(file_id, param_var.span)
                        .with_message(format!("out-parameter `{}` defined here", param_var)),
                ]);
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
    OutRef,
}

impl fmt::Display for ArgKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ArgKind::Expr => write!(f, "expression"),
            ArgKind::OutRef => write!(f, "out-reference"),
        }
    }
}
