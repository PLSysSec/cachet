// vim: set tw=99 ts=4 sts=4 sw=4 et:

use crate::ast::{Span, Spanned};
use crate::parser::ast::Expr;

pub type ParseError<T> = lalrpop_util::ParseError<usize, T, UserParseError>;
pub type UserParseError = Spanned<&'static str>;

#[derive(Default)]
pub struct VarTags {
    pub mut_tag: Option<Span>,
}

pub enum VarTag {
    Mut,
}

impl VarTags {
    pub fn reduce<T>(tags: impl Iterator<Item = Spanned<VarTag>>) -> Result<Self, ParseError<T>> {
        let mut accum = VarTags::default();

        for tag in tags {
            match tag.value {
                VarTag::Mut => {
                    if accum.mut_tag.replace(tag.span).is_some() {
                        return Err(ParseError::User {
                            error: Spanned {
                                span: tag.span,
                                value: "duplicate `mut` tag",
                            },
                        });
                    }
                }
            }
        }

        Ok(accum)
    }
}

pub enum CallableKind {
    Fn,
    Op,
}

#[derive(Default)]
pub struct CallableTags {
    pub unsafe_tag: Option<Span>,
}

pub enum CallableTag {
    Unsafe,
}

impl CallableTags {
    pub fn reduce<T>(
        tags: impl Iterator<Item = Spanned<CallableTag>>,
    ) -> Result<Self, ParseError<T>> {
        let mut accum = CallableTags::default();

        for tag in tags {
            match tag.value {
                CallableTag::Unsafe => {
                    if accum.unsafe_tag.replace(tag.span).is_some() {
                        return Err(ParseError::User {
                            error: Spanned {
                                span: tag.span,
                                value: "duplicate `unsafe` tag",
                            },
                        });
                    }
                }
            }
        }

        Ok(accum)
    }
}

pub struct MaybeGrouped {
    pub is_grouped: bool,
    pub expr: Expr,
}

impl MaybeGrouped {
    pub fn grouped(expr: Expr) -> Self {
        MaybeGrouped {
            is_grouped: true,
            expr,
        }
    }

    pub fn ungrouped(expr: Expr) -> Self {
        MaybeGrouped {
            is_grouped: false,
            expr,
        }
    }
}
