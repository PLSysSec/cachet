// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::ops::Deref;

use derive_more::From;

use crate::ast::{Span, Spanned};

use crate::parser::ast::{Block, Expr};

pub type RawParseError<T> = lalrpop_util::ParseError<usize, T, UserParseError>;
pub type UserParseError = Spanned<&'static str>;

#[derive(Default)]
pub struct VarTags {
    pub mut_tag: Option<Span>,
}

pub enum VarTag {
    Mut,
}

impl VarTags {
    pub fn reduce<T>(
        tags: impl Iterator<Item = Spanned<VarTag>>,
    ) -> Result<Self, RawParseError<T>> {
        let mut accum = VarTags::default();
        for tag in tags {
            accum.update(tag)?;
        }
        Ok(accum)
    }

    pub fn update<T>(&mut self, tag: Spanned<VarTag>) -> Result<(), RawParseError<T>> {
        match tag.value {
            VarTag::Mut => {
                if self.mut_tag.replace(tag.span).is_some() {
                    return Err(RawParseError::User {
                        error: Spanned {
                            span: tag.span,
                            value: "duplicate `mut` tag",
                        },
                    });
                }
            }
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct ParamTags {
    pub in_tag: Option<Span>,
    pub ref_tag: Option<Span>,
    pub out_tag: Option<Span>,
    pub var_tags: VarTags,
}

#[derive(From)]
pub enum ParamTag {
    In,
    Ref,
    Out,
    Var(VarTag),
}

impl ParamTags {
    pub fn reduce<T>(
        tags: impl Iterator<Item = Spanned<ParamTag>>,
    ) -> Result<Self, RawParseError<T>> {
        let mut accum = ParamTags::default();
        let mut saw_ref_param_tag = false;

        for tag in tags {
            let is_ref_param_tag = match tag.value {
                ParamTag::In => {
                    if accum.in_tag.replace(tag.span).is_some() {
                        return Err(RawParseError::User {
                            error: Spanned {
                                span: tag.span,
                                value: "duplicate `in` tag",
                            },
                        });
                    }
                    true
                }
                ParamTag::Ref => {
                    if accum.ref_tag.replace(tag.span).is_some() {
                        return Err(RawParseError::User {
                            error: Spanned {
                                span: tag.span,
                                value: "duplicate `ref` tag",
                            },
                        });
                    }
                    true
                }
                ParamTag::Out => {
                    if accum.out_tag.replace(tag.span).is_some() {
                        return Err(RawParseError::User {
                            error: Spanned {
                                span: tag.span,
                                value: "duplicate `out` tag",
                            },
                        });
                    }
                    true
                }
                ParamTag::Var(var_tag) => {
                    accum.var_tags.update(Spanned::new(tag.span, var_tag))?;
                    if saw_ref_param_tag {
                        return Err(RawParseError::User {
                            error: Spanned {
                                span: tag.span,
                                value: "irrelevant tag for a reference parameter",
                            },
                        });
                    }
                    false
                }
            };

            if is_ref_param_tag {
                if saw_ref_param_tag {
                    return Err(RawParseError::User {
                        error: Spanned {
                            span: tag.span,
                            value: "`in`, `ref`, and `out` tags are mutually exclusive",
                        },
                    });
                }
                saw_ref_param_tag = true;
            }
        }

        Ok(accum)
    }
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
    ) -> Result<Self, RawParseError<T>> {
        let mut accum = CallableTags::default();

        for tag in tags {
            match tag.value {
                CallableTag::Unsafe => {
                    if accum.unsafe_tag.replace(tag.span).is_some() {
                        return Err(RawParseError::User {
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

pub enum CallableKind {
    Fn,
    Op,
}

pub struct PrecExpr {
    pub is_grouped: bool,
    pub expr: Expr,
}

impl PrecExpr {
    pub fn grouped(expr: Expr) -> Self {
        PrecExpr {
            is_grouped: true,
            expr,
        }
    }
}

impl Deref for PrecExpr {
    type Target = Expr;

    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}

impl From<Expr> for PrecExpr {
    fn from(expr: Expr) -> Self {
        PrecExpr {
            is_grouped: false,
            expr,
        }
    }
}

impl From<Block> for PrecExpr {
    fn from(block: Block) -> Self {
        Expr::from(block).into()
    }
}
