// vim: set tw=99 ts=4 sts=4 sw=4 et:

use codespan::Span;

use crate::parser::ast::Spanned;

pub type ParseError<T> = lalrpop_util::ParseError<usize, T, UserParseError>;
pub type UserParseError = Spanned<&'static str>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FnDefKind {
    Fn,
    Op,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FnDefAttr {
    Fallible,
    Unsafe,
}

impl FnDefAttr {
    pub fn reduce<T>(
        attrs: impl Iterator<Item = Spanned<Self>>,
    ) -> Result<(Option<Span>, Option<Span>), ParseError<T>> {
        let mut is_fallible = None;
        let mut is_unsafe = None;

        for attr in attrs {
            match attr.value {
                FnDefAttr::Fallible => {
                    if is_fallible.replace(attr.span).is_some() {
                        return Err(ParseError::User {
                            error: Spanned {
                                span: is_fallible.take().unwrap(),
                                value: "duplicate `fallible` attribute",
                            },
                        });
                    }
                }
                FnDefAttr::Unsafe => {
                    if is_unsafe.replace(attr.span).is_some() {
                        return Err(ParseError::User {
                            error: Spanned {
                                span: is_unsafe.take().unwrap(),
                                value: "duplicate `unsafe` attribute",
                            },
                        });
                    }
                }
            }
        }

        Ok((is_fallible, is_unsafe))
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ParamVarAttr {
    Out,
}

impl ParamVarAttr {
    pub fn reduce<T>(
        attrs: impl Iterator<Item = Spanned<Self>>,
    ) -> Result<Option<Span>, ParseError<T>> {
        let mut is_out = None;

        for attr in attrs {
            match attr.value {
                ParamVarAttr::Out => {
                    if is_out.replace(attr.span).is_some() {
                        return Err(ParseError::User {
                            error: Spanned {
                                span: is_out.take().unwrap(),
                                value: "duplicate `out` attribute",
                            },
                        });
                    }
                }
            }
        }

        Ok(is_out)
    }
}
