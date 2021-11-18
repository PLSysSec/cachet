// vim: set tw=99 ts=4 sts=4 sw=4 et:

use derive_more::From;

use crate::ast::{
    Hole, Ident, ParamIndex, Params, ParamsConfig, ReachableHoleConfig, Span, Spanned,
};
use crate::parser::ast::{ParserExpr, ParserOutVarParam, ParserParams, ParserVarParam};

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
                    if accum.mut_tag.replace(tag.span()).is_some() {
                        return Err(ParseError::User {
                            error: Spanned::new(tag.span(), "duplicate `mut` tag"),
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
                    if accum.unsafe_tag.replace(tag.span()).is_some() {
                        return Err(ParseError::User {
                            error: Spanned::new(tag.span(), "duplicate `unsafe` tag"),
                        });
                    }
                }
            }
        }

        Ok(accum)
    }
}

#[derive(From)]
pub enum Param {
    #[from]
    Var(ParserVarParam),
    #[from]
    OutVar(ParserOutVarParam),
    Label(Spanned<Ident>),
}

pub fn catalog_params<C: ParamsConfig, T>(
    parsed_params: Vec<Spanned<Param>>,
) -> Result<(ParserParams<C>, Vec<ParamIndex<C>>), ParseError<T>>
where
    C::HasOutVarParams: ReachableHoleConfig,
{
    let mut params = Params::default();
    let param_order = parsed_params
        .into_iter()
        .map(|param| match param.value {
            Param::Var(var_param) => Ok(params.var_params.push_and_get_key(var_param).into()),
            Param::OutVar(out_var_param) => match &mut params.out_var_params {
                Hole::Empty(_) => Err(ParseError::User {
                    error: Spanned::new(
                        *param.span,
                        "out-parameters aren't allowed in this context",
                    ),
                }),
                Hole::Full(reachable, out_var_params) => Ok(ParamIndex::OutVar(
                    *reachable,
                    out_var_params.push_and_get_key(out_var_param),
                )),
            },
            Param::Label(label_param) => {
                Ok(params.label_params.push_and_get_key(label_param).into())
            }
        })
        .collect::<Result<_, _>>()?;
    Ok((params, param_order))
}

pub struct MaybeGrouped {
    pub is_grouped: bool,
    pub expr: ParserExpr,
}

impl MaybeGrouped {
    pub fn grouped(expr: ParserExpr) -> Self {
        MaybeGrouped {
            is_grouped: true,
            expr,
        }
    }

    pub fn ungrouped(expr: ParserExpr) -> Self {
        MaybeGrouped {
            is_grouped: false,
            expr,
        }
    }
}
