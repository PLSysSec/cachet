// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt;

pub fn fmt_join<T: fmt::Display>(
    f: &mut impl fmt::Write,
    sep: impl fmt::Display,
    mut iter: impl Iterator<Item = T>,
) -> Result<(), fmt::Error> {
    if let Some(first_item) = iter.next() {
        write!(f, "{}", first_item)?;
        for item in iter {
            write!(f, "{}{}", sep, item)?;
        }
    }
    Ok(())
}

pub fn fmt_join_trailing<T: fmt::Display>(
    f: &mut impl fmt::Write,
    sep: impl fmt::Display,
    iter: impl Iterator<Item = T>,
) -> Result<(), fmt::Error> {
    for item in iter {
        write!(f, "{}{}", item, sep)?;
    }
    Ok(())
}

macro_rules! chain_from {
    (
        $src:ty => $mid:ty => $dst:ty
        $(| $(<$($generic_params:ident),+>)? $(where $($where_clause:tt)+)?)?
    ) => {
        impl$($(<$($generic_params),+>)?)? ::std::convert::From<$src> for $dst
        $($(where $($where_clause)+)?)?
        {
            fn from(src: $src) -> Self {
                ::std::convert::From::from(::std::convert::Into::<$mid>::into(src))
            }
        }
    };
}
pub(crate) use chain_from;
