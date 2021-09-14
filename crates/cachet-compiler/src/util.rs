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
