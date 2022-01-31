use std::fmt::{self, Display, Write};

pub fn fmt_join<T: Display>(
    f: &mut impl Write,
    sep: impl Display,
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

pub fn fmt_join_or<T, W: Write>(
    f: &mut W,
    iter: impl Iterator<Item = T>,
    fmt_item: impl Fn(&mut W, T) -> Result<(), fmt::Error>,
) -> Result<(), fmt::Error> {
    let mut iter = iter.peekable();
    if let Some(first_item) = iter.next() {
        fmt_item(f, first_item)?;
        if let Some(second_item) = iter.next() {
            write!(
                f,
                "{}",
                match iter.peek() {
                    Some(_) => ", ",
                    None => " or ",
                }
            )?;
            fmt_item(f, second_item)?;
            while let Some(item) = iter.next() {
                write!(
                    f,
                    "{}",
                    match iter.peek() {
                        Some(_) => ", ",
                        None => ", or ",
                    }
                )?;
                fmt_item(f, item)?;
            }
        }
    }
    Ok(())
}

pub fn fmt_join_leading<T: fmt::Display>(
    f: &mut impl fmt::Write,
    sep: impl fmt::Display,
    iter: impl Iterator<Item = T>,
) -> Result<(), fmt::Error> {
    for item in iter {
        write!(f, "{}{}", sep, item)?;
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
