use std::fmt;

pub fn fmt_join_or<T, W: fmt::Write>(
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
