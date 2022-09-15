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

#[derive(Debug, Clone)]
pub struct AffixWriter<'a, W> {
    inner: W,
    line_prefix: &'a str,
    line_suffix: &'a str,
    has_started_line: bool,
}

impl<'a, W> AffixWriter<'a, W> {
    pub fn new(inner: W, line_prefix: &'a str, line_suffix: &'a str) -> Self {
        Self {
            inner,
            line_prefix,
            line_suffix,
            has_started_line: false,
        }
    }

    pub fn into_inner(self) -> W {
        self.inner
    }
}

impl<'a, W: Write> AffixWriter<'a, W> {
    fn ensure_line_started(&mut self) -> Result<(), fmt::Error> {
        if !self.has_started_line {
            self.inner.write_str(self.line_prefix)?;
            self.has_started_line = true;
        }
        Ok(())
    }
}

impl<'a, W: Write> Write for AffixWriter<'a, W> {
    fn write_str(&mut self, mut s: &str) -> Result<(), fmt::Error> {
        while !s.is_empty() {
            self.ensure_line_started()?;

            match s.find('\n') {
                Some(newline_index) => {
                    let before_newline = &s[..newline_index];
                    let after_newline = &s[newline_index + 1..];

                    self.inner.write_str(before_newline)?;
                    self.inner.write_str(self.line_suffix)?;
                    self.inner.write_char('\n')?;
                    s = after_newline;
                    self.has_started_line = false;
                }
                None => {
                    self.inner.write_str(s)?;
                    break;
                }
            }
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> Result<(), fmt::Error> {
        self.ensure_line_started()?;
        if c == '\n' {
            self.has_started_line = false;
        }

        self.inner.write_char(c)?;
        Ok(())
    }
}
