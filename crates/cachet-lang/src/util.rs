use crate::ast::Spanned;

pub fn map_spanned<T, U>(
    x: Spanned<T>,
    f: impl FnOnce(Spanned<T>) -> Option<U>,
) -> Option<Spanned<U>> {
    let span = x.span;
    match f(x) {
        None => None,
        Some(y) => Some(Spanned::new(span, y)),
    }
}
