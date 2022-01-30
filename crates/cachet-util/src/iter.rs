use std::iter::FromIterator;

pub fn collect_eager<T, U: FromIterator<T>>(mut iter: impl Iterator<Item = T>) -> U {
    let result = U::from_iter(&mut iter);
    for _ in iter {}
    result
}
