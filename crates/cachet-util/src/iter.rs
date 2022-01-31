use std::iter::{FromIterator, Sum};

pub fn collect_eager<T, U: FromIterator<T>>(mut iter: impl Iterator<Item = T>) -> U {
    let result = U::from_iter(&mut iter);
    for _ in iter {}
    result
}

pub fn sum_eager<T, U: Sum<T>>(mut iter: impl Iterator<Item = T>) -> U {
    let result = U::sum(&mut iter);
    for _ in iter {}
    result
}
