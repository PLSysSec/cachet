use std::borrow::Borrow;
use std::fmt::{self, Debug};
use std::ops::Deref;

use derive_more::Display;
use internment::Intern;

/// [Grammar](https://boogie-docs.readthedocs.io/en/latest/LangRef.html#grammar-token-ident):
///
/// ```grammar
/// ident     ::= [ "\\" ] non_digit { non_digit | digit }
/// non_digit ::= ( "A…Z" | "a…z" | "'" | "~" | "#" | "$" | "^" | "_" | "." | "?" | "`" )
/// digit     ::= "0…9"
/// ```
// TODO(spinda): Expand derives.
#[derive(Clone, Copy, Display, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Ident(Intern<String>);

impl<T: ?Sized> AsRef<T> for Ident
where
    String: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        (**self).as_ref()
    }
}

impl<T: ?Sized> Borrow<T> for Ident
where
    String: Borrow<T>,
{
    fn borrow(&self) -> &T {
        (**self).borrow()
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Debug::fmt(&**self, f)
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(s.into())
    }
}

impl From<&String> for Ident {
    fn from(s: &String) -> Self {
        String::from(s).into()
    }
}

impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        String::from(s).into()
    }
}

impl From<&mut str> for Ident {
    fn from(s: &mut str) -> Self {
        String::from(s).into()
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        (*ident).clone()
    }
}
