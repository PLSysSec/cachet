// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;

use codespan::Span;
use codespan_reporting::diagnostic::Diagnostic;

pub mod parser;
pub mod resolver;
pub mod type_checker;
mod util;

pub trait FrontendError: Error {
    fn span(&self) -> Span;
    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T>;
}
