// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::error::Error;

use codespan_reporting::diagnostic::Diagnostic;

use crate::ast::Span;

pub mod ast;
// TODO(spinda): pub mod flattener;
// TODO(spinda): pub mod liveness_checker;
// TODO(spinda): pub mod mut_checker;
pub mod normalizer;
pub mod parser;
// TODO(spinda): pub mod recursion_checker;
pub mod resolver;
pub mod type_checker;
// TODO(spinda): pub mod validator;
// TODO(spinda): pub mod vis_checker;
// TODO(spinda): pub mod well_formedness_checker;
mod util;

pub trait FrontendError: Error {
    fn span(&self) -> Span;
    fn build_diagnostic<T: Copy>(&self, file_id: T) -> Diagnostic<T>;
}