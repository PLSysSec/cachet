// vim: set tw=99 ts=4 sts=4 sw=4 et:

#![feature(type_alias_impl_trait)]

pub use bpl::{compile as compile_bpl, BplCode};
pub use cpp::{compile as compile_cpp, CppCode, CppCompilerOutput};

mod bpl;
mod cpp;
