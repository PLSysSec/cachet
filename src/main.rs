// vim: set tw=99 ts=4 sts=4 sw=4 et:

use std::fmt;
use std::fs::{read_to_string, File};
use std::io::prelude::*;
use std::iter;
use std::path::PathBuf;

use anyhow::{Context, Error};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use structopt::clap::AppSettings;
use structopt::StructOpt;

use cachet::backend::interpreter::Interpreter;

use cachet::frontend::parser::parse;
use cachet::frontend::resolver::{resolve, ResolveErrors};
use cachet::frontend::type_checker::{type_check, TypeCheckErrors};
use cachet::frontend::FrontendError;

/// The Cachet compiler.
#[derive(StructOpt)]
#[structopt(global_settings = &[AppSettings::DeriveDisplayOrder])]
struct Opt {
    /// Input specification file (typically ending in .cachet).
    #[structopt(parse(from_os_str))]
    spec_input: PathBuf,
    /// Output file for forward declarations (typically ending in .h or .hpp).
    #[structopt(
        parse(from_os_str),
        required_unless("dry-run"),
        requires("defs-output")
    )]
    fwd_decls_output: Option<PathBuf>,
    /// Output file for definitions (typically ending in .inc or .cpp).
    #[structopt(
        parse(from_os_str),
        required_unless("dry-run"),
        requires("fwd-decls-output"),
    )]
    defs_output: Option<PathBuf>,

    /// Skip writing output files.
    #[structopt(long)]
    dry_run: bool,

    /// Dump parsing output.
    #[structopt(long)]
    dump_parsing: bool,
    /// Dump name resolution output.
    #[structopt(long)]
    dump_name_resolution: bool,
    /// Dump type checking output.
    #[structopt(long)]
    dump_type_checking: bool,
}

fn main() -> Result<(), Error> {
    let opt = Opt::from_args();

    let spec_src = read_to_string(&opt.spec_input)
        .with_context(|| format!("Failed to read {}", opt.spec_input.display()))?;

    let spec = match parse(&spec_src) {
        Ok(spec) => spec,
        Err(error) => {
            report_all(&opt.spec_input.display(), &spec_src, iter::once(&error));
            return Err(Error::msg(format!(
                "Failed to parse {}",
                opt.spec_input.display()
            )));
        }
    };
    if opt.dump_parsing {
        eprintln!("=== PARSING ===\n\n{:#?}\n\n", spec);
    }

    let env = match resolve(spec) {
        Ok(env) => env,
        Err(ResolveErrors(errors)) => {
            report_all(&opt.spec_input.display(), &spec_src, errors.iter());
            return Err(Error::msg(format!(
                "Failed to resolve names in {}",
                opt.spec_input.display()
            )));
        }
    };
    if opt.dump_name_resolution {
        eprintln!("=== NAME RESOLUTION ===\n\n{:#?}\n\n", env);
    }

    let env = match type_check(env) {
        Ok(env) => env,
        Err(TypeCheckErrors(errors)) => {
            report_all(&opt.spec_input.display(), &spec_src, errors.iter());
            return Err(Error::msg(format!(
                "Failed to type check {}",
                opt.spec_input.display()
            )));
        }
    };
    if opt.dump_type_checking {
        eprintln!("=== TYPE CHECKING ===\n\n{:#?}\n\n", env);
    }

    let interpreter = Interpreter::generate(&env);

    if !opt.dry_run {
        let fwd_decls_output = opt.fwd_decls_output.unwrap();
        write!(
            File::create(&fwd_decls_output)
                .with_context(|| format!("Failed to open {}", fwd_decls_output.display()))?,
            "{}",
            interpreter.fwd_decls
        )
        .with_context(|| format!("Failed to write {}", fwd_decls_output.display()))?;

        let defs_output = opt.defs_output.unwrap();
        write!(
            File::create(&defs_output)
                .with_context(|| format!("Failed to open {}", defs_output.display()))?,
            "{}",
            interpreter.defs
        )
        .with_context(|| format!("Failed to write {}", defs_output.display()))?;
    }

    Ok(())
}

fn report_all<'a, N: Clone + fmt::Display, E: 'a + FrontendError>(
    name: N,
    src: &str,
    errors: impl Iterator<Item = &'a E>,
) -> () {
    let file = SimpleFile::new(name, src);

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    for error in errors {
        let diagnostic = error.build_diagnostic(());
        term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
    }
}
