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

use cachet_lang::flattener::flatten;
use cachet_lang::normalizer::normalize;
use cachet_lang::parser::parse;
use cachet_lang::resolver::{resolve, ResolveErrors};
use cachet_lang::type_checker::{type_check, TypeCheckErrors};
use cachet_lang::FrontendError;

use cachet_compiler::{compile_bpl, compile_cpp};

// TODO(spinda): Change to this format for specifying compiler outputs:
//
//   cachet-compiler foo.cachet \
//      --cpp-decls foo.h       \
//      --cpp-defs foo.inc      \
//      --bpl foo.bpl

/// The Cachet compiler.
#[derive(StructOpt)]
#[structopt(global_settings = &[AppSettings::DeriveDisplayOrder])]
struct Opt {
    /// Input source file (typically ending in .cachet).
    #[structopt(parse(from_os_str))]
    input: PathBuf,
    /// Output file for declarations (typically ending in .h or .hpp).
    #[structopt(
        parse(from_os_str),
        required_unless("dry-run"),
        requires("defs-output")
    )]
    decls_output: Option<PathBuf>,
    /// Output file for definitions (typically ending in .inc or .cpp).
    #[structopt(
        parse(from_os_str),
        required_unless("dry-run"),
        requires("decls-output")
    )]
    defs_output: Option<PathBuf>,
    /// Output file for specification (typically ending in .bpl).
    #[structopt(parse(from_os_str), required_unless("dry-run"))]
    spec_output: Option<PathBuf>,

    /// Skip writing output files.
    #[structopt(long)]
    dry_run: bool,
    /// Dump parser output.
    #[structopt(long)]
    dump_parser: bool,
    /// Dump name resolver output.
    #[structopt(long)]
    dump_resolver: bool,
    /// Dump type checker output.
    #[structopt(long)]
    dump_type_checker: bool,
    /// Dump normalizer output.
    #[structopt(long)]
    dump_normalizer: bool,
    /// Dump flattener output.
    #[structopt(long)]
    dump_flattener: bool,
}

fn main() -> Result<(), Error> {
    let opt = Opt::from_args();

    let src = read_to_string(&opt.input)
        .with_context(|| format!("Failed to read {}", opt.input.display()))?;

    let items = match parse(&src) {
        Ok(items) => items,
        Err(error) => {
            report_all(&opt.input.display(), &src, iter::once(&error));
            return Err(Error::msg(format!(
                "Failed to parse {}",
                opt.input.display()
            )));
        }
    };
    if opt.dump_parser {
        println!("=== PARSER ===\n\n{:#?}\n\n", items);
    }

    let env = match resolve(items) {
        Ok(env) => env,
        Err(ResolveErrors(errors)) => {
            report_all(&opt.input.display(), &src, errors.iter());
            return Err(Error::msg(format!(
                "Failed to resolve names in {}",
                opt.input.display()
            )));
        }
    };
    if opt.dump_resolver {
        println!("=== RESOLVER ===\n\n{:#?}\n\n", env);
    }

    let env = match type_check(env) {
        Ok(env) => env,
        Err(TypeCheckErrors(errors)) => {
            report_all(&opt.input.display(), &src, errors.iter());
            return Err(Error::msg(format!(
                "Failed to type check {}",
                opt.input.display()
            )));
        }
    };
    if opt.dump_type_checker {
        println!("=== TYPE CHECKER ===\n\n{:#?}\n\n", env);
    }

    let env = normalize(env);
    if opt.dump_normalizer {
        println!("=== NORMALIZER ===\n\n{:#?}\n\n", env);
    }

    let cpp_compiler_output = compile_cpp(&env);

    if !opt.dry_run {
        let decls_output = opt.decls_output.unwrap();
        write!(
            File::create(&decls_output)
                .with_context(|| format!("Failed to open {}", decls_output.display()))?,
            "{}",
            cpp_compiler_output.decls
        )
        .with_context(|| format!("Failed to write {}", decls_output.display()))?;

        let defs_output = opt.defs_output.unwrap();
        write!(
            File::create(&defs_output)
                .with_context(|| format!("Failed to open {}", defs_output.display()))?,
            "{}",
            cpp_compiler_output.defs
        )
        .with_context(|| format!("Failed to write {}", defs_output.display()))?;
    }

    let env = flatten(env);
    if opt.dump_flattener {
        println!("=== FLATTENER ===\n\n{:#?}\n\n", env);
    }

    let bpl_code = compile_bpl(&env);

    if !opt.dry_run {
        let spec_output = opt.spec_output.unwrap();
        write!(
            File::create(&spec_output)
                .with_context(|| format!("Failed to open {}", spec_output.display()))?,
            "{}",
            bpl_code
        )
        .with_context(|| format!("Failed to write {}", spec_output.display()))?;
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
