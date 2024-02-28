use std::{io::Write, path::PathBuf};

use clap::Parser;
use phf::phf_map;
use rura_lir::{
    pass::{
        analysis::fmt_analysis_errors,
        diagnostic::{fmt_diagnostic_messages, DiagnosticLevel},
        PassEntry,
    },
    pprint::PrettyPrint,
};
use winnow::Located;

static PASSES: phf::Map<&'static str, PassEntry> = phf_map! {
    "type_inference" => rura_lir::pass::analysis::type_inference::get_pass,
    "improper_termination" => rura_lir::pass::diagnostic::improper_termination::get_pass,
    "variable_definition" => rura_lir::pass::diagnostic::variable_definition::get_pass,
};

#[derive(Debug, serde::Deserialize)]
struct PassConfig {
    pub name: String,
    pub config: toml::Table,
}

#[derive(Debug, serde::Deserialize)]
struct PassSchedule {
    pub passes: Vec<PassConfig>,
}

#[derive(Debug, clap::Parser)]
pub struct Opt {
    /// Path to the input IR file
    #[clap(short, long)]
    pub input: PathBuf,
    /// Path to the output IR file
    #[clap(short, long)]
    pub output: PathBuf,
    /// Path to the pass schedule file
    #[clap(short, long)]
    pub schedule: PathBuf,
}

// TODO: handle errors more gracefully
fn main() {
    let opt = Opt::parse();
    let schedule = std::fs::read_to_string(&opt.schedule).unwrap();
    let schedule: PassSchedule = toml::from_str(&schedule).unwrap();

    let module = std::fs::read_to_string(&opt.input).unwrap();
    let mut input = Located::new(module.as_str());
    let module = rura_lir::parser::parse_module(&mut input).unwrap();

    let mut passes = Vec::new();
    for pass in schedule.passes.iter() {
        let entry = PASSES.get(pass.name.as_str()).unwrap();
        passes.push(entry(&module, &pass.config));
    }

    let mut err_buffer = String::new();
    for pass in passes.iter_mut() {
        match pass {
            rura_lir::pass::BoxedPass::Analysis(pass) => {
                let errors = pass.analyze(&module);
                fmt_analysis_errors(&mut err_buffer, &errors).unwrap();
                if !errors.is_empty() {
                    std::process::exit(1);
                }
            }
            rura_lir::pass::BoxedPass::Diagnostic(pass) => {
                let diagnostics = pass.diagnose(&module);
                fmt_diagnostic_messages(&mut err_buffer, &diagnostics).unwrap();
                if diagnostics
                    .iter()
                    .any(|x| matches!(x.level, DiagnosticLevel::Error))
                {
                    std::process::exit(1);
                }
            }
        }
    }

    let mut output = std::fs::File::create(&opt.output).unwrap();
    write!(output, "{}", PrettyPrint::new(&module)).unwrap();
}
