use std::{
    io::{Read, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use phf::phf_map;
use rura_lir::{
    pass::{
        analysis::fmt_analysis_errors,
        diagnostic::{fmt_diagnostic_messages, DiagnosticLevel},
        BoxedPass, PassEntry,
    },
    pprint::PrettyPrint,
};
use winnow::Located;

static PASSES: phf::Map<&'static str, PassEntry> = phf_map! {
    "type_inference" => rura_lir::pass::analysis::type_inference::get_pass,
    "improper_termination" => rura_lir::pass::diagnostic::improper_termination::get_pass,
    "variable_definition" => rura_lir::pass::diagnostic::variable_definition::get_pass,
};

fn default_schedule() -> PassSchedule {
    PassSchedule {
        passes: vec![
            PassConfig {
                name: "improper_termination".to_string(),
                config: toml::Table::new(),
            },
            PassConfig {
                name: "variable_definition".to_string(),
                config: toml::Table::new(),
            },
            PassConfig {
                name: "type_inference".to_string(),
                config: toml::Table::new(),
            },
        ],
    }
}

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
pub enum SubOpt {
    /// Run passes according to the schedule
    Scheduled {
        /// Path to the pass schedule file (if not provided, use the default schedule)
        #[clap(short, long)]
        schedule: Option<PathBuf>,
    },
    /// Single pass name and its configuration
    Single {
        /// Name of the pass
        #[clap(short, long)]
        name: String,
        /// Configuration of the pass
        #[clap(short, long, default_value_t = Default::default())]
        config: toml::Table,
    },
    /// List all available passes
    List,
}

#[derive(Debug, clap::Parser)]
pub struct Opt {
    /// Path to the input IR file (if not provided, read from stdin)
    #[clap(short, long)]
    pub input: Option<PathBuf>,
    /// Path to the output IR file (if not provided, write to stdout)
    #[clap(short, long)]
    pub output: Option<PathBuf>,
    #[clap(subcommand)]
    pub sub_opt: Option<SubOpt>,
}

fn run_pass<'a>(pass: &mut BoxedPass<'a>, module: &'a rura_core::lir::ir::Module) {
    let mut err_buffer = String::new();
    match pass {
        rura_lir::pass::BoxedPass::Analysis(pass) => {
            let errors = pass.analyze(module);
            fmt_analysis_errors(&mut err_buffer, &errors).unwrap();
            eprint!("{}", err_buffer);
            if !errors.is_empty() {
                std::process::exit(1);
            }
        }
        rura_lir::pass::BoxedPass::Diagnostic(pass) => {
            let diagnostics = pass.diagnose(module);
            fmt_diagnostic_messages(&mut err_buffer, &diagnostics).unwrap();
            eprint!("{}", err_buffer);
            if diagnostics
                .iter()
                .any(|x| matches!(x.level, DiagnosticLevel::Error))
            {
                std::process::exit(1);
            }
        }
    }
}

fn run_schedule<P: AsRef<Path>>(schedule_path: Option<P>, module: &rura_core::lir::ir::Module) {
    let schedule = match schedule_path {
        Some(schedule) => {
            let schedule = std::fs::read_to_string(schedule).unwrap();
            toml::from_str(&schedule).unwrap()
        }
        None => default_schedule(),
    };
    let mut passes = Vec::new();

    for pass in schedule.passes.iter() {
        let entry = PASSES.get(pass.name.as_str()).unwrap();
        passes.push(entry(module, &pass.config));
    }

    for pass in passes.iter_mut() {
        run_pass(pass, module);
    }
}

fn run_single(name: String, config: toml::Table, module: &rura_core::lir::ir::Module) {
    let entry = PASSES.get(name.as_str()).unwrap();
    let mut pass = entry(module, &config);
    run_pass(&mut pass, module);
}

fn run_list() {
    for name in PASSES.keys() {
        println!("{}", name);
    }
}

// TODO: handle errors more gracefully
fn main() {
    let opt = Opt::parse();

    if matches!(opt.sub_opt, Some(SubOpt::List)) {
        run_list();
        return;
    }

    let module = match opt.input {
        Some(input) => std::fs::read_to_string(input).unwrap(),
        None => {
            let mut buffer = String::new();
            std::io::stdin().read_to_string(&mut buffer).unwrap();
            buffer
        }
    };
    let mut input = Located::new(module.as_str());
    let module = rura_lir::parser::parse_module(&mut input).unwrap();

    match opt.sub_opt {
        None => run_schedule::<&PathBuf>(None, &module),
        Some(SubOpt::Scheduled { schedule }) => run_schedule(schedule, &module),
        Some(SubOpt::Single { name, config }) => run_single(name, config, &module),
        Some(SubOpt::List) => unreachable!(),
    }

    match opt.output {
        Some(output) => {
            let mut output = std::fs::File::create(output).unwrap();
            write!(output, "{}", PrettyPrint::new(&module)).unwrap();
        }
        None => {
            print!("{}", PrettyPrint::new(&module));
        }
    }
}
