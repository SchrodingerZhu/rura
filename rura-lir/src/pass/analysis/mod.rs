#![allow(unused)]
use std::{any::Any, fmt::Write};

use rura_core::{
    fmt_separated,
    lir::{
        ir::{LExpr, Module},
        QualifiedName,
    },
};

use super::{visitor::VisitorContext, Pass};

pub mod free_variable;
pub mod type_inference;

pub struct AnalysisError<'a> {
    context: Box<[VisitorContext<'a>]>,
    message: Box<dyn std::error::Error>,
}
pub trait AnalysisPass<'a>: Pass {
    fn analyze(&mut self, module: &'a Module) -> Box<[AnalysisError<'a>]>;
}

// TODO: dedup the macro
#[cfg(feature = "colorful-diagnostic")]
macro_rules! ansi {
    ($color:ident, $x:expr) => {
        ::nu_ansi_term::Color::$color.paint($x)
    };
}

#[cfg(not(feature = "colorful-diagnostic"))]
macro_rules! ansi {
    ($color:ident, $x:expr) => {
        $x
    };
}

pub fn fmt_analysis_errors<'a, W: Write>(
    f: &mut W,
    messages: &'a [AnalysisError<'a>],
) -> std::fmt::Result {
    for message in messages {
        write!(f, "{} ", ansi!(Red, "[error]"))?;
        fmt_separated(f, message.context.iter(), " > ")?;
        write!(f, ":\n\t")?;
        write!(f, "{}", message.message)?;
        writeln!(f)?;
    }
    Ok(())
}
