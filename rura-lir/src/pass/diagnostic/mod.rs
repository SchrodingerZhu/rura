use std::fmt::{Display, Write};

use rura_core::fmt_separated;
use rura_core::lir::ir::Module;

use super::visitor::{LirVisitor, TracingContext, VisitorContext};

pub mod improper_termination;
pub mod variable_definition;

pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
}

pub struct Diagnostic<'a> {
    pub level: DiagnosticLevel,
    pub message: Box<dyn Display + 'a>,
    pub context: Box<[VisitorContext<'a>]>,
}

pub struct DiagnosticAgent<'a> {
    context: Vec<VisitorContext<'a>>,
    messages: Vec<Diagnostic<'a>>,
}

impl<'a> DiagnosticAgent<'a> {
    fn new() -> Self {
        Self {
            context: Vec::new(),
            messages: Vec::new(),
        }
    }

    fn push_context(&mut self, context: VisitorContext<'a>) {
        self.context.push(context);
    }

    fn pop_context(&mut self) {
        self.context.pop();
    }

    pub fn add_diagnostic(&mut self, level: DiagnosticLevel, message: impl Display + 'a) {
        self.messages.push(Diagnostic {
            level,
            message: Box::new(message),
            context: self.context.clone().into(),
        });
    }

    fn into_messages(self) -> Box<[Diagnostic<'a>]> {
        self.messages.into()
    }
}

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

pub fn fmt_diagnostic_messages<'a, W: Write>(
    f: &mut W,
    messages: &'a [Diagnostic<'a>],
) -> std::fmt::Result {
    for message in messages {
        match message.level {
            DiagnosticLevel::Info => write!(f, "{} ", ansi!(Green, "[info]"))?,
            DiagnosticLevel::Warning => write!(f, "{} ", ansi!(Yellow, "[warning]"))?,
            DiagnosticLevel::Error => write!(f, "{} ", ansi!(Red, "[error]"))?,
        }
        fmt_separated(f, message.context.iter(), " > ")?;
        write!(f, ":\n\t")?;
        write!(f, "{}", message.message)?;
        writeln!(f)?;
    }
    Ok(())
}

impl<'a> TracingContext<'a> for DiagnosticAgent<'a> {
    fn push_context(&mut self, context: VisitorContext<'a>) {
        self.push_context(context);
    }

    fn pop_context(&mut self) {
        self.pop_context();
    }
}

pub trait DiagnosticPass<'a> {
    fn diagnose(&mut self, module: &'a Module) -> Box<[Diagnostic<'a>]>;
}

pub fn default_diagnose_module<'a, S: LirVisitor<Context<'a> = DiagnosticAgent<'a>> + ?Sized>(
    this: &mut S,
    module: &'a Module,
) -> Box<[Diagnostic<'a>]> {
    let mut agent = DiagnosticAgent::new();
    this.visit_module(module, &mut agent);
    agent.into_messages()
}
