use std::fmt::{Display, Write};

use crate::{
    fmt_separated,
    lir::{FunctionPrototype, Lir, Module},
    QualifiedName,
};

use super::Pass;

pub mod improper_termination;
pub mod variable_definition;

pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub enum DiagnosticContext<'a> {
    Module(&'a QualifiedName),
    Function(&'a QualifiedName),
    Instrution(usize),
    Closure,
    ThenBlock,
    ElseBlock,
    Eliminator(&'a QualifiedName),
    External(&'a QualifiedName),
    InductiveTypeDef(&'a QualifiedName),
}

pub struct Diagnostic<'a> {
    pub level: DiagnosticLevel,
    pub message: Box<dyn Display + 'a>,
    pub context: Box<[DiagnosticContext<'a>]>,
}

pub struct DiagnosticAgent<'a> {
    context: Vec<DiagnosticContext<'a>>,
    messages: Vec<Diagnostic<'a>>,
}

impl<'a> DiagnosticAgent<'a> {
    fn new() -> Self {
        Self {
            context: Vec::new(),
            messages: Vec::new(),
        }
    }

    fn push_context(&mut self, context: DiagnosticContext<'a>) {
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

impl std::fmt::Display for DiagnosticContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagnosticContext::Module(name) => write!(f, "module {}", name),
            DiagnosticContext::Function(name) => write!(f, "function {}", name),
            DiagnosticContext::Instrution(i) => write!(f, "instruction at {}", i),
            DiagnosticContext::Closure => write!(f, "closure body"),
            DiagnosticContext::ThenBlock => write!(f, "then block"),
            DiagnosticContext::ElseBlock => write!(f, "else block"),
            DiagnosticContext::Eliminator(ctor) => {
                write!(f, "elimination body for {ctor}")
            }
            DiagnosticContext::External(name) => write!(f, "external {}", name),
            DiagnosticContext::InductiveTypeDef(name) => write!(f, "inductive type {}", name),
        }
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

pub fn default_visit_function_def<'a, S: DiagnosticPass + ?Sized>(
    this: &mut S,
    function: &'a crate::lir::FunctionDef,
    agent: &mut DiagnosticAgent<'a>,
) {
    this.visit_block(&function.body, agent);
}

pub fn default_visit_block<'a, S: DiagnosticPass + ?Sized>(
    this: &mut S,
    function: &'a crate::lir::Block,
    agent: &mut DiagnosticAgent<'a>,
) {
    for (i, instr) in function.0.iter().enumerate() {
        agent.push_context(DiagnosticContext::Instrution(i));
        match instr {
            Lir::IfThenElse(inner) => this.visit_if_then_else(inner, agent),
            Lir::Closure(inner) => this.visit_closure(inner, agent),
            Lir::InductiveElimination {
                eliminator,
                inductive,
            } => this.visit_eliminator(*inductive, eliminator, agent),
            _ => this.visit_normal_instruction(instr, agent),
        }
        agent.pop_context();
    }
}

pub fn default_visit_if_then_else<'a, S: DiagnosticPass + ?Sized>(
    this: &mut S,
    inner: &'a crate::lir::IfThenElse,
    agent: &mut DiagnosticAgent<'a>,
) {
    agent.push_context(DiagnosticContext::ThenBlock);
    this.visit_block(&inner.then_branch, agent);
    agent.pop_context();
    agent.push_context(DiagnosticContext::ElseBlock);
    this.visit_block(&inner.else_branch, agent);
    agent.pop_context();
}

pub fn default_visit_closure<'a, S: DiagnosticPass + ?Sized>(
    this: &mut S,
    inner: &'a crate::lir::ClosureCreation,
    agent: &mut DiagnosticAgent<'a>,
) {
    agent.push_context(DiagnosticContext::Closure);
    this.visit_block(&inner.body, agent);
    agent.pop_context();
}

pub fn default_visit_eliminator<'a, S: DiagnosticPass + ?Sized>(
    this: &mut S,
    _inductive: usize,
    eliminator: &'a [crate::lir::InductiveEliminator],
    agent: &mut DiagnosticAgent<'a>,
) {
    for elim in eliminator.iter() {
        agent.push_context(DiagnosticContext::Eliminator(&elim.ctor));
        this.visit_block(&elim.body, agent);
        agent.pop_context();
    }
}

pub fn default_visit_module<'a, S: DiagnosticPass + ?Sized>(
    this: &mut S,
    module: &'a Module,
    agent: &mut DiagnosticAgent<'a>,
) {
    agent.push_context(DiagnosticContext::Module(&module.name));

    for def in module.inductive_types.iter() {
        this.visit_inductive_typedef(def, agent);
    }

    for external in module.external_functions.iter() {
        this.visit_external(external, agent);
    }

    for function in module.functions.iter() {
        agent.push_context(DiagnosticContext::Function(&function.prototype.name));
        this.visit_function_def(function, agent);
        agent.pop_context();
    }
}

pub trait DiagnosticPass: Pass {
    fn run_diagnostic<'a>(&mut self, module: &'a Module) -> Box<[Diagnostic<'a>]> {
        let mut agent = DiagnosticAgent::new();
        self.visit_module(module, &mut agent);
        agent.into_messages()
    }

    fn visit_if_then_else<'a>(
        &mut self,
        inner: &'a crate::lir::IfThenElse,
        agent: &mut DiagnosticAgent<'a>,
    ) {
        default_visit_if_then_else(self, inner, agent);
    }

    fn visit_closure<'a>(
        &mut self,
        inner: &'a crate::lir::ClosureCreation,
        agent: &mut DiagnosticAgent<'a>,
    ) {
        default_visit_closure(self, inner, agent);
    }

    fn visit_eliminator<'a>(
        &mut self,
        inductive: usize,
        eliminator: &'a [crate::lir::InductiveEliminator],
        agent: &mut DiagnosticAgent<'a>,
    ) {
        default_visit_eliminator(self, inductive, eliminator, agent);
    }

    fn visit_block<'a>(&mut self, inner: &'a crate::lir::Block, agent: &mut DiagnosticAgent<'a>) {
        default_visit_block(self, inner, agent);
    }

    #[allow(unused_variables)]
    fn visit_module<'a>(&mut self, module: &'a Module, context: &mut DiagnosticAgent<'a>) {
        default_visit_module(self, module, context);
    }
    #[allow(unused_variables)]
    fn visit_external<'a>(
        &mut self,
        external: &'a FunctionPrototype,
        context: &mut DiagnosticAgent<'a>,
    ) {
    }
    #[allow(unused_variables)]
    fn visit_inductive_typedef<'a>(
        &mut self,
        typedef: &'a crate::lir::InductiveTypeDef,
        context: &mut DiagnosticAgent<'a>,
    ) {
    }
    #[allow(unused_variables)]
    fn visit_function_def<'a>(
        &mut self,
        function: &'a crate::lir::FunctionDef,
        context: &mut DiagnosticAgent<'a>,
    ) {
        default_visit_function_def(self, function, context);
    }
    #[allow(unused_variables)]
    fn visit_normal_instruction<'a>(
        &mut self,
        instruction: &'a crate::lir::Lir,
        context: &mut DiagnosticAgent<'a>,
    ) {
    }
}
