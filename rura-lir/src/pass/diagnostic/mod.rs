use std::fmt::Display;

use crate::{
    fmt_separated,
    lir::{FunctionPrototype, Lir, Module},
    QualifiedName,
};

use super::Pass;

pub mod improper_termination;

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
    Closure(usize),
    ThenBlock(usize),
    ElseBlock(usize),
    Eliminator(usize, &'a QualifiedName),
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
            DiagnosticContext::Closure(i) => write!(f, "closure body at {}", i),
            DiagnosticContext::ThenBlock(i) => write!(f, "then block at {}", i),
            DiagnosticContext::ElseBlock(i) => write!(f, "else block at {}", i),
            DiagnosticContext::Eliminator(i, ctor) => {
                write!(f, "elimination body for {ctor} at {i}")
            }
            DiagnosticContext::External(name) => write!(f, "external {}", name),
            DiagnosticContext::InductiveTypeDef(name) => write!(f, "inductive type {}", name),
        }
    }
}

pub fn fmt_diagnostic_messages<'a>(
    f: &mut std::fmt::Formatter<'_>,
    messages: &'a [Diagnostic<'a>],
) -> std::fmt::Result {
    for message in messages {
        match message.level {
            DiagnosticLevel::Info => write!(f, "[info] ")?,
            DiagnosticLevel::Warning => write!(f, "[warning] ")?,
            DiagnosticLevel::Error => write!(f, "[error] ")?,
        }
        fmt_separated(f, message.context.iter(), " > ")?;
        write!(f, ":\n\t")?;
        message.message.fmt(f)?;
        writeln!(f)?;
    }
    Ok(())
}

pub trait DiagnosticPass: Pass {
    fn run_diagnostic<'a>(&mut self, module: &'a Module) -> Box<[Diagnostic<'a>]> {
        let mut agent = DiagnosticAgent::new();
        agent.push_context(DiagnosticContext::Module(&module.name));
        self.visit_module(module, &mut agent);

        for def in module.inductive_types.iter() {
            agent.push_context(DiagnosticContext::InductiveTypeDef(&def.name));
            self.visit_inductive_type_def(def, &mut agent);
            agent.pop_context();
        }

        for external in module.external_functions.iter() {
            agent.push_context(DiagnosticContext::External(&external.name));
            self.visit_external(external, &mut agent);
            agent.pop_context();
        }

        fn walk_instr<'a, S: DiagnosticPass + ?Sized>(
            pass: &mut S,
            instrs: &'a [Lir],
            agent: &mut DiagnosticAgent<'a>,
        ) {
            for (i, instr) in instrs.iter().enumerate() {
                agent.push_context(DiagnosticContext::Instrution(i));
                pass.visit_instruction(instr, agent);
                match instr {
                    Lir::IfThenElse(inner) => {
                        agent.push_context(DiagnosticContext::ThenBlock(i));
                        walk_instr(pass, &inner.then_branch.0, agent);
                        agent.pop_context();
                        agent.push_context(DiagnosticContext::ElseBlock(i));
                        walk_instr(pass, &inner.else_branch.0, agent);
                        agent.pop_context();
                    }
                    Lir::InductiveElimination { eliminator, .. } => {
                        for elim in eliminator.iter() {
                            agent.push_context(DiagnosticContext::Eliminator(i, &elim.ctor));
                            walk_instr(pass, &elim.body.0, agent);
                            agent.pop_context();
                        }
                    }
                    Lir::Closure(inner) => {
                        agent.push_context(DiagnosticContext::Closure(i));
                        walk_instr(pass, &inner.body.0, agent);
                        agent.pop_context();
                    }
                    _ => {}
                }
                agent.pop_context();
            }
        }

        for function in module.functions.iter() {
            agent.push_context(DiagnosticContext::Function(&function.prototype.name));
            self.visit_function_def(function, &mut agent);
            walk_instr(self, &function.body.0, &mut agent);
            agent.pop_context();
        }

        agent.into_messages()
    }

    fn visit_module<'a>(&mut self, module: &'a Module, context: &mut DiagnosticAgent<'a>);
    fn visit_external<'a>(
        &mut self,
        external: &'a FunctionPrototype,
        context: &mut DiagnosticAgent<'a>,
    );
    fn visit_inductive_type_def<'a>(
        &mut self,
        typedef: &'a crate::lir::InductiveTypeDef,
        context: &mut DiagnosticAgent<'a>,
    );
    fn visit_function_def<'a>(
        &mut self,
        function: &'a crate::lir::FunctionDef,
        context: &mut DiagnosticAgent<'a>,
    );
    fn visit_instruction<'a>(
        &mut self,
        instruction: &'a crate::lir::Lir,
        context: &mut DiagnosticAgent<'a>,
    );
}
