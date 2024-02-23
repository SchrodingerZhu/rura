use std::fmt::Display;

use crate::{
    lir::{FunctionPrototype, Lir, Module},
    QualifiedName,
};

use super::Pass;

mod improper_termination;

pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
}

pub enum DiagnosticContext<'a> {
    Module(&'a QualifiedName),
    Function(&'a QualifiedName),
    Instrution(usize),
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

pub trait DiagnosticPass: Pass {
    fn run_diagnostic<'a>(&mut self, module: &'a Module) -> Box<[Diagnostic<'a>]> {
        let mut messages = Vec::new();
        let mut context = Vec::new();
        context.push(DiagnosticContext::Module(&module.name));
        self.visit_module(module, &context, &mut messages);

        for def in module.inductive_types.iter() {
            context.push(DiagnosticContext::InductiveTypeDef(&def.name));
            self.visit_inductive_type_def(def, &context, &mut messages);
            context.pop();
        }

        for external in module.external_functions.iter() {
            context.push(DiagnosticContext::External(&external.name));
            self.visit_external(external, &context, &mut messages);
            context.pop();
        }

        fn walk_instr<'a, S: DiagnosticPass + ?Sized>(
            pass: &mut S,
            instrs: &'a [Lir],
            context: &mut Vec<DiagnosticContext<'a>>,
            messages: &mut Vec<Diagnostic<'a>>,
        ) {
            for (i, instr) in instrs.iter().enumerate() {
                context.push(DiagnosticContext::Instrution(i));
                pass.visit_instruction(instr, context, messages);
                match instr {
                    Lir::IfThenElse(inner) => {
                        context.push(DiagnosticContext::ThenBlock(i));
                        walk_instr(pass, &inner.then_branch.0, context, messages);
                        context.pop();
                        context.push(DiagnosticContext::ElseBlock(i));
                        walk_instr(pass, &inner.else_branch.0, context, messages);
                        context.pop();
                    }
                    Lir::InductiveElimination { eliminator, .. } => {
                        for elim in eliminator.iter() {
                            context.push(DiagnosticContext::Eliminator(i, &elim.ctor));
                            walk_instr(pass, &elim.body.0, context, messages);
                            context.pop();
                        }
                    }
                    _ => {}
                }
                context.pop();
            }
        }

        for function in module.functions.iter() {
            context.push(DiagnosticContext::Function(&function.prototype.name));
            self.visit_function_def(function, &context, &mut messages);
            walk_instr(self, &function.body.0, &mut context, &mut messages);
            context.pop();
        }

        messages.into()
    }

    fn visit_module<'a>(
        &mut self,
        module: &'a Module,
        context: &[DiagnosticContext<'a>],
        diagnostics: &mut Vec<Diagnostic<'a>>,
    );
    fn visit_external<'a>(
        &mut self,
        module: &'a FunctionPrototype,
        context: &[DiagnosticContext<'a>],
        diagnostics: &mut Vec<Diagnostic<'a>>,
    );
    fn visit_inductive_type_def<'a>(
        &mut self,
        module: &'a crate::lir::InductiveTypeDef,
        context: &[DiagnosticContext<'a>],
        diagnostics: &mut Vec<Diagnostic<'a>>,
    );
    fn visit_function_def<'a>(
        &mut self,
        function: &'a crate::lir::FunctionDef,
        context: &[DiagnosticContext<'a>],
        diagnostics: &mut Vec<Diagnostic<'a>>,
    );
    fn visit_instruction<'a>(
        &mut self,
        instruction: &'a crate::lir::Lir,
        context: &[DiagnosticContext<'a>],
        diagnostics: &mut Vec<Diagnostic<'a>>,
    );
}
