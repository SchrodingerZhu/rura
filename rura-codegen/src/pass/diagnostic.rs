use std::fmt::Display;

use crate::lir::Module;

use super::{Pass, PassType};

pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
}

pub struct Diagnostic<Message> {
    pub level: DiagnosticLevel,
    pub message: Message,
}

pub trait DiagnosticPass: Pass {
    const TYPE: PassType = PassType::Diagnostic;
    type Message: Display;
    fn run_diagnostic(&self, module: &Module) -> Box<[Diagnostic<Self::Message>]>;
}

pub struct UndefinedVariablePass;

impl Pass for UndefinedVariablePass {
    const IDENTIFER: &'static str = "undefined-variable";
    const TYPE: PassType = PassType::Diagnostic;
}

// TODO: Implement the diagnostic pass for undefined variables
