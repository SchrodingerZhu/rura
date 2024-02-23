use crate::{lir::Lir, pass::Pass};

use super::{Action, DiagnosticPass};

pub struct ImproperTermination;

impl Pass for ImproperTermination {
    fn get_identifier(&self) -> &str {
        "improper_termination"
    }
}

impl DiagnosticPass for ImproperTermination {
    fn visit_function_def<'a>(
        &mut self,
        function: &'a crate::lir::FunctionDef,
        context: &mut super::DiagnosticAgent<'a>,
    ) -> Action {
        if !function.body.is_terminated() {
            context.add_diagnostic(
                super::DiagnosticLevel::Error,
                "function body is improperly terminated",
            );
        }
        Action::Continue
    }

    fn visit_instruction<'a>(
        &mut self,
        instruction: &'a crate::lir::Lir,
        context: &mut super::DiagnosticAgent<'a>,
    ) -> Action {
        match instruction {
            Lir::Closure(inner) => {
                if !inner.body.is_terminated() {
                    context.add_diagnostic(
                        super::DiagnosticLevel::Error,
                        "closure body is improperly terminated",
                    );
                }
            }
            Lir::IfThenElse(inner) => {
                if !inner.then_branch.is_terminated() {
                    context.add_diagnostic(
                        super::DiagnosticLevel::Error,
                        "then block is improperly terminated",
                    );
                }
                if !inner.else_branch.is_terminated() {
                    context.add_diagnostic(
                        super::DiagnosticLevel::Error,
                        "else block is improperly terminated",
                    );
                }
            }
            Lir::InductiveElimination { eliminator, .. } => {
                for elim in eliminator.iter() {
                    if !elim.body.is_terminated() {
                        context.add_diagnostic(
                            super::DiagnosticLevel::Error,
                            "eliminator body is improperly terminated",
                        );
                    }
                }
            }
            _ => {}
        }
        Action::Continue
    }
}

#[cfg(test)]
mod test {
    use crate::{
        parser::parse_module,
        pass::diagnostic::{fmt_diagnostic_messages, DiagnosticPass},
    };

    #[test]
    fn test_no_errors() {
        let mut module = r#"
            module test {
                fn main() -> i32 {
                    %0 = constant 0 : i32;
                    return %0;
                }
            }
        "#;
        let mut pass = super::ImproperTermination;
        let module = parse_module(&mut module).unwrap();
        let messages = pass.run_diagnostic(&module);
        let mut buffer = String::new();
        fmt_diagnostic_messages(&mut buffer, &messages).unwrap();
        println!("{}", buffer);
        assert!(messages.is_empty());
    }

    #[test]
    fn test_errors() {
        let mut module = r#"
            module test {
                fn main() -> i32 {
                    %0 = constant 0 : i32;
                    %1 = constant 1 : i32;
                }
            }
        "#;
        let mut pass = super::ImproperTermination;
        let module = parse_module(&mut module).unwrap();
        let messages = pass.run_diagnostic(&module);
        let mut buffer = String::new();
        fmt_diagnostic_messages(&mut buffer, &messages).unwrap();
        println!("{}", buffer);
        assert!(!messages.is_empty());
    }
}
