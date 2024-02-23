use crate::pass::Pass;

use super::{default_visit_block, DiagnosticPass};

pub struct ImproperTermination;

impl Pass for ImproperTermination {
    fn get_identifier(&self) -> &str {
        "improper_termination"
    }
}

impl DiagnosticPass for ImproperTermination {
    fn visit_block<'a>(
        &mut self,
        inner: &'a crate::lir::Block,
        agent: &mut super::DiagnosticAgent<'a>,
    ) {
        if !inner.is_terminated() {
            agent.add_diagnostic(
                super::DiagnosticLevel::Error,
                "block is not properly terminated",
            );
        }
        default_visit_block(self, inner, agent);
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
                    %2 = (%0 : i32) -> i32 {
                        %3 = %0 + %1;
                    };
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
