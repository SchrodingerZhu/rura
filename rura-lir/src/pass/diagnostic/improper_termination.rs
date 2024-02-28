use crate::pass::{
    visitor::{default_visit_block, LirVisitor},
    BoxedPass, Pass,
};
use rura_core::lir::ir::{Block, Module};

use super::{default_diagnose_module, DiagnosticPass};

pub struct ImproperTermination;

impl Pass for ImproperTermination {
    fn get_identifier(&self) -> &str {
        "improper_termination"
    }
}

impl LirVisitor for ImproperTermination {
    type Context<'a> = super::DiagnosticAgent<'a>;
    fn visit_block<'a>(&mut self, inner: &'a Block, agent: &mut super::DiagnosticAgent<'a>) {
        if !inner.is_terminated() {
            agent.add_diagnostic(
                super::DiagnosticLevel::Error,
                "block is not properly terminated",
            );
        }
        default_visit_block(self, inner, agent);
    }
}

impl<'a> DiagnosticPass<'a> for ImproperTermination {
    fn diagnose(&mut self, module: &'a rura_core::lir::ir::Module) -> Box<[super::Diagnostic<'a>]> {
        default_diagnose_module(self, module)
    }
}

pub fn get_pass<'a>(_: &'a Module, _: &'_ toml::Table) -> BoxedPass<'a> {
    BoxedPass::Diagnostic(Box::new(ImproperTermination))
}

#[cfg(test)]
mod test {
    use winnow::Located;

    use crate::{
        parser::parse_module,
        pass::diagnostic::{fmt_diagnostic_messages, DiagnosticPass},
    };

    #[test]
    fn test_no_errors() {
        let mut module = Located::new(
            r#"
            module test {
                fn main() -> i32 {
                    %0 = constant 0 : i32;
                    return %0;
                }
            }
        "#,
        );
        let mut pass = super::ImproperTermination;
        let module = parse_module(&mut module).unwrap();
        let messages = pass.diagnose(&module);
        let mut buffer = String::new();
        fmt_diagnostic_messages(&mut buffer, &messages).unwrap();
        println!("{}", buffer);
        assert!(messages.is_empty());
    }

    #[test]
    fn test_errors() {
        let mut module = Located::new(
            r#"
            module test {
                fn main() -> i32 {
                    %0 = constant 0 : i32;
                    %1 = constant 1 : i32;
                    %2 = (%3 : i32) -> i32 {
                        %4 = %0 + %1;
                        %5 = %4 + %3;
                    };
                }
            }
        "#,
        );
        let mut pass = super::ImproperTermination;
        let module = parse_module(&mut module).unwrap();
        let messages = pass.diagnose(&module);
        let mut buffer = String::new();
        fmt_diagnostic_messages(&mut buffer, &messages).unwrap();
        println!("{}", buffer);
        assert!(!messages.is_empty());
    }
}
