use std::{
    collections::HashSet,
    fmt::{Display, Formatter},
};

use crate::pass::Pass;

use super::{default_visit_block, DiagnosticContext, DiagnosticPass};

#[derive(Default)]
pub struct VariableDefinition {
    ever_defined: HashSet<usize>,
    definitions: HashSet<usize>,
    changelog: Vec<Vec<usize>>,
}

enum Error {
    Shadowed(usize),
    Undefined(usize),
    MultipleDefinitions(usize),
    NonUnique(usize),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Shadowed(var) => write!(
                f,
                "parameter %{var} shadows the definition outside the capture"
            ),
            Self::Undefined(var) => write!(f, "%{var} appears to be undefined within the scope"),
            Self::MultipleDefinitions(var) => {
                write!(f, "multiple definitions encountered for %{var}")
            }
            Self::NonUnique(var) => {
                write!(
                    f,
                    "operand %{var} is not uniquely defined within a same function"
                )
            }
        }
    }
}

impl Pass for VariableDefinition {
    fn get_identifier(&self) -> &str {
        "variable_definition"
    }
}

impl VariableDefinition {
    pub fn new() -> Self {
        Self {
            ever_defined: HashSet::new(),
            definitions: HashSet::new(),
            changelog: Vec::new(),
        }
    }

    fn define_variable(&mut self, var: usize) -> Option<Error> {
        if self.definitions.insert(var) {
            match self.changelog.last_mut() {
                Some(last) => last.push(var),
                None => self.changelog.push(vec![var]),
            }
            if !self.ever_defined.insert(var) {
                Some(Error::NonUnique(var))
            } else {
                None
            }
        } else {
            Some(Error::MultipleDefinitions(var))
        }
    }

    pub fn new_scope(&mut self) {
        self.changelog.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        if let Some(last) = self.changelog.pop() {
            for var in last {
                self.definitions.remove(&var);
            }
        }
    }

    pub fn defined_in_current_scope(&self, var: usize) -> bool {
        self.changelog.last().map_or(false, |x| x.contains(&var))
    }
}

impl DiagnosticPass for VariableDefinition {
    fn visit_function_def<'a>(
        &mut self,
        function: &'a crate::lir::FunctionDef,
        context: &mut super::DiagnosticAgent<'a>,
    ) {
        self.ever_defined.clear();
        self.new_scope();
        for i in function.prototype.params.iter().map(|x| x.0) {
            if let Some(err) = self.define_variable(i) {
                context.add_diagnostic(super::DiagnosticLevel::Error, err);
            }
        }
        default_visit_block(self, &function.body, context);
        self.pop_scope();
    }
    fn visit_normal_instruction<'a>(
        &mut self,
        instruction: &'a crate::lir::Lir,
        context: &mut super::DiagnosticAgent<'a>,
    ) {
        for used in instruction
            .using_operands()
            .filter(|x| !self.definitions.contains(x))
        {
            context.add_diagnostic(super::DiagnosticLevel::Error, Error::Undefined(used));
        }
        for defined in instruction.defining_operand() {
            if let Some(err) = self.define_variable(defined) {
                context.add_diagnostic(super::DiagnosticLevel::Error, err);
            }
        }
    }
    fn visit_if_then_else<'a>(
        &mut self,
        inner: &'a crate::lir::IfThenElse,
        agent: &mut super::DiagnosticAgent<'a>,
    ) {
        if !self.definitions.contains(&inner.condition) {
            agent.add_diagnostic(
                super::DiagnosticLevel::Error,
                Error::Undefined(inner.condition),
            );
        }
        agent.push_context(DiagnosticContext::ThenBlock);
        self.new_scope();
        self.visit_block(&inner.then_branch, agent);
        self.pop_scope();
        agent.pop_context();
        agent.push_context(DiagnosticContext::ElseBlock);
        self.new_scope();
        self.visit_block(&inner.else_branch, agent);
        self.pop_scope();
        agent.pop_context();
    }

    fn visit_eliminator<'a>(
        &mut self,
        inductive: usize,
        eliminator: &'a [crate::lir::InductiveEliminator],
        agent: &mut super::DiagnosticAgent<'a>,
    ) {
        if !self.definitions.contains(&inductive) {
            agent.add_diagnostic(super::DiagnosticLevel::Error, Error::Undefined(inductive));
        }
        macro_rules! check_multiple_definitions {
            ($x:expr) => {
                if let Some(err) = self.define_variable($x) {
                    agent.add_diagnostic(super::DiagnosticLevel::Error, err);
                }
            };
        }
        for elim in eliminator.iter() {
            agent.push_context(DiagnosticContext::Eliminator(&elim.ctor));
            self.new_scope();
            match &elim.style {
                crate::lir::EliminationStyle::Unwrap { fields, token } => {
                    check_multiple_definitions!(*token);
                    for (_, i) in fields.iter() {
                        check_multiple_definitions!(*i);
                    }
                }
                crate::lir::EliminationStyle::Mutation(recv) => {
                    for i in recv.iter() {
                        check_multiple_definitions!(i.value);
                        check_multiple_definitions!(i.hole);
                    }
                }
                crate::lir::EliminationStyle::Fixpoint(value) => {
                    check_multiple_definitions!(*value);
                }
                crate::lir::EliminationStyle::Ref(fields) => {
                    for (_, i) in fields.iter() {
                        check_multiple_definitions!(*i);
                    }
                }
            }
            default_visit_block(self, &elim.body, agent);
            self.pop_scope();
            agent.pop_context();
        }
    }

    fn visit_closure<'a>(
        &mut self,
        inner: &'a crate::lir::ClosureCreation,
        agent: &mut super::DiagnosticAgent<'a>,
    ) {
        self.new_scope();
        for i in inner.params.iter().map(|x| x.0) {
            if let Some(err) = self.define_variable(i) {
                if self.defined_in_current_scope(i) {
                    agent.add_diagnostic(super::DiagnosticLevel::Error, err);
                } else {
                    agent.add_diagnostic(super::DiagnosticLevel::Error, Error::Shadowed(i));
                }
            }
        }
        agent.push_context(DiagnosticContext::Closure);
        self.visit_block(&inner.body, agent);
        agent.pop_context();
        self.pop_scope();
        if let Some(err) = self.define_variable(inner.result) {
            agent.add_diagnostic(super::DiagnosticLevel::Error, err);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::pass::diagnostic::{fmt_diagnostic_messages, DiagnosticPass};

    macro_rules! test_variable_defs {
        (input = $input:literal, error_cnt = $error_cnt:expr) => {
            let mut module = $input;
            let mut pass = super::VariableDefinition::new();
            let module = crate::parser::parse_module(&mut module).unwrap();
            let messages = pass.run_diagnostic(&module);
            let mut buffer = String::new();
            fmt_diagnostic_messages(&mut buffer, &messages).unwrap();
            println!("{}", buffer);
            assert_eq!(messages.len(), $error_cnt);
        };
    }
    #[test]
    fn test_duplicated_function_parameter() {
        test_variable_defs! {
          input = r#"
          module test {
            fn main(%0: i32, %0: i32) -> i32 {
              %1 = constant 0 : i32;
              return %1;
            }
          }
        "#,
          error_cnt = 1
        }
    }
    #[test]
    fn test_definition_cross_if_then_else() {
        test_variable_defs! {
          input = r#"
          module test {
            fn main(%0: i32) -> i32 {
              if %0 {
                %1 = constant 1 : i32;
                return %1;
              } else {
                %1 = constant 2 : i32;
                return %1;
              }
            }
          }
        "#,
          error_cnt = 1
        }
    }
    #[test]
    fn test_definition_refering_another_scope() {
        test_variable_defs! {
          input = r#"
          module test {
            fn main(%0: i32) -> i32 {
              %1 = constant 1 : i32;
              if %0 {
                %2 = constant 2 : i32;
                return %1;
              } else {
                return %2;
              }
            }
          }
        "#,
          error_cnt = 1
        }
    }
    #[test]
    fn test_closure_shadowing() {
        test_variable_defs! {
          input = r#"
          module test {
            fn main(%0: i32) -> i32 {
              %1 = constant 1 : i32;
              %2 = (%0 : usize) -> usize {
                return %0;
              };
              return %2;
            }
          }
        "#,
          error_cnt = 1
        }
    }
    #[test]
    fn test_eliminator_definitions() {
        test_variable_defs! {
          input = r#"
          module test {
            fn foo<T>(%0 : List<@T>) -> @T {
              match %0 {
                [fixpoint(%1)] List::Nil => {
                  unreachable [panic];
                }
                [unwrap(%1)] List::Cons(%2, %3) => {
                  drop %3;
                  drop %1;
                  drop %4;
                  return %2;
                }
              }
            }
          }
      "#,
          error_cnt = 2
        }
    }
}
