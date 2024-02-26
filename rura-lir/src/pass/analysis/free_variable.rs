use std::collections::HashSet;

use crate::pass::visitor::{default_visit_block, LirVisitor, TracingContext, VisitorContext};

struct FreeOperandVisitor {
    definitions: HashSet<usize>,
    free: HashSet<usize>,
    changelog: Vec<Vec<usize>>,
}

impl FreeOperandVisitor {
    pub fn new() -> Self {
        Self {
            definitions: HashSet::new(),
            free: HashSet::new(),
            changelog: Vec::new(),
        }
    }

    fn define_variable(&mut self, var: usize) {
        if self.definitions.insert(var) {
            match self.changelog.last_mut() {
                Some(last) => last.push(var),
                None => self.changelog.push(vec![var]),
            }
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
}
struct FakeContext;

impl TracingContext<'_> for FakeContext {
    fn push_context(&mut self, _context: VisitorContext) {}

    fn pop_context(&mut self) {}
}

impl LirVisitor for FreeOperandVisitor {
    type Context<'a> = FakeContext;

    fn visit_normal_instruction(&mut self, instruction: &crate::lir::Lir, _: &mut FakeContext) {
        instruction
            .using_operands()
            .filter(|x| !self.definitions.contains(x))
            .for_each(|x| {
                self.free.insert(x);
            });
        instruction
            .defining_operand()
            .for_each(|x| self.define_variable(x));
    }

    fn visit_if_then_else(&mut self, inner: &crate::lir::IfThenElse, ctx: &mut FakeContext) {
        if !self.definitions.contains(&inner.condition) {
            self.free.insert(inner.condition);
        }
        self.new_scope();
        self.visit_block(&inner.then_branch, ctx);
        self.pop_scope();
        self.new_scope();
        self.visit_block(&inner.else_branch, ctx);
        self.pop_scope();
    }

    fn visit_eliminator(
        &mut self,
        inductive: usize,
        eliminator: &[crate::lir::InductiveEliminator],
        ctx: &mut FakeContext,
    ) {
        if !self.definitions.contains(&inductive) {
            self.free.insert(inductive);
        }
        for elim in eliminator.iter() {
            self.new_scope();
            match &elim.style {
                crate::lir::EliminationStyle::Unwrap { fields, token } => {
                    self.define_variable(*token);
                    for (_, i) in fields.iter() {
                        self.define_variable(*i);
                    }
                }
                crate::lir::EliminationStyle::Mutation(recv) => {
                    for i in recv.iter() {
                        self.define_variable(i.value);
                        self.define_variable(i.hole);
                    }
                }
                crate::lir::EliminationStyle::Fixpoint(value) => {
                    self.define_variable(*value);
                }
                crate::lir::EliminationStyle::Ref(fields) => {
                    for (_, i) in fields.iter() {
                        self.define_variable(*i);
                    }
                }
            }
            default_visit_block(self, &elim.body, ctx);
            self.pop_scope();
        }
    }

    fn visit_closure(&mut self, inner: &crate::lir::ClosureCreation, agent: &mut FakeContext) {
        self.new_scope();
        for i in inner.params.iter().map(|x| x.0) {
            self.define_variable(i);
        }
        self.visit_block(&inner.body, agent);
        self.pop_scope();
        self.define_variable(inner.result);
    }
}

pub fn get_free_variable(closure: &crate::lir::ClosureCreation) -> HashSet<usize> {
    let mut visitor = FreeOperandVisitor::new();
    visitor.visit_closure(closure, &mut FakeContext);
    visitor.free
}

#[cfg(test)]
mod test {
    use crate::{lir::Lir, parser::parse_module};

    use super::get_free_variable;

    #[test]
    fn test_free_variable_analysis() {
        let mut input = r#"
            module test {
                fn test() -> fn (i32, i32) -> i32 {
                    %0 = constant 991208 : i32;
                    %1 = (%2 : i32, %3 : i32) -> i32 {
                        %4 = %2 + %3;
                        %5 = %4 + %0;
                        return %5;
                    };
                    return %1;
                }
            }
        "#;
        let module = parse_module(&mut input).unwrap();
        let Lir::Closure(clsoure) = &module.functions[0].body.0[1] else {
            panic!("Expected a closure");
        };
        let free = get_free_variable(clsoure);
        assert_eq!(free, [0].iter().cloned().collect());
    }
}
