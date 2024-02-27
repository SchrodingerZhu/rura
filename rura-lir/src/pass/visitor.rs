use rura_core::lir::ir::{
    Block, ClosureCreation, FunctionDef, FunctionPrototype, IfThenElse, InductiveEliminator,
    InductiveTypeDef, Lir, Module,
};
use rura_core::lir::QualifiedName;

#[derive(Debug, Clone)]
pub enum VisitorContext<'a> {
    Module(&'a QualifiedName),
    Function(&'a QualifiedName),
    Instruction(usize),
    Closure,
    ThenBlock,
    ElseBlock,
    Eliminator(&'a QualifiedName),
    External(&'a QualifiedName),
    InductiveTypeDef(&'a QualifiedName),
}

impl std::fmt::Display for VisitorContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VisitorContext::Module(name) => write!(f, "module {}", name),
            VisitorContext::Function(name) => write!(f, "function {}", name),
            VisitorContext::Instruction(i) => write!(f, "instruction at {}", i),
            VisitorContext::Closure => write!(f, "closure body"),
            VisitorContext::ThenBlock => write!(f, "then block"),
            VisitorContext::ElseBlock => write!(f, "else block"),
            VisitorContext::Eliminator(ctor) => {
                write!(f, "elimination body for {ctor}")
            }
            VisitorContext::External(name) => write!(f, "external {}", name),
            VisitorContext::InductiveTypeDef(name) => write!(f, "inductive type {}", name),
        }
    }
}

pub trait TracingContext<'a> {
    fn push_context(&mut self, context: VisitorContext<'a>);
    fn pop_context(&mut self);
}

pub fn default_visit_function_def<'a, S: LirVisitor + ?Sized>(
    this: &mut S,
    function: &'a FunctionDef,
    context: &mut S::Context<'a>,
) {
    this.visit_block(&function.body, context);
}

pub fn default_visit_block<'a, S: LirVisitor + ?Sized>(
    this: &mut S,
    function: &'a Block,
    context: &mut S::Context<'a>,
) {
    for (i, instr) in function.0.iter().enumerate() {
        context.push_context(VisitorContext::Instruction(i));
        match instr {
            Lir::IfThenElse(inner) => this.visit_if_then_else(inner, context),
            Lir::Closure(inner) => this.visit_closure(inner, context),
            Lir::InductiveElimination {
                eliminator,
                inductive,
            } => this.visit_eliminator(*inductive, eliminator, context),
            _ => this.visit_normal_instruction(instr, context),
        }
        context.pop_context();
    }
}

pub fn default_visit_if_then_else<'a, S: LirVisitor + ?Sized>(
    this: &mut S,
    inner: &'a IfThenElse,
    context: &mut S::Context<'a>,
) {
    context.push_context(VisitorContext::ThenBlock);
    this.visit_block(&inner.then_branch, context);
    context.pop_context();
    context.push_context(VisitorContext::ElseBlock);
    this.visit_block(&inner.else_branch, context);
    context.pop_context();
}

pub fn default_visit_closure<'a, S: LirVisitor + ?Sized>(
    this: &mut S,
    inner: &'a ClosureCreation,
    context: &mut S::Context<'a>,
) {
    context.push_context(VisitorContext::Closure);
    this.visit_block(&inner.body, context);
    context.pop_context();
}

pub fn default_visit_eliminator<'a, S: LirVisitor + ?Sized>(
    this: &mut S,
    _inductive: usize,
    eliminator: &'a [InductiveEliminator],
    context: &mut S::Context<'a>,
) {
    for elim in eliminator.iter() {
        context.push_context(VisitorContext::Eliminator(&elim.ctor));
        this.visit_block(&elim.body, context);
        context.pop_context();
    }
}

pub fn default_visit_module<'a, S: LirVisitor + ?Sized>(
    this: &mut S,
    module: &'a Module,
    context: &mut S::Context<'a>,
) {
    context.push_context(VisitorContext::Module(&module.name));

    for def in module.inductive_types.iter() {
        this.visit_inductive_typedef(def, context);
    }

    for external in module.external_functions.iter() {
        this.visit_external(external, context);
    }

    for function in module.functions.iter() {
        context.push_context(VisitorContext::Function(&function.prototype.name));
        this.visit_function_def(function, context);
        context.pop_context();
    }
}

pub trait LirVisitor {
    type Context<'a>: TracingContext<'a>;

    fn visit_if_then_else<'a>(&mut self, inner: &'a IfThenElse, context: &mut Self::Context<'a>) {
        default_visit_if_then_else(self, inner, context);
    }

    fn visit_closure<'a>(&mut self, inner: &'a ClosureCreation, context: &mut Self::Context<'a>) {
        default_visit_closure(self, inner, context);
    }

    fn visit_eliminator<'a>(
        &mut self,
        inductive: usize,
        eliminator: &'a [InductiveEliminator],
        context: &mut Self::Context<'a>,
    ) {
        default_visit_eliminator(self, inductive, eliminator, context);
    }

    fn visit_block<'a>(&mut self, inner: &'a Block, context: &mut Self::Context<'a>) {
        default_visit_block(self, inner, context);
    }

    #[allow(unused_variables)]
    fn visit_module<'a>(&mut self, module: &'a Module, context: &mut Self::Context<'a>) {
        default_visit_module(self, module, context);
    }
    #[allow(unused_variables)]
    fn visit_external<'a>(
        &mut self,
        external: &'a FunctionPrototype,
        context: &mut Self::Context<'a>,
    ) {
    }
    #[allow(unused_variables)]
    fn visit_inductive_typedef<'a>(
        &mut self,
        typedef: &'a InductiveTypeDef,
        context: &mut Self::Context<'a>,
    ) {
    }
    #[allow(unused_variables)]
    fn visit_function_def<'a>(
        &mut self,
        function: &'a FunctionDef,
        context: &mut Self::Context<'a>,
    ) {
        default_visit_function_def(self, function, context);
    }
    #[allow(unused_variables)]
    fn visit_normal_instruction<'a>(
        &mut self,
        instruction: &'a Lir,
        context: &mut Self::Context<'a>,
    ) {
    }
}
