use std::collections::{HashMap, HashSet};

use crate::pass::visitor::{LirVisitor, TracingContext, VisitorContext};
use crate::pprint::PrettyPrint;
use crate::{Ident, QualifiedName};

use crate::types::{LirType, TypeVar};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct TypeInference {
    types: HashMap<QualifiedName, HashMap<usize, LirType>>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("binary operands %{0} : {} and %{2} : {} has inconsistent types", 
            PrettyPrint::new(&**.1), PrettyPrint::new(&**.3))]
    InconsistentBinaryOperands(usize, Box<LirType>, usize, Box<LirType>),
    #[error("the type of operand %{0} is unknown when being used at this site")]
    UnknownOperandType(usize),
    #[error("argument %{0} has invalid type {} for a rura function", PrettyPrint::new(&**.1))]
    InvalidArgumentType(usize, Box<LirType>),
    #[error("closure %{0} captures an operand %{1} of an imcompatible type {}", PrettyPrint::new(&**.2))]
    ImcompatibleCapture(usize, usize, Box<LirType>),
    #[error("operand %{0} contains unknown type variable in its type {}", PrettyPrint::new(&**.1))]
    UnknownTypeVar(usize, Box<LirType>),
}

pub struct TypeInferenceContext<'a> {
    type_variables: HashSet<&'a Ident>,
    context: Vec<VisitorContext<'a>>,
    errors: Vec<(VisitorContext<'a>, Error)>,
}

impl<'a> TypeInferenceContext<'a> {
    pub fn inconsistent_binary_operands(
        &mut self,
        lhs: usize,
        rhs: usize,
        lhs_type: &LirType,
        rhs_type: &LirType,
    ) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InconsistentBinaryOperands(
                lhs,
                Box::new(lhs_type.clone()),
                rhs,
                Box::new(rhs_type.clone()),
            ),
        ));
    }

    pub fn unknown_operand_type(&mut self, operand: usize) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::UnknownOperandType(operand),
        ));
    }

    pub fn invalid_argument_type(&mut self, arg: usize, arg_type: &LirType) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InvalidArgumentType(arg, Box::new(arg_type.clone())),
        ));
    }

    pub fn imcompatible_capture(
        &mut self,
        closure: usize,
        operand: usize,
        operand_type: &'a LirType,
    ) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::ImcompatibleCapture(closure, operand, Box::new(operand_type.clone())),
        ));
    }

    pub fn check_type_var(&mut self, operand: usize, operand_type: &'a LirType) {
        fn recursively_check_inside_typevar(var: &TypeVar, set: &HashSet<&Ident>) -> bool {
            match var {
                TypeVar::Plain(i) => set.contains(i),
                TypeVar::Associated(a, _) => set.contains(a),
                TypeVar::AsExpr(a, _, _) => recursively_check_inside_typevar(a, set),
            }
        }
        fn recursively_check_typevar(ty: &LirType, set: &HashSet<&Ident>) -> bool {
            match ty {
                LirType::Closure(a, b) => {
                    for i in a.iter() {
                        if !recursively_check_typevar(i, set) {
                            return false;
                        }
                    }
                    recursively_check_typevar(b, set)
                }
                LirType::Primitive(_) => true,
                LirType::Unit => true,
                LirType::Bottom => true,
                LirType::Object(_, b) => {
                    for i in b.iter() {
                        if !recursively_check_typevar(i, set) {
                            return false;
                        }
                    }
                    true
                }
                LirType::Tuple(inner) => {
                    for i in inner.iter() {
                        if !recursively_check_typevar(i, set) {
                            return false;
                        }
                    }
                    true
                }
                LirType::TypeVar(a) => match a {
                    TypeVar::Plain(i) => set.contains(i),
                    TypeVar::Associated(a, _) => set.contains(a),
                    TypeVar::AsExpr(a, _, _) => recursively_check_inside_typevar(a, set),
                },
                LirType::Hole(inner) => recursively_check_typevar(inner, set),
                LirType::Ref(inner) => recursively_check_typevar(inner, set),
            }
        }
        if !recursively_check_typevar(operand_type, &self.type_variables) {
            self.errors.push((
                self.context.last().unwrap().clone(),
                Error::UnknownTypeVar(operand, Box::new(operand_type.clone())),
            ));
        }
    }
}

impl<'a> TracingContext<'a> for TypeInferenceContext<'a> {
    fn push_context(&mut self, context: VisitorContext<'a>) {
        self.context.push(context);
    }

    fn pop_context(&mut self) {
        self.context.pop();
    }
}

// impl LirVisitor for TypeInference {
//     type Context<'a> = TypeInferenceContext<'a>;
//     fn visit_function_def<'a>(
//         &mut self,
//         function: &'a crate::lir::FunctionDef,
//         context: &mut TypeInferenceContext<'a>,
//     ) {
//         context.type_variables = function
//             .prototype
//             .type_params
//             .iter()
//             .collect::<HashSet<_>>();
//         self.new_scope();
//         for i in function.prototype.params.iter().map(|x| x.0) {
//             if let Some(err) = self.define_variable(i) {
//                 context.add_diagnostic(super::DiagnosticLevel::Error, err);
//             }
//         }
//         default_visit_block(self, &function.body, context);
//         self.pop_scope();
//     }
//     fn visit_normal_instruction<'a>(
//         &mut self,
//         instruction: &'a crate::lir::Lir,
//         context: &mut super::DiagnosticAgent<'a>,
//     ) {
//         for used in instruction
//             .using_operands()
//             .filter(|x| !self.definitions.contains(x))
//         {
//             context.add_diagnostic(super::DiagnosticLevel::Error, Error::Undefined(used));
//         }
//         for defined in instruction.defining_operand() {
//             if let Some(err) = self.define_variable(defined) {
//                 context.add_diagnostic(super::DiagnosticLevel::Error, err);
//             }
//         }
//     }
//     fn visit_if_then_else<'a>(
//         &mut self,
//         inner: &'a crate::lir::IfThenElse,
//         agent: &mut super::DiagnosticAgent<'a>,
//     ) {
//         if !self.definitions.contains(&inner.condition) {
//             agent.add_diagnostic(
//                 super::DiagnosticLevel::Error,
//                 Error::Undefined(inner.condition),
//             );
//         }
//         agent.push_context(VisitorContext::ThenBlock);
//         self.new_scope();
//         self.visit_block(&inner.then_branch, agent);
//         self.pop_scope();
//         agent.pop_context();
//         agent.push_context(VisitorContext::ElseBlock);
//         self.new_scope();
//         self.visit_block(&inner.else_branch, agent);
//         self.pop_scope();
//         agent.pop_context();
//     }

//     fn visit_eliminator<'a>(
//         &mut self,
//         inductive: usize,
//         eliminator: &'a [crate::lir::InductiveEliminator],
//         agent: &mut super::DiagnosticAgent<'a>,
//     ) {
//         if !self.definitions.contains(&inductive) {
//             agent.add_diagnostic(super::DiagnosticLevel::Error, Error::Undefined(inductive));
//         }
//         macro_rules! check_multiple_definitions {
//             ($x:expr) => {
//                 if let Some(err) = self.define_variable($x) {
//                     agent.add_diagnostic(super::DiagnosticLevel::Error, err);
//                 }
//             };
//         }
//         for elim in eliminator.iter() {
//             agent.push_context(VisitorContext::Eliminator(&elim.ctor));
//             self.new_scope();
//             match &elim.style {
//                 crate::lir::EliminationStyle::Unwrap { fields, token } => {
//                     check_multiple_definitions!(*token);
//                     for (_, i) in fields.iter() {
//                         check_multiple_definitions!(*i);
//                     }
//                 }
//                 crate::lir::EliminationStyle::Mutation(recv) => {
//                     for i in recv.iter() {
//                         check_multiple_definitions!(i.value);
//                         check_multiple_definitions!(i.hole);
//                     }
//                 }
//                 crate::lir::EliminationStyle::Fixpoint(value) => {
//                     check_multiple_definitions!(*value);
//                 }
//                 crate::lir::EliminationStyle::Ref(fields) => {
//                     for (_, i) in fields.iter() {
//                         check_multiple_definitions!(*i);
//                     }
//                 }
//             }
//             default_visit_block(self, &elim.body, agent);
//             self.pop_scope();
//             agent.pop_context();
//         }
//     }

//     fn visit_closure<'a>(
//         &mut self,
//         inner: &'a crate::lir::ClosureCreation,
//         agent: &mut super::DiagnosticAgent<'a>,
//     ) {
//         self.new_scope();
//         for i in inner.params.iter().map(|x| x.0) {
//             if let Some(err) = self.define_variable(i) {
//                 if self.defined_in_current_scope(i) {
//                     agent.add_diagnostic(super::DiagnosticLevel::Error, err);
//                 } else {
//                     agent.add_diagnostic(super::DiagnosticLevel::Error, Error::Shadowed(i));
//                 }
//             }
//         }
//         agent.push_context(VisitorContext::Closure);
//         self.visit_block(&inner.body, agent);
//         agent.pop_context();
//         self.pop_scope();
//         if let Some(err) = self.define_variable(inner.result) {
//             agent.add_diagnostic(super::DiagnosticLevel::Error, err);
//         }
//     }
// }
