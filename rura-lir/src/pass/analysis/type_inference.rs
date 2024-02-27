use rura_core::{Constant, PrimitiveType};
use std::collections::{HashMap, HashSet};

use rura_core::lir::ir::{BinaryOp, FunctionDef, Lir};
use rura_core::lir::types::{LirType, TypeVar};
use rura_core::lir::{Ident, QualifiedName};

use crate::pass::visitor::default_visit_function_def;
use crate::pass::visitor::{LirVisitor, TracingContext, VisitorContext};
use crate::pprint::PrettyPrint;

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
    #[error("{} is not a valid type to appear at Rura's function interface", PrettyPrint::new(&**.0))]
    InvalidInterfaceType(Box<LirType>),
    #[error("closure %{0} captures an operand %{1} of an incompatible type {}", PrettyPrint::new(&**.2))]
    IncompatibleCapture(usize, usize, Box<LirType>),
    #[error("operand %{0} contains unknown type variable in its type {}", PrettyPrint::new(&**.1))]
    UnknownTypeVar(usize, Box<LirType>),
    #[error("{} contains unknown type variable(s) in function interface", PrettyPrint::new(&**.0))]
    UnknownTypeVarInInterface(Box<LirType>),
    #[error("%{0} : {} is not of the required type for function call", PrettyPrint::new(&**.1))]
    InvalidFunctionArgument(usize, Box<LirType>),
    #[error("%{0} : {} is not a permissible operand for this instruction", PrettyPrint::new(&**.1))]
    InvalidOperand(usize, Box<LirType>),
    #[error("return value %{0} : {} does not match function signature", PrettyPrint::new(&**.1))]
    InvalidReturnValue(usize, Box<LirType>),
    #[error("target type {} is not pemissible in casting", PrettyPrint::new(&**.0))]
    InvalidCastTarget(Box<LirType>),
}

pub struct TypeInferenceContext<'a> {
    return_type: &'a LirType,
    type_variables: HashSet<&'a Ident>,
    context: Vec<VisitorContext<'a>>,
    errors: Vec<(VisitorContext<'a>, Error)>,
    current_function: HashMap<usize, LirType>,
}

impl<'a> TypeInferenceContext<'a> {
    pub fn new() -> Self {
        const UNIT_TYPE: LirType = LirType::Unit;
        Self {
            return_type: &UNIT_TYPE,
            type_variables: HashSet::new(),
            context: vec![],
            errors: vec![],
            current_function: HashMap::new(),
        }
    }
    pub fn set_type(&mut self, operand: usize, ty: LirType) {
        self.current_function.insert(operand, ty);
    }
    pub fn inconsistent_binary_operands(
        &mut self,
        lhs: usize,
        rhs: usize,
        lhs_type: LirType,
        rhs_type: LirType,
    ) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InconsistentBinaryOperands(lhs, Box::new(lhs_type), rhs, Box::new(rhs_type)),
        ));
    }

    pub fn unknown_operand_type(&mut self, operand: usize) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::UnknownOperandType(operand),
        ));
    }

    pub fn invalid_interface_type(&mut self, arg_type: LirType) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InvalidInterfaceType(Box::new(arg_type)),
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
            Error::IncompatibleCapture(closure, operand, Box::new(operand_type.clone())),
        ));
    }

    pub fn invalid_return_value(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InvalidReturnValue(operand, Box::new(operand_type)),
        ));
    }

    pub fn invalid_cast_target(&mut self, target: LirType) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InvalidCastTarget(Box::new(target)),
        ));
    }

    fn check_type_var<F>(&mut self, error_maker: F, operand_type: &'a LirType)
    where
        F: FnOnce(Box<LirType>) -> Error,
    {
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
                LirType::Token(inner) => recursively_check_typevar(inner, set),
                LirType::Unique(inner) => recursively_check_typevar(inner, set),
            }
        }
        if !recursively_check_typevar(operand_type, &self.type_variables) {
            self.errors.push((
                self.context.last().unwrap().clone(),
                error_maker(Box::new(operand_type.clone())),
            ));
        }
    }

    fn check_type_var_in_interface(&mut self, arg_type: &'a LirType) {
        self.check_type_var(Error::UnknownTypeVarInInterface, arg_type);
    }

    fn check_type_var_for_operand(&mut self, operand: usize, operand_type: &'a LirType) {
        self.check_type_var(|x| Error::UnknownTypeVar(operand, x), operand_type);
    }

    fn invalid_function_argument(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InvalidFunctionArgument(operand, Box::new(operand_type)),
        ));
    }
    fn invalid_operand(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.last().unwrap().clone(),
            Error::InvalidOperand(operand, Box::new(operand_type)),
        ));
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

macro_rules! get_type {
    ($context:ident, $operand:expr) => {
        if let Some(ty) = $context.current_function.get(&$operand) {
            ty
        } else {
            $context.unknown_operand_type($operand);
            return;
        }
    };
}

impl LirVisitor for TypeInference {
    type Context<'a> = TypeInferenceContext<'a>;
    fn visit_function_def<'a>(
        &mut self,
        function: &'a FunctionDef,
        context: &mut TypeInferenceContext<'a>,
    ) {
        context.return_type = &function.prototype.return_type;
        // insert all type variables
        context.type_variables = function.prototype.type_params.iter().collect();
        context.check_type_var_in_interface(&function.prototype.return_type);
        if !function.prototype.return_type.is_interface_compat() {
            context.invalid_interface_type(function.prototype.return_type.clone());
        }

        // insert all arguments as known type
        for (op, ty) in function.prototype.params.iter() {
            context.check_type_var_for_operand(*op, ty);
            if !ty.is_interface_compat() {
                context.invalid_interface_type(ty.clone());
            }
            context.current_function.insert(*op, ty.clone());
        }
        // continue to visit the body
        default_visit_function_def(self, function, context);
        // submit local result to global
        let current_function = std::mem::take(&mut context.current_function);
        self.types
            .insert(function.prototype.name.clone(), current_function);
    }
    fn visit_normal_instruction<'a>(
        &mut self,
        instruction: &'a Lir,
        context: &mut TypeInferenceContext<'a>,
    ) {
        match instruction {
            Lir::Apply {
                closure,
                arg,
                result,
            } => {
                let closure_type = get_type!(context, *closure);
                let LirType::Closure(params, ret) = closure_type else {
                    context.invalid_operand(*closure, closure_type.clone());
                    return;
                };
                let arg_type = get_type!(context, *arg);
                let expect_type = params.first().unwrap_or(&LirType::Unit);
                if arg_type != expect_type {
                    context.invalid_function_argument(*arg, arg_type.clone());
                    return;
                }
                if params.len() > 1 {
                    // not fully applied
                    let params = params.iter().skip(1).cloned().collect();
                    let ty = LirType::Closure(params, ret.clone());
                    context.current_function.insert(*result, ty);
                } else {
                    context.set_type(*result, ret.as_ref().clone());
                }
            }
            Lir::BinaryOp(op) => {
                let lhs_type = get_type!(context, op.lhs);
                let rhs_type = get_type!(context, op.rhs);
                if lhs_type != rhs_type {
                    context.inconsistent_binary_operands(
                        op.lhs,
                        op.rhs,
                        lhs_type.clone(),
                        rhs_type.clone(),
                    );
                    return;
                }
                if op.is_arithmetic() || op.is_bitwise() {
                    context.set_type(op.result, lhs_type.clone());
                } else {
                    context.set_type(op.result, LirType::Primitive(PrimitiveType::Bool));
                }
            }
            Lir::Call(_) => todo!("figure out how to look up function type"),
            Lir::Clone { value, result } => {
                let ty = get_type!(context, *value);
                context.set_type(*result, ty.clone());
            }
            Lir::Closure(_) => unreachable!(),
            Lir::Drop { value, token } => todo!(),
            Lir::CtorCall(_) => todo!(),
            Lir::InductiveElimination { .. } => unreachable!(),
            Lir::Return { value } => todo!(),
            Lir::TupleIntro { elements, result } => todo!(),
            Lir::TupleElim { tuple, eliminator } => todo!(),
            Lir::UnaryOp(x) => {
                let ty = get_type!(context, x.operand);
                if matches!(x.op, rura_core::UnOp::Neg) && !ty.is_numeric() {
                    context.invalid_operand(x.operand, ty.clone());
                } else if matches!(x.op, rura_core::UnOp::Not) && !ty.is_boolean() {
                    context.invalid_operand(x.operand, ty.clone());
                } else {
                    context.set_type(x.result, ty.clone());
                }
            }
            Lir::IfThenElse(_) => unreachable!(),
            Lir::Constant { value, result } => {
                let ty = match **value {
                    Constant::I8(_) => PrimitiveType::I8,
                    Constant::I16(_) => PrimitiveType::I16,
                    Constant::I32(_) => PrimitiveType::I32,
                    Constant::I64(_) => PrimitiveType::I64,
                    Constant::ISize(_) => PrimitiveType::ISize,
                    Constant::I128(_) => PrimitiveType::I128,
                    Constant::U8(_) => PrimitiveType::U8,
                    Constant::U16(_) => PrimitiveType::U16,
                    Constant::U32(_) => PrimitiveType::U32,
                    Constant::U64(_) => PrimitiveType::U64,
                    Constant::USize(_) => PrimitiveType::USize,
                    Constant::U128(_) => PrimitiveType::U128,
                    Constant::F32(_) => PrimitiveType::F32,
                    Constant::F64(_) => PrimitiveType::F64,
                    Constant::Bool(_) => PrimitiveType::Bool,
                    Constant::Char(_) => PrimitiveType::Char,
                    Constant::Literal(_) => PrimitiveType::Str,
                    Constant::Unit => {
                        context.set_type(*result, LirType::Unit);
                        return;
                    }
                };
                context.set_type(*result, LirType::Primitive(ty));
            }
            Lir::Fill { hole, value } => {
                let hole_type = get_type!(context, *hole);
                let value_type = get_type!(context, *value);
                let LirType::Hole(inner) = hole_type else {
                    context.invalid_operand(*hole, hole_type.clone());
                    return;
                };
                if **inner != *value_type {
                    context.invalid_operand(*value, value_type.clone());
                }
            }
            Lir::Curry { function, result } => {
                todo!("implement after function prototype lookup is ready")
            }
            Lir::Unreachable { panic } => {}
            Lir::RcToUnique { value, result } => {
                let ty = get_type!(context, *value);
                if !ty.is_object() {
                    context.invalid_operand(*value, ty.clone());
                } else {
                    context.set_type(*result, LirType::Unique(Box::new(ty.clone())));
                }
            }
            Lir::UniqueToRc { value, result } => {
                let ty = get_type!(context, *value);
                match ty {
                    LirType::Unique(inner) => {
                        context.set_type(*result, inner.as_ref().clone());
                    }
                    _ => {
                        context.invalid_operand(*value, ty.clone());
                    }
                }
            }
            Lir::Cast {
                value,
                result,
                target,
            } => {
                if !target.is_numeric() {
                    context.invalid_cast_target(target.as_ref().clone());
                }
                let vt = get_type!(context, *value);
                if !vt.is_numeric() {
                    context.invalid_operand(*value, vt.clone());
                } else {
                    context.set_type(*result, vt.clone());
                }
            }
        }
    }
    // fn visit_if_then_else<'a>(
    //     &mut self,
    //     inner: &'a crate::lir::IfThenElse,
    //     agent: &mut super::DiagnosticAgent<'a>,
    // ) {
    //     if !self.definitions.contains(&inner.condition) {
    //         agent.add_diagnostic(
    //             super::DiagnosticLevel::Error,
    //             Error::Undefined(inner.condition),
    //         );
    //     }
    //     agent.push_context(VisitorContext::ThenBlock);
    //     self.new_scope();
    //     self.visit_block(&inner.then_branch, agent);
    //     self.pop_scope();
    //     agent.pop_context();
    //     agent.push_context(VisitorContext::ElseBlock);
    //     self.new_scope();
    //     self.visit_block(&inner.else_branch, agent);
    //     self.pop_scope();
    //     agent.pop_context();
    // }

    // fn visit_eliminator<'a>(
    //     &mut self,
    //     inductive: usize,
    //     eliminator: &'a [crate::lir::InductiveEliminator],
    //     agent: &mut super::DiagnosticAgent<'a>,
    // ) {
    //     if !self.definitions.contains(&inductive) {
    //         agent.add_diagnostic(super::DiagnosticLevel::Error, Error::Undefined(inductive));
    //     }
    //     macro_rules! check_multiple_definitions {
    //         ($x:expr) => {
    //             if let Some(err) = self.define_variable($x) {
    //                 agent.add_diagnostic(super::DiagnosticLevel::Error, err);
    //             }
    //         };
    //     }
    //     for elim in eliminator.iter() {
    //         agent.push_context(VisitorContext::Eliminator(&elim.ctor));
    //         self.new_scope();
    //         match &elim.style {
    //             crate::lir::EliminationStyle::Unwrap { fields, token } => {
    //                 check_multiple_definitions!(*token);
    //                 for (_, i) in fields.iter() {
    //                     check_multiple_definitions!(*i);
    //                 }
    //             }
    //             crate::lir::EliminationStyle::Mutation(recv) => {
    //                 for i in recv.iter() {
    //                     check_multiple_definitions!(i.value);
    //                     check_multiple_definitions!(i.hole);
    //                 }
    //             }
    //             crate::lir::EliminationStyle::Fixpoint(value) => {
    //                 check_multiple_definitions!(*value);
    //             }
    //             crate::lir::EliminationStyle::Ref(fields) => {
    //                 for (_, i) in fields.iter() {
    //                     check_multiple_definitions!(*i);
    //                 }
    //             }
    //         }
    //         default_visit_block(self, &elim.body, agent);
    //         self.pop_scope();
    //         agent.pop_context();
    //     }
    // }

    // fn visit_closure<'a>(
    //     &mut self,
    //     inner: &'a crate::lir::ClosureCreation,
    //     agent: &mut super::DiagnosticAgent<'a>,
    // ) {
    //     self.new_scope();
    //     for i in inner.params.iter().map(|x| x.0) {
    //         if let Some(err) = self.define_variable(i) {
    //             if self.defined_in_current_scope(i) {
    //                 agent.add_diagnostic(super::DiagnosticLevel::Error, err);
    //             } else {
    //                 agent.add_diagnostic(super::DiagnosticLevel::Error, Error::Shadowed(i));
    //             }
    //         }
    //     }
    //     agent.push_context(VisitorContext::Closure);
    //     self.visit_block(&inner.body, agent);
    //     agent.pop_context();
    //     self.pop_scope();
    //     if let Some(err) = self.define_variable(inner.result) {
    //         agent.add_diagnostic(super::DiagnosticLevel::Error, err);
    //     }
    // }
}
