use rura_core::{Constant, PrimitiveType};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use rura_core::lir::ir::{
    BinaryOp, FunctionDef, FunctionPrototype, InductiveTypeDef, LExpr, Lir, Module,
};
use rura_core::lir::types::{subst, unified_compare, LirType, TypeVar};
use rura_core::lir::{Ident, QualifiedName};

use crate::pass::visitor::default_visit_function_def;
use crate::pass::visitor::{LirVisitor, TracingContext, VisitorContext};
use crate::pass::{BoxedPass, Pass};
use crate::pprint::PrettyPrint;

use super::free_variable::get_free_variable;
use super::AnalysisPass;

#[derive(Debug, Clone)]
struct TypeInference<'a> {
    types: HashMap<QualifiedName, HashMap<usize, LirType>>,
    dictionary: Dictionary<'a>,
}

#[derive(Debug, Clone)]
pub struct Dictionary<'a> {
    functions: HashMap<&'a QualifiedName, &'a FunctionPrototype>,
    inductives: HashMap<&'a QualifiedName, &'a InductiveTypeDef>,
}

impl<'a> From<&'a Module> for Dictionary<'a> {
    fn from(module: &'a Module) -> Self {
        let functions = module
            .functions
            .iter()
            .map(|x| &x.prototype)
            .chain(module.external_functions.iter())
            .map(|x| (&x.name, x))
            .collect();
        let inductives = module
            .inductive_types
            .iter()
            .map(|x| (&x.name, x))
            .collect();
        Self {
            functions,
            inductives,
        }
    }
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
    #[error("{} is not a valid type to appear at Rura's inductive type arguments", PrettyPrint::new(&**.0))]
    InvalidTypeArgument(Box<LirType>),
    #[error("closure captures an operand %{0} of an incompatible type {}", PrettyPrint::new(&**.1))]
    IncompatibleCapture(usize, Box<LirType>),
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
    #[error("unknown function {0} encountered")]
    UnknownFunction(QualifiedName),
    #[error("unknown inductive type constructor {0}::{1} encountered")]
    UnknownInductive(QualifiedName, Ident),
    #[error("failed to instantiate type variables inside {}", PrettyPrint::new(&**.0))]
    FailedToInstantiate(Box<LirType>),
    #[error("insufficient number of arguments for function call")]
    InsufficientArguments,
}

pub struct TypeInferenceContext<'a> {
    return_type: &'a LirType,
    type_variables: HashSet<&'a Ident>,
    context: Vec<VisitorContext<'a>>,
    errors: Vec<(Box<[VisitorContext<'a>]>, Error)>,
    current_function: HashMap<usize, LirType>,
    should_continue: bool,
}

impl<'a> Default for TypeInferenceContext<'a> {
    fn default() -> Self {
        Self::new()
    }
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
            should_continue: true,
        }
    }
    pub fn set_type(&mut self, operand: usize, ty: LirType) {
        self.check_type_var_for_operand(operand, &ty);
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
            self.context.clone().into(),
            Error::InconsistentBinaryOperands(lhs, Box::new(lhs_type), rhs, Box::new(rhs_type)),
        ));
        self.should_continue = false;
    }

    pub fn unknown_operand_type(&mut self, operand: usize) {
        self.errors.push((
            self.context.clone().into(),
            Error::UnknownOperandType(operand),
        ));
        self.should_continue = false;
    }

    pub fn invalid_interface_type(&mut self, arg_type: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::InvalidInterfaceType(Box::new(arg_type)),
        ));
        // could continue
    }

    pub fn invalid_type_argument(&mut self, arg_type: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::InvalidTypeArgument(Box::new(arg_type)),
        ));
        // could continue
    }

    pub fn imcompatible_capture(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::IncompatibleCapture(operand, Box::new(operand_type)),
        ));
        // could continue
    }

    pub fn invalid_return_value(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::InvalidReturnValue(operand, Box::new(operand_type)),
        ));
        // could continue
    }

    pub fn invalid_cast_target(&mut self, target: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::InvalidCastTarget(Box::new(target)),
        ));
        // could continue
    }

    fn check_type_var<F>(&mut self, error_maker: F, operand_type: &'_ LirType)
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
                self.context.clone().into(),
                error_maker(Box::new(operand_type.clone())),
            ));
        }
    }

    fn check_type_var_in_interface(&mut self, arg_type: &'a LirType) {
        self.check_type_var(Error::UnknownTypeVarInInterface, arg_type);
        // could continue
    }

    fn check_type_var_for_operand(&mut self, operand: usize, operand_type: &'_ LirType) {
        self.check_type_var(|x| Error::UnknownTypeVar(operand, x), operand_type);
        // could continue
    }

    fn invalid_function_argument(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::InvalidFunctionArgument(operand, Box::new(operand_type)),
        ));
        // could continue
    }
    fn invalid_operand(&mut self, operand: usize, operand_type: LirType) {
        self.errors.push((
            self.context.clone().into(),
            Error::InvalidOperand(operand, Box::new(operand_type)),
        ));
        self.should_continue = false;
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

impl LirVisitor for TypeInference<'_> {
    type Context<'a> = TypeInferenceContext<'a>;
    fn visit_function_def<'a>(
        &mut self,
        function: &'a FunctionDef,
        context: &mut TypeInferenceContext<'a>,
    ) {
        context.return_type = &function.prototype.return_type;
        context.should_continue = true;
        // insert all type variables
        context.type_variables = function.prototype.type_params.iter().collect();
        context.check_type_var_in_interface(&function.prototype.return_type);
        if !function.prototype.return_type.is_interface_compat() {
            context.invalid_interface_type(function.prototype.return_type.clone());
        }

        // insert all arguments as known type
        for (op, ty) in function.prototype.params.iter() {
            if !ty.is_interface_compat() {
                context.invalid_interface_type(ty.clone());
            }
            context.set_type(*op, ty.clone());
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
            Lir::Call(funcall) => match self.dictionary.functions.get(&funcall.function) {
                Some(prototype) => {
                    let unifier = prototype
                        .type_params
                        .iter()
                        .zip(funcall.type_params.iter())
                        .collect::<HashMap<_, _>>();
                    match subst(&unifier, &prototype.return_type) {
                        Some(ret_ty) => {
                            context.set_type(funcall.result, ret_ty.clone());
                            for ((_, expected), arg) in
                                prototype.params.iter().zip(funcall.args.iter())
                            {
                                let arg_ty = get_type!(context, *arg);
                                if !unified_compare(&unifier, expected, arg_ty) {
                                    context.invalid_function_argument(*arg, arg_ty.clone());
                                    return;
                                }
                            }
                        }
                        None => {
                            context.errors.push((
                                context.context.clone().into(),
                                Error::FailedToInstantiate(Box::new(prototype.return_type.clone())),
                            ));
                            context.should_continue = false;
                        }
                    }
                }
                None => {
                    context.errors.push((
                        context.context.clone().into(),
                        Error::UnknownFunction(funcall.function.clone()),
                    ));
                    context.should_continue = false;
                }
            },
            Lir::Clone { value, result } => {
                let ty = get_type!(context, *value);
                if !ty.is_object() {
                    context.invalid_operand(*value, ty.clone());
                } else {
                    context.set_type(*result, ty.clone());
                }
            }
            Lir::Closure(_) => unreachable!(),
            Lir::Drop { value, token } => {
                let ty = get_type!(context, *value);
                match ty {
                    LirType::Object(..) => {
                        context.set_type(*value, LirType::Token(Box::new(ty.clone())));
                    }
                    LirType::Unique(inner) if inner.is_object() => {
                        context.set_type(*value, inner.as_ref().clone());
                    }
                    _ => {
                        context.invalid_operand(*value, ty.clone());
                    }
                }
            }
            Lir::CtorCall(call) => {
                if !self.dictionary.inductives.contains_key(&call.type_name) {
                    context.errors.push((
                        context.context.clone().into(),
                        Error::UnknownInductive(call.type_name.clone(), call.ctor.clone()),
                    ));
                    return;
                }
                for (i, ty) in call.type_params.iter().enumerate() {
                    if !ty.is_materializable() {
                        context.invalid_type_argument(ty.clone());
                    }
                }
                let ty = LirType::Object(call.type_name.clone(), call.type_params.clone());
                context.set_type(call.result, ty);
            }
            Lir::InductiveElimination { .. } => unreachable!(),
            Lir::Return { value } => {
                let ty = get_type!(context, *value);
                if ty != context.return_type {
                    context.invalid_return_value(*value, ty.clone());
                }
            }
            Lir::TupleIntro { elements, result } => {
                let mut inner_types = vec![];
                for i in elements.iter().map(|x| {
                    context
                        .current_function
                        .get(x)
                        .filter(|x| x.is_materializable())
                        .ok_or(*x)
                        .cloned()
                }) {
                    match i {
                        Ok(ty) => inner_types.push(ty),
                        Err(operand) => {
                            context.unknown_operand_type(operand);
                            return;
                        }
                    }
                }
                context.set_type(*result, LirType::Tuple(inner_types.into_boxed_slice()));
            }
            Lir::TupleElim { tuple, eliminator } => {
                let ty = get_type!(context, *tuple);
                if let LirType::Tuple(inner) = ty {
                    if eliminator.len() != inner.len() {
                        context.invalid_operand(*tuple, ty.clone());
                    } else {
                        let target: Vec<_> = eliminator
                            .iter()
                            .copied()
                            .zip(inner.iter().cloned())
                            .collect();
                        for (v, t) in target {
                            context.set_type(v, t);
                        }
                    }
                } else {
                    context.invalid_operand(*tuple, ty.clone());
                }
            }
            Lir::UnaryOp(x) => {
                let ty = get_type!(context, x.operand);
                if (matches!(x.op, rura_core::UnOp::Neg) && !ty.is_numeric())
                    || (matches!(x.op, rura_core::UnOp::Not) && !ty.is_boolean())
                {
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
            Lir::Curry {
                function,
                result,
                type_params,
            } => {
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

    fn visit_block<'a>(
        &mut self,
        inner: &'a rura_core::lir::ir::Block,
        context: &mut Self::Context<'a>,
    ) {
        for (i, instr) in inner.0.iter().enumerate() {
            context.push_context(VisitorContext::Instruction(i));
            match instr {
                Lir::IfThenElse(inner) => self.visit_if_then_else(inner, context),
                Lir::Closure(inner) => self.visit_closure(inner, context),
                Lir::InductiveElimination {
                    eliminator,
                    inductive,
                } => self.visit_eliminator(*inductive, eliminator, context),
                _ => self.visit_normal_instruction(instr, context),
            }
            if !context.should_continue {
                break;
            }
            context.pop_context();
        }
    }
    fn visit_if_then_else<'a>(
        &mut self,
        inner: &'a rura_core::lir::ir::IfThenElse,
        context: &mut Self::Context<'a>,
    ) {
        let condition_ty = get_type!(context, inner.condition);
        if !condition_ty.is_boolean() {
            context.invalid_operand(inner.condition, condition_ty.clone());
        }
        context.push_context(VisitorContext::ThenBlock);
        self.visit_block(&inner.then_branch, context);
        context.pop_context();
        context.push_context(VisitorContext::ElseBlock);
        self.visit_block(&inner.else_branch, context);
        context.pop_context();
    }

    fn visit_eliminator<'a>(
        &mut self,
        inductive: usize,
        eliminator: &'a [rura_core::lir::ir::InductiveEliminator],
        context: &mut Self::Context<'a>,
    ) {
        todo!()
    }

    fn visit_closure<'a>(
        &mut self,
        inner: &'a rura_core::lir::ir::ClosureCreation,
        context: &mut Self::Context<'a>,
    ) {
        context.push_context(VisitorContext::Closure);
        let return_type = context.return_type;
        context.return_type = &inner.return_type;
        if !inner.return_type.is_interface_compat() {
            context.invalid_interface_type(inner.return_type.clone());
        }

        for (op, ty) in inner.params.iter() {
            if !ty.is_interface_compat() {
                context.invalid_interface_type(ty.clone());
            }
            context.set_type(*op, ty.clone());
        }

        let captures = get_free_variable(inner);
        for op in captures.iter() {
            let ty = get_type!(context, *op);
            if !ty.is_materializable() {
                context.imcompatible_capture(*op, ty.clone());
            }
        }

        self.visit_block(&inner.body, context);
        context.return_type = return_type;
        context.pop_context();
        context.set_type(
            inner.result,
            LirType::Closure(
                inner.params.iter().map(|(_, ty)| ty.clone()).collect(),
                Box::new(inner.return_type.clone()),
            ),
        );
    }
}

impl Pass for TypeInference<'_> {
    fn get_identifier(&self) -> &str {
        "type_inference"
    }
}

impl<'a> AnalysisPass<'a> for TypeInference<'a> {
    fn analyze(
        &mut self,
        module: &'a rura_core::lir::ir::Module,
    ) -> Box<[super::AnalysisError<'a>]> {
        let mut context = TypeInferenceContext::new();
        self.visit_module(module, &mut context);
        let errors = std::mem::take(&mut context.errors);
        module.add_metadata(self.get_identifier(), &self.types);
        errors
            .into_iter()
            .map(|(ctx, err)| super::AnalysisError {
                context: ctx,
                message: Box::new(err),
            })
            .collect()
    }
}

pub fn get_pass<'a>(module: &'a Module, _: &'_ toml::Table) -> BoxedPass<'a> {
    BoxedPass::Analysis(Box::new(TypeInference {
        types: HashMap::new(),
        dictionary: Dictionary::from(module),
    }))
}

#[cfg(test)]
mod test {
    use crate::{parser, pass::visitor::LirVisitor, pprint::PrettyPrint};
    use std::collections::HashMap;
    use winnow::Located;

    macro_rules! test_type_inference {
        (input = $input:literal, err = $err:expr) => {
            let mut input = Located::new($input);
            let mut module = parser::parse_module(&mut input).unwrap();

            let types = {
                let mut visitor = super::TypeInference {
                    types: HashMap::new(),
                    dictionary: super::Dictionary::from(&module),
                };
                let mut context = super::TypeInferenceContext::new();
                visitor.visit_module(&module, &mut context);
                for (ctx, err) in context.errors.iter() {
                    println!("{:?} : {}", ctx, err);
                }
                assert_eq!(context.errors.len(), $err);
                visitor.types
            };
            module.add_metadata("type_inference", &types);
            let pprint = format!("{}", PrettyPrint::new(&module));
            println!("{}", pprint);

            let mut input = Located::new(pprint.as_str());
            let new_module = parser::parse_module(&mut input).unwrap();
            assert_eq!(module, new_module);
        };
    }

    #[test]
    fn test_return_numeric() {
        test_type_inference!(
            input = r#"
        module typing {
            fn test(%0 : i32) -> (i32, i32) {
                %1 = constant 42 : i32;
                %2 = %0 + %1;
                %3 = (%0, %2);
                return %3;
            }
        }
        "#,
            err = 0
        );
    }

    #[test]
    fn test_closure_apply() {
        test_type_inference!(
            input = r#"
        module typing {
            fn test(%0 : fn (i32, i32) -> i32, %1 : i32) -> fn(i32) -> i32 {
                %2 = apply %0, %1;
                return %2;
            }
        }
        "#,
            err = 0
        );
    }

    #[test]
    fn test_closure_capture() {
        test_type_inference!(
            input = r#"
        module typing {
            fn test(%0 : i32) -> fn(i32) -> i32 {
                %2 = (%1 : i32) -> i32 {
                    %3 = %0 + %1;
                    return %3;
                };
                return %2;
            }
        }
        "#,
            err = 0
        );
    }

    #[test]
    fn test_external_funcall() {
        test_type_inference!(
            input = r#"
        module typing {
            fn length(%0 : str) -> usize;
            fn test() -> usize {
                %0 = constant "hello" : str;
                %1 = call length (%0);
                return %1;
            }
        }
        "#,
            err = 0
        );
    }
}
