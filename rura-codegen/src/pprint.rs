use rura_core::types::{LirType, TypeVar};
use std::fmt::{Display, Formatter};

use crate::lir::{ArithMode, BinOp, BinaryOp, Block, ClosureCreation, FunctionCall, Lir};

pub struct PrettyPrint<'a, T> {
    target: &'a T,
    indent: usize,
}

impl<'a, T> PrettyPrint<'a, T> {
    pub fn new(target: &'a T) -> Self {
        PrettyPrint { target, indent: 0 }
    }

    pub fn with_indent(target: &'a T, indent: usize) -> Self {
        PrettyPrint { target, indent }
    }

    pub fn next_level<'b, Z>(&self, target: &'b Z) -> PrettyPrint<'b, Z> {
        PrettyPrint {
            target,
            indent: self.indent + 1,
        }
    }

    pub fn same_level<'b, Z>(&self, target: &'b Z) -> PrettyPrint<'b, Z> {
        PrettyPrint {
            target,
            indent: self.indent,
        }
    }
}

impl Display for PrettyPrint<'_, TypeVar> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.target {
            TypeVar::Plain(ident) => {
                write!(f, "@{}", ident.as_ref())
            }
            TypeVar::Associated(ident, ty) => {
                write!(f, "@{}::{}", ident.as_ref(), ty.as_ref())
            }
            TypeVar::AsExpr(ty, qn, ident) => {
                write!(f, "<{} as {}>::{}", PrettyPrint::new(&**ty), qn, ident)
            }
        }
    }
}

impl Display for PrettyPrint<'_, LirType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.target {
            LirType::Closure(params, ret) => {
                write!(f, "fn(")?;
                for (idx, ty) in params.iter().enumerate() {
                    write!(f, "{}", PrettyPrint::new(ty))?;
                    if idx + 1 != params.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", PrettyPrint::new(&**ret))
            }
            LirType::Scalar(x) => write!(f, "{}", x),
            LirType::Unit => write!(f, "()"),
            LirType::Bottom => write!(f, "!"),
            LirType::Object(qn, params) => {
                write!(f, "{}", qn)?;
                if !params.is_empty() {
                    write!(f, "<")?;
                    for (idx, ty) in params.iter().enumerate() {
                        write!(f, "{}", PrettyPrint::new(ty))?;
                        if idx + 1 != params.len() {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            LirType::Tuple(types) => {
                write!(f, "(")?;
                for ty in types.iter() {
                    write!(f, "{},", PrettyPrint::new(ty))?;
                }
                write!(f, ")")
            }
            LirType::TypeVar(var) => write!(f, "{}", PrettyPrint::new(var)),
            LirType::Hole(ty) => write!(f, "◊{}", PrettyPrint::new(&**ty)),
            LirType::Ref(ty) => write!(f, "&{}", PrettyPrint::new(&**ty)),
        }
    }
}

impl Display for PrettyPrint<'_, ArithMode> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.target {
            ArithMode::Default => write!(f, "[default]"),
            ArithMode::Saturating => write!(f, "[saturating]"),
            ArithMode::Wrapping => write!(f, "[wrapping]"),
        }
    }
}

impl Display for PrettyPrint<'_, Block> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        if self.target.0.is_empty() {
            return write!(f, "}}");
        } else {
            writeln!(f)?;
        }
        for stmt in self.target.0.iter() {
            writeln!(f, "{}", self.next_level(stmt))?;
        }
        write!(f, "{}}}", "\t".repeat(self.indent))
    }
}

impl Display for PrettyPrint<'_, BinOp> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let op = match self.target {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        };
        write!(f, "{}", op)
    }
}

impl Display for PrettyPrint<'_, BinaryOp> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.target.mode.as_ref() {
            None => write!(
                f,
                "%{} = %{} {} %{};",
                self.target.result,
                self.target.lhs,
                PrettyPrint::new(&self.target.op),
                self.target.rhs
            ),
            Some(mode) => write!(
                f,
                "%{} = %{} {} %{} {};",
                self.target.result,
                self.target.lhs,
                PrettyPrint::new(&self.target.op),
                self.target.rhs,
                PrettyPrint::new(mode)
            ),
        }
    }
}

fn print_separated<D: Display, I: ExactSizeIterator<Item = D>>(
    f: &mut Formatter<'_>,
    iter: I,
    sep: &str,
) -> std::fmt::Result {
    let length = iter.len();
    for (idx, item) in iter.enumerate() {
        write!(f, "{}", item)?;
        if idx + 1 != length {
            write!(f, "{}", sep)?;
        }
    }
    Ok(())
}

#[repr(transparent)]
struct Var(usize);

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Display for PrettyPrint<'_, FunctionCall> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%{} = call {} (",
            self.target.result, self.target.function
        )?;
        print_separated(f, self.target.args.iter().copied().map(Var), ", ")?;
        write!(f, ");")
    }
}

impl Display for PrettyPrint<'_, ClosureCreation> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        struct TypedParam<'a> {
            var: Var,
            ty: &'a LirType,
        }
        impl Display for TypedParam<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}: {}", self.var, PrettyPrint::new(self.ty))
            }
        }
        write!(f, "%{} = (", self.target.result)?;
        print_separated(
            f,
            self.target
                .params
                .iter()
                .map(|(x, ty)| TypedParam { var: Var(*x), ty }),
            ", ",
        )?;
        write!(
            f,
            ") -> {} {}",
            PrettyPrint::new(&self.target.return_type),
            self.same_level(&self.target.body)
        )
    }
}

impl Display for PrettyPrint<'_, Lir> {
    #[allow(unused)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "\t".repeat(self.indent))?;
        match self.target {
            Lir::Apply {
                closure,
                arg,
                result,
            } => write!(f, "%{} = apply %{}, %{};", result, closure, arg),
            Lir::BinaryOp(x) => write!(f, "{}", PrettyPrint::new(&**x)),
            Lir::Call(call) => write!(f, "{}", PrettyPrint::new(&**call)),
            Lir::Clone { value, result } => write!(f, "%{} = clone %{};", result, value),
            Lir::Closure(x) => write!(f, "{}", self.same_level(&**x)),
            Lir::Drop { value, token } => todo!(),
            Lir::CtorCall(_) => todo!(),
            Lir::InductiveElimination {
                inductive,
                eliminator,
            } => todo!(),
            Lir::Return { value } => write!(f, "return %{};", value),
            Lir::TupleIntro { elements, result } => todo!(),
            Lir::TupleElim { tuple, eliminator } => todo!(),
            Lir::UnaryOp(_) => todo!(),
            Lir::IfThenElse(_) => todo!(),
            Lir::ConstantScalar { value, result } => todo!(),
            Lir::Fill { hole, value } => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse_module;
    use rura_core::types::ScalarType;
    use rura_core::{Ident, QualifiedName};
    fn assert_type_eq(ty: &LirType) {
        let src = format!("module test {{ fn test() -> {}; }}", PrettyPrint::new(ty));
        let mut input = src.as_str();
        let module = parse_module(&mut input).unwrap();
        assert_eq!(&module.external_functions[0].return_type, ty);
    }
    fn assert_lir_eq(ir: &Lir) {
        let src = format!(
            "module test {{\n\tfn test() -> () {{\n{}\n\t}}\n}}",
            PrettyPrint::with_indent(ir, 2)
        );
        println!("{}", src);
        let mut input = src.as_str();
        let module = parse_module(&mut input).unwrap();
        assert_eq!(&module.functions[0].body.0[0], ir);
    }
    #[test]
    fn test_lir_type_pprint() {
        let ty = LirType::Object(
            QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Option")])),
            Box::new([LirType::Scalar(ScalarType::USize)]),
        );
        assert_eq!(format!("{}", PrettyPrint::new(&ty)), "std::Option<usize>");
        assert_type_eq(&ty);
        assert_type_eq(&LirType::Scalar(ScalarType::USize));
        assert_type_eq(&LirType::Tuple(Box::new([LirType::Scalar(
            ScalarType::USize,
        )])));
        assert_type_eq(&LirType::Closure(
            Box::new([LirType::Scalar(ScalarType::USize)]),
            Box::new(LirType::Scalar(ScalarType::USize)),
        ));
        assert_type_eq(&LirType::Ref(Box::new(LirType::Scalar(ScalarType::USize))));
        assert_type_eq(&LirType::TypeVar(TypeVar::Plain(Ident::new("T"))));
        assert_type_eq(&LirType::Hole(Box::new(LirType::Scalar(ScalarType::USize))));
    }
    #[test]
    fn test_apply_pprint() {
        let apply = Lir::Apply {
            closure: 0,
            arg: 1,
            result: 2,
        };
        assert_lir_eq(&apply);
    }
    #[test]
    fn test_binary_op_pprint() {
        let binop = BinaryOp {
            result: 0,
            lhs: 1,
            rhs: 2,
            op: BinOp::Add,
            mode: Some(ArithMode::Wrapping),
        };
        assert_eq!(
            format!("{}", PrettyPrint::new(&binop)),
            "%0 = %1 + %2 [wrapping];"
        );
        assert_lir_eq(&Lir::BinaryOp(Box::new(binop)));
    }
    #[test]
    fn test_function_call_pprint() {
        let call = FunctionCall {
            result: 0,
            function: QualifiedName::new(Box::new([Ident::new("foo")])),
            args: [1, 2].into(),
        };
        assert_eq!(
            format!("{}", PrettyPrint::new(&call)),
            "%0 = call foo (%1, %2);"
        );
        assert_lir_eq(&Lir::Call(Box::new(call)));
    }
    #[test]
    fn test_clone_pprint() {
        let clone = Lir::Clone {
            value: 0,
            result: 1,
        };
        assert_lir_eq(&clone);
    }
    #[test]
    fn test_closure_creation_pprint() {
        let closure = ClosureCreation {
            result: 0,
            params: [(1, LirType::Scalar(ScalarType::USize))].into(),
            return_type: LirType::Scalar(ScalarType::USize),
            body: Block([Lir::Return { value: 1 }].into()),
        };
        assert_lir_eq(&Lir::Closure(Box::new(closure)));
    }
}
