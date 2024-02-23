use rura_parsing::{Constant, Member};
use std::fmt::{Display, Formatter};

use crate::lir::{
    ArithMode, BinOp, BinaryOp, Block, Bound, ClosureCreation, CtorCall, CtorDef, EliminationStyle,
    FunctionCall, FunctionDef, FunctionPrototype, IfThenElse, InductiveEliminator,
    InductiveTypeDef, Lir, MakeMutReceiver, Module, TraitExpr, UnaryOp,
};
use crate::types::{LirType, TypeVar};
use crate::{fmt_separated, Ident, QualifiedName};

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
            LirType::Primitive(x) => write!(f, "{}", x),
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
        fmt_separated(f, self.target.args.iter().copied().map(Var), ", ")?;
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
        fmt_separated(
            f,
            self.target
                .params
                .iter()
                .map(|(x, ty)| TypedParam { var: Var(*x), ty }),
            ", ",
        )?;
        write!(
            f,
            ") -> {} {};",
            PrettyPrint::new(&self.target.return_type),
            self.same_level(&self.target.body)
        )
    }
}

impl Display for PrettyPrint<'_, Constant> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.target {
            Constant::I8(x) => write!(f, "{} : i8", x),
            Constant::I16(x) => write!(f, "{} : i16", x),
            Constant::I32(x) => write!(f, "{} : i32", x),
            Constant::I64(x) => write!(f, "{} : i64", x),
            Constant::I128(x) => write!(f, "{} : i128", x),
            Constant::ISize(x) => write!(f, "{} : isize", x),
            Constant::U8(x) => write!(f, "{} : u8", x),
            Constant::U16(x) => write!(f, "{} : u16", x),
            Constant::U32(x) => write!(f, "{} : u32", x),
            Constant::U64(x) => write!(f, "{} : u64", x),
            Constant::U128(x) => write!(f, "{} : u128", x),
            Constant::USize(x) => write!(f, "{} : usize", x),
            Constant::F32(x) => write!(f, "{} : f32", x),
            Constant::F64(x) => write!(f, "{} : f64", x),
            Constant::Bool(x) => write!(f, "{} : bool", x),
            Constant::Char(x) => write!(f, "{x:?} : char"),
            Constant::Literal(x) => write!(f, "{x:?} : str"),
        }
    }
}

impl Display for PrettyPrint<'_, IfThenElse> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if %{} {} else {}",
            self.target.condition,
            self.same_level(&self.target.then_branch),
            self.same_level(&self.target.else_branch)
        )
    }
}

impl Display for PrettyPrint<'_, UnaryOp> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "%{} = {}%{};",
            self.target.result,
            match self.target.op {
                crate::lir::UnOp::Neg => "-",
                crate::lir::UnOp::Not => "!",
            },
            self.target.operand
        )
    }
}

impl Display for PrettyPrint<'_, CtorCall> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{} = new", self.target.result)?;
        if let Some(token) = self.target.token {
            write!(f, " [%{}]", token)?;
        }
        write!(f, " {}", self.target.type_name)?;
        if !self.target.type_params.is_empty() {
            write!(f, "<")?;
            fmt_separated(
                f,
                self.target.type_params.iter().map(PrettyPrint::new),
                ", ",
            )?;
            write!(f, ">")?;
        }
        write!(f, " @ {} (", self.target.ctor)?;
        if !self.target.args.is_empty() {
            fmt_separated(f, self.target.args.iter().copied().map(Var), ", ")?;
        }
        write!(f, ")")?;
        if self.target.unique_rc {
            write!(f, " [unique]")?;
        }
        write!(f, ";")
    }
}

#[repr(transparent)]
struct Binding<'a>(&'a (Member<Ident>, usize));

impl Display for Binding<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 .0 {
            Member::Named(ident) => write!(f, "{} : {}", ident, Var(self.0 .1)),
            Member::Index(_) => write!(f, "{}", Var(self.0 .1)),
        }
    }
}

impl Display for PrettyPrint<'_, MakeMutReceiver> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.target.target {
            Member::Named(ident) => write!(
                f,
                "[%{}] {} : %{}",
                self.target.hole, ident, self.target.value
            ),
            Member::Index(_) => write!(f, "[%{}] %{}", self.target.hole, self.target.value),
        }
    }
}

fn print_binding_list(
    f: &mut Formatter<'_>,
    bindings: &[(Member<Ident>, usize)],
) -> std::fmt::Result {
    if bindings[0].0.is_named() {
        write!(f, "{{")?;
        fmt_separated(f, bindings.iter().map(Binding), ", ")?;
        write!(f, "}}")
    } else {
        write!(f, "(")?;
        fmt_separated(f, bindings.iter().map(Binding), ", ")?;
        write!(f, ")")
    }
}

fn print_match_arm_header(
    style: &EliminationStyle,
    ctor: &QualifiedName,
    f: &mut Formatter<'_>,
) -> std::fmt::Result {
    match style {
        EliminationStyle::Unwrap { fields, token } => {
            write!(f, "[unwrap(%{token})] {ctor}")?;
            print_binding_list(f, fields)
        }
        EliminationStyle::Mutation(bindings) => {
            write!(f, "[mutation] {}", ctor)?;
            if bindings[0].target.is_named() {
                write!(f, "{{")?;
                fmt_separated(f, bindings.iter().map(PrettyPrint::new), ", ")?;
                write!(f, "}}")
            } else {
                write!(f, "(")?;
                fmt_separated(f, bindings.iter().map(PrettyPrint::new), ", ")?;
                write!(f, ")")
            }
        }
        EliminationStyle::Fixpoint(x) => write!(f, "[fixpoint(%{x})] {}", ctor),
        EliminationStyle::Ref(x) => {
            write!(f, "[ref] {}", ctor)?;
            print_binding_list(f, x)
        }
    }
}

impl Display for PrettyPrint<'_, InductiveEliminator> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "\t".repeat(self.indent))?;
        print_match_arm_header(&self.target.style, &self.target.ctor, f)?;
        write!(f, " => {}", self.same_level(&self.target.body))
    }
}

impl Display for PrettyPrint<'_, TraitExpr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        #[repr(transparent)]
        struct TraitParameter<'a>(&'a (Option<Ident>, LirType));
        impl Display for TraitParameter<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match &self.0 .0 {
                    Some(ident) => write!(f, "{} = {}", ident, PrettyPrint::new(&self.0 .1)),
                    None => write!(f, "{}", PrettyPrint::new(&self.0 .1)),
                }
            }
        }
        write!(f, "{}", self.target.name)?;
        if !self.target.params.is_empty() {
            write!(f, "<")?;
            fmt_separated(f, self.target.params.iter().map(TraitParameter), ", ")?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl Display for PrettyPrint<'_, Bound> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : ", PrettyPrint::new(&self.target.target))?;
        fmt_separated(f, self.target.bounds.iter().map(PrettyPrint::new), " + ")
    }
}

impl Display for PrettyPrint<'_, FunctionPrototype> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        #[repr(transparent)]
        struct Parameter<'a>(&'a (usize, LirType));
        impl Display for Parameter<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}: {}", Var(self.0 .0), PrettyPrint::new(&self.0 .1))
            }
        }
        write!(f, "fn {}", self.target.name)?;
        if !self.target.type_params.is_empty() {
            write!(f, "<")?;
            fmt_separated(f, self.target.type_params.iter(), ", ")?;
            write!(f, ">")?;
        }
        write!(f, "(")?;
        fmt_separated(f, self.target.params.iter().map(Parameter), ", ")?;
        write!(f, ") -> {}", PrettyPrint::new(&self.target.return_type))?;
        if !self.target.bounds.is_empty() {
            let indent = self.indent + 1;
            let padding = "\t".repeat(indent);
            write!(f, "\n{}where ", padding)?;
            fmt_separated(
                f,
                self.target.bounds.iter().map(PrettyPrint::new),
                &format!(",\n{padding}      "),
            )?;
        }
        Ok(())
    }
}

impl Display for PrettyPrint<'_, FunctionDef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let padding = "\t".repeat(self.indent);
        write!(f, "{padding}{}", self.same_level(&self.target.prototype))?;
        write!(f, "\n{padding}{}", self.same_level(&self.target.body))
    }
}

fn print_member_list(
    f: &mut Formatter<'_>,
    members: &[(Member<Ident>, LirType)],
) -> std::fmt::Result {
    struct NamedMember<'a>(&'a Ident, &'a LirType);
    impl Display for NamedMember<'_> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}: {}", self.0, PrettyPrint::new(self.1))
        }
    }
    if members[0].0.is_named() {
        write!(f, "{{")?;
        fmt_separated(
            f,
            members.iter().map(|x| {
                let Member::Named(ident) = &x.0 else {
                    unreachable!("must be named member fields")
                };
                NamedMember(ident, &x.1)
            }),
            ", ",
        )?;
        write!(f, "}}")
    } else {
        write!(f, "(")?;
        fmt_separated(f, members.iter().map(|x| PrettyPrint::new(&x.1)), ", ")?;
        write!(f, ")")
    }
}

impl Display for PrettyPrint<'_, CtorDef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", "\t".repeat(self.indent), self.target.name)?;
        if !self.target.params.is_empty() {
            print_member_list(f, &self.target.params)
        } else {
            Ok(())
        }
    }
}

impl Display for PrettyPrint<'_, InductiveTypeDef> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let decl_padding = "\t".repeat(self.indent);
        write!(f, "{}enum {}", decl_padding, self.target.name)?;
        if !self.target.type_params.is_empty() {
            write!(f, "<")?;
            fmt_separated(f, self.target.type_params.iter(), ", ")?;
            write!(f, ">")?;
        }
        if !self.target.type_params.is_empty() {
            let padding = "\t".repeat(self.indent + 1);
            write!(f, "\n{}where ", padding)?;
            fmt_separated(
                f,
                self.target.bounds.iter().map(PrettyPrint::new),
                &format!(",\n{}      ", padding),
            )?;
        }
        writeln!(f, "\n{}{{", decl_padding)?;
        fmt_separated(
            f,
            self.target.ctors.iter().map(|x| self.next_level(x)),
            ",\n",
        )?;
        write!(f, "\n{decl_padding}}}")
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
            Lir::Drop { value, token } => match token {
                Some(token) => write!(f, "%{} = drop %{};", token, value),
                None => write!(f, "drop %{};", value),
            },
            Lir::CtorCall(inner) => write!(f, "{}", PrettyPrint::new(&**inner)),
            Lir::InductiveElimination {
                inductive,
                eliminator,
            } => {
                write!(f, "match %{} {{", inductive)?;
                for arm in eliminator.iter() {
                    write!(f, "\n{}", self.next_level(arm))?;
                }
                write!(f, "\n{}}}", "\t".repeat(self.indent))
            }
            Lir::Return { value } => write!(f, "return %{};", value),
            Lir::TupleIntro { elements, result } => {
                write!(f, "%{} = (", result)?;
                fmt_separated(f, elements.iter().copied().map(Var), ", ")?;
                write!(f, ");")
            }
            Lir::TupleElim { tuple, eliminator } => {
                write!(f, "(")?;
                fmt_separated(f, eliminator.iter().copied().map(Var), ", ")?;
                write!(f, ") = %{};", tuple)
            }
            Lir::UnaryOp(inner) => write!(f, "{}", self.same_level(&**inner)),
            Lir::IfThenElse(inner) => write!(f, "{}", self.same_level(&**inner)),
            Lir::Constant { value, result } => {
                write!(f, "%{} = constant {};", result, PrettyPrint::new(&**value))
            }
            Lir::Fill { hole, value } => write!(f, "fill %{} <- %{};", hole, value),
            Lir::Curry { function, result } => write!(f, "%{} = curry %{};", result, function),
            Lir::Unreachable { panic } => {
                if *panic {
                    write!(f, "unreachable [panic];")
                } else {
                    write!(f, "unreachable;")
                }
            }
        }
    }
}

impl Display for PrettyPrint<'_, Module> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "module {} {{", self.target.name)?;
        for typedef in self.target.inductive_types.iter() {
            writeln!(f, "{}\n", self.next_level(typedef))?;
        }
        for function in self.target.functions.iter() {
            writeln!(f, "{}\n", self.next_level(function))?;
        }
        for external in self.target.external_functions.iter() {
            let padding = "\t".repeat(self.indent + 1);
            writeln!(f, "{padding}{};", self.next_level(external))?;
        }
        write!(f, "}}")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse_module;
    use rura_parsing::PrimitiveType;
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
            Box::new([LirType::Primitive(PrimitiveType::USize)]),
        );
        assert_eq!(format!("{}", PrettyPrint::new(&ty)), "std::Option<usize>");
        assert_type_eq(&ty);
        assert_type_eq(&LirType::Primitive(PrimitiveType::USize));
        assert_type_eq(&LirType::Tuple(Box::new([LirType::Primitive(
            PrimitiveType::USize,
        )])));
        assert_type_eq(&LirType::Closure(
            Box::new([LirType::Primitive(PrimitiveType::USize)]),
            Box::new(LirType::Primitive(PrimitiveType::USize)),
        ));
        assert_type_eq(&LirType::Ref(Box::new(LirType::Primitive(
            PrimitiveType::USize,
        ))));
        assert_type_eq(&LirType::TypeVar(TypeVar::Plain(Ident::new("T"))));
        assert_type_eq(&LirType::Hole(Box::new(LirType::Primitive(
            PrimitiveType::USize,
        ))));
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
            params: [(1, LirType::Primitive(PrimitiveType::USize))].into(),
            return_type: LirType::Primitive(PrimitiveType::USize),
            body: Block([Lir::Return { value: 1 }].into()),
        };
        assert_lir_eq(&Lir::Closure(Box::new(closure)));
    }
    #[test]
    fn test_drop_pprint() {
        let drop = Lir::Drop {
            value: 0,
            token: Some(1),
        };
        assert_lir_eq(&drop);
        let drop = Lir::Drop {
            value: 0,
            token: None,
        };
        assert_lir_eq(&drop);
    }
    #[test]
    fn test_fill_pprint() {
        let fill = Lir::Fill { hole: 0, value: 1 };
        assert_lir_eq(&fill);
    }
    #[test]
    fn test_constant_char_pprint() {
        let constant = Lir::Constant {
            value: Box::new(Constant::Char('a')),
            result: 0,
        };
        assert_lir_eq(&constant);
    }
    #[test]
    fn test_constant_bool_pprint() {
        let constant = Lir::Constant {
            value: Box::new(Constant::Bool(true)),
            result: 0,
        };
        assert_lir_eq(&constant);
    }
    #[test]
    fn test_if_then_else_pprint() {
        let if_then_else = IfThenElse {
            condition: 0,
            then_branch: Block([Lir::Return { value: 1 }].into()),
            else_branch: Block([Lir::Return { value: 2 }].into()),
        };
        assert_lir_eq(&Lir::IfThenElse(Box::new(if_then_else)));
    }
    #[test]
    fn test_unary_op_pprint() {
        let unary_op = UnaryOp {
            result: 0,
            op: crate::lir::UnOp::Neg,
            operand: 1,
        };
        assert_lir_eq(&Lir::UnaryOp(Box::new(unary_op)));
        let unary_op = UnaryOp {
            result: 0,
            op: crate::lir::UnOp::Not,
            operand: 1,
        };
        assert_lir_eq(&Lir::UnaryOp(Box::new(unary_op)));
    }
    #[test]
    fn test_tuple_elim_pprint() {
        let tuple_elim = Lir::TupleElim {
            tuple: 0,
            eliminator: [1, 2].into(),
        };
        assert_lir_eq(&tuple_elim);
    }
    #[test]
    fn test_tuple_intro_pprint() {
        let tuple_intro = Lir::TupleIntro {
            elements: [0, 1].into(),
            result: 2,
        };
        assert_lir_eq(&tuple_intro);
    }
    #[test]
    fn test_ctor_call_pprint_without_token() {
        let ctor_call = CtorCall {
            result: 0,
            type_name: QualifiedName::new(Box::new([Ident::new("Option")])),
            ctor: Ident::new("Some"),
            type_params: Box::new([LirType::Primitive(PrimitiveType::USize)]),
            args: [1].into(),
            token: None,
            unique_rc: false,
        };
        assert_lir_eq(&Lir::CtorCall(Box::new(ctor_call)));
    }
    #[test]
    fn test_ctor_call_pprint_with_token() {
        let ctor_call = CtorCall {
            result: 0,
            type_name: QualifiedName::new(Box::new([Ident::new("Option")])),
            ctor: Ident::new("Some"),
            type_params: Box::new([LirType::Primitive(PrimitiveType::USize)]),
            args: [1].into(),
            token: Some(2),
            unique_rc: false,
        };
        assert_lir_eq(&Lir::CtorCall(Box::new(ctor_call)));
    }
    #[test]
    fn test_ctor_call_pprint_unqiue_rc() {
        let ctor_call = CtorCall {
            result: 0,
            type_name: QualifiedName::new(Box::new([Ident::new("Option")])),
            ctor: Ident::new("Some"),
            type_params: Box::new([LirType::Primitive(PrimitiveType::USize)]),
            args: [1].into(),
            token: None,
            unique_rc: true,
        };
        assert_lir_eq(&Lir::CtorCall(Box::new(ctor_call)));
    }
    #[test]
    fn test_inductive_elim_pprint() {
        let test = Lir::InductiveElimination {
            inductive: 0,
            eliminator: Box::new([
                InductiveEliminator {
                    ctor: QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                    style: EliminationStyle::Fixpoint(1),
                    body: Block(vec![
                        Lir::Constant {
                            value: Box::new(Constant::I32(3)),
                            result: 2,
                        },
                        Lir::Return { value: 2 },
                    ]),
                },
                InductiveEliminator {
                    ctor: QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                    style: EliminationStyle::Unwrap {
                        token: 3,
                        fields: Box::new([
                            (Member::Named("head".into()), 4),
                            (Member::Named("tail".into()), 5),
                        ]),
                    },
                    body: Block(vec![
                        Lir::Constant {
                            value: Box::new(Constant::I32(4)),
                            result: 6,
                        },
                        Lir::Return { value: 6 },
                    ]),
                },
            ]),
        };
        assert_lir_eq(&test);
    }

    #[test]
    fn test_module_pprint() {
        use std::io::Write;
        const MODULE: &str = r#"
        module test {
            enum List<T>
                where @T: Foo
            { Nil, Cons(@T, List<@T>) }
            fn test<T, U>(%1: i32, %2: f64) -> i32 where @T : std::TraitFoo + std::TraitBar<Head = ()>, @U : LoveYou {
                %3 = constant 3 : i32;
                %4 = constant "12\n" : str;
                return %3;
            }
            fn test2(%0 : i32) -> i32 {
                %1 = constant 3 : i32;
                %2 = (%0 : i32) -> i32 { 
                    %2 = %0 + %1; 
                    return %2;
                };
                %3 = apply %2, %0;
                return %3;
            }
            fn extern_test<T>(%1: i32, %2: f64) -> i32 where @T: std::TraitFoo + std::TraitBar<Head = ()>;
        }
        "#;
        let mut input = MODULE;
        let module = parse_module(&mut input).unwrap();
        let mut buffer = Vec::new();
        println!("{}", PrettyPrint::new(&module));
        write!(&mut buffer, "{}", PrettyPrint::new(&module)).unwrap();
        let string = String::from_utf8(buffer).unwrap();
        let mut input2 = string.as_str();
        let module2 = parse_module(&mut input2).unwrap();
        assert_eq!(module, module2);
    }
}
