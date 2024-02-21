use rura_core::types::{LirType, TypeVar};
use std::fmt::{Display, Formatter};

#[repr(transparent)]
pub struct PrettyPrint<'a, T>(&'a T);

impl Display for PrettyPrint<'_, TypeVar> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            TypeVar::Plain(ident) => {
                write!(f, "@{}", ident.as_ref())
            }
            TypeVar::Associated(ident, ty) => {
                write!(f, "@{}::{}", ident.as_ref(), ty.as_ref())
            }
            TypeVar::AsExpr(ty, qn, ident) => {
                write!(f, "<{} as {}>::{}", PrettyPrint(&**ty), qn, ident)
            }
        }
    }
}

impl Display for PrettyPrint<'_, LirType> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            LirType::Closure(params, ret) => {
                write!(f, "fn(")?;
                for (idx, ty) in params.iter().enumerate() {
                    write!(f, "{}", PrettyPrint(ty))?;
                    if idx + 1 != params.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", PrettyPrint(&**ret))
            }
            LirType::Scalar(x) => write!(f, "{}", x),
            LirType::Unit => write!(f, "()"),
            LirType::Bottom => write!(f, "!"),
            LirType::Object(qn, params) => {
                write!(f, "{}", qn)?;
                if !params.is_empty() {
                    write!(f, "<")?;
                    for (idx, ty) in params.iter().enumerate() {
                        write!(f, "{}", PrettyPrint(ty))?;
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
                    write!(f, "{},", PrettyPrint(ty))?;
                }
                write!(f, ")")
            }
            LirType::TypeVar(var) => write!(f, "{}", PrettyPrint(var)),
            LirType::Hole(ty) => write!(f, "◊{}", PrettyPrint(&**ty)),
            LirType::Ref(ty) => write!(f, "&{}", PrettyPrint(&**ty)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rura_core::types::ScalarType;
    use rura_core::{Ident, QualifiedName};
    #[test]
    fn test_lir_type_pretty_print() {
        let ty = LirType::Object(
            QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Option")])),
            Box::new([LirType::Scalar(ScalarType::USize)]),
        );
        assert_eq!(format!("{}", PrettyPrint(&ty)), "std::Option<usize>");
    }
}
