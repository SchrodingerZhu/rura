use std::{fmt::Display, fmt::Formatter};

use rura_parsing::ScalarType;

use crate::{fmt_separated, Ident, QualifiedName};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub name: Ident,
    pub args: Box<[RuraType]>,
}

impl Display for Constructor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.args.is_empty() {
            write!(f, " (")?;
            fmt_separated(f, &self.args, ", ")?;
            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InductiveType {
    pub qualified_name: QualifiedName,
    pub type_params: Box<[Ident]>,
    pub constructors: Box<[Constructor]>,
}

impl Display for InductiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum {}<", self.qualified_name)?;
        fmt_separated(f, &self.type_params, ", ")?;
        write!(f, "> {{")?;
        fmt_separated(f, &self.constructors, ", ")?;
        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClosureType {
    pub args: Box<[RuraType]>,
    pub ret: RuraType,
}

impl Display for ClosureType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn (")?;
        fmt_separated(f, &self.args, ", ")?;
        write!(f, ") -> {}", self.ret)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeVar {
    Plain(Ident),
    Associated(Ident, Ident),
    AsExpr(Box<Self>, QualifiedName, Ident),
}

impl Display for TypeVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeVar::Plain(ident) => write!(f, "{}", ident),
            TypeVar::Associated(ident, assoc) => write!(f, "{}::{}", ident, assoc),
            TypeVar::AsExpr(ty, ident, assoc) => write!(f, "<{} as {}>::{}", ty, ident, assoc),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RuraType {
    Scalar(ScalarType),
    Unit,
    Bottom,
    Inductive(Box<InductiveType>),
    Closure(Box<ClosureType>),
    Tuple(Box<[RuraType]>),
    TypeVar(Box<TypeVar>),
    TypeRef(QualifiedName, Box<[RuraType]>),
    ResolvedInductive(Box<InductiveType>, Box<[RuraType]>),
}

impl Display for RuraType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuraType::Scalar(scalar) => write!(f, "{}", scalar),
            RuraType::Unit => write!(f, "()"),
            RuraType::Bottom => write!(f, "!"),
            RuraType::Inductive(inductive) => write!(f, "{}", inductive),
            RuraType::Closure(closure) => write!(f, "{}", closure),
            RuraType::Tuple(tuple) => {
                write!(f, "(")?;
                fmt_separated(f, tuple, ", ")?;
                write!(f, ")")
            }
            RuraType::TypeVar(ident) => write!(f, "{}", ident),
            RuraType::TypeRef(name, args) => {
                write!(f, "{}<", name)?;
                fmt_separated(f, args, ", ")?;
                write!(f, ">")
            }
            RuraType::ResolvedInductive(inductive, args) => {
                write!(f, "{}<", inductive.qualified_name)?;
                fmt_separated(f, args, ", ")?;
                write!(f, ">")
            }
        }
    }
}

/// Surface Type is the type without detailed structure
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LirType {
    /// fn (A, B) -> C
    Closure(Box<[Self]>, Box<Self>),
    /// usize, i32, etc
    Scalar(ScalarType),
    /// ()
    Unit,
    /// !
    Bottom,
    /// List<T>, Option<T>, etc
    Object(QualifiedName, Box<[Self]>),
    /// (A, B, C, D, ...)
    Tuple(Box<[Self]>),
    /// A type variable in context
    TypeVar(TypeVar),
    /// Hole of mutable borrows
    Hole(Box<Self>),
    /// Immutable reference
    Ref(Box<Self>),
}
