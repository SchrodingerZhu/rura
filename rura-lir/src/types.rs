use std::{fmt::Display, fmt::Formatter};

use rura_parsing::ScalarType;

use crate::{Ident, QualifiedName};

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
