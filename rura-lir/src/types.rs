use std::{fmt::Display, fmt::Formatter};

use rura_parsing::PrimitiveType;

use crate::{Ident, QualifiedName};

#[derive(
    Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub enum LirType {
    /// fn (A, B) -> C
    Closure(Box<[Self]>, Box<Self>),
    /// usize, i32, etc
    Primitive(PrimitiveType),
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
    /// Reuse Token
    Token(Box<Self>),
}

impl LirType {
    pub fn is_materializable(&self) -> bool {
        match self {
            LirType::Primitive(_) | LirType::Unit => true,
            LirType::Closure(a, b) => {
                a.iter().all(Self::is_materializable) && b.is_materializable()
            }
            LirType::Bottom => false,
            LirType::Object(_, params) => params.iter().all(Self::is_materializable),
            LirType::Tuple(components) => components.iter().all(Self::is_materializable),
            LirType::TypeVar(_) => true,
            LirType::Hole(_) => false,
            LirType::Ref(_) => false,
            LirType::Token(_) => false,
        }
    }
}

impl From<Box<[Self]>> for LirType {
    fn from(types: Box<[Self]>) -> Self {
        Self::Tuple(types)
    }
}

impl From<(Box<[Self]>, Box<Self>)> for LirType {
    fn from(f: (Box<[Self]>, Box<Self>)) -> Self {
        let (params, ret) = f;
        Self::Closure(params, ret)
    }
}
