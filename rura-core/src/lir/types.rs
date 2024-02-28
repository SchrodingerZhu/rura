use std::{fmt::Display, fmt::Formatter};

use crate::lir::{Ident, QualifiedName};
use crate::PrimitiveType;

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
    /// Unique Rc
    Unique(Box<Self>),
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
            LirType::Unique(_) => false,
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            LirType::Primitive(p) => p.is_numeric(),
            _ => false,
        }
    }
    pub fn is_unique(&self) -> bool {
        matches!(self, LirType::Unique(_))
    }
    pub fn is_boolean(&self) -> bool {
        matches!(self, LirType::Primitive(PrimitiveType::Bool))
    }
    pub fn is_interface_compat(&self) -> bool {
        if let LirType::Unique(x) = self {
            x.is_object() && x.is_materializable()
        } else {
            self.is_materializable()
        }
    }
    pub fn is_object(&self) -> bool {
        matches!(self, LirType::Object(_, _))
    }
}

impl From<Box<[Self]>> for LirType {
    fn from(types: Box<[Self]>) -> Self {
        Self::Tuple(types)
    }
}

impl From<(Box<[Self]>, Self)> for LirType {
    fn from(f: (Box<[Self]>, Self)) -> Self {
        let (params, ret) = f;
        Self::Closure(params, Box::new(ret))
    }
}
