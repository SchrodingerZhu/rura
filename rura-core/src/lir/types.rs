use std::collections::HashMap;
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

// unify simple type arguments and compare the type equality
pub fn unified_compare<'a>(
    unifier: &'_ HashMap<&'a Ident, &'a LirType>,
    unified: &'a LirType,
    target: &'a LirType,
) -> bool {
    match (unified, target) {
        (LirType::Primitive(a), LirType::Primitive(b)) => a == b,
        (LirType::Unit, LirType::Unit) => true,
        (LirType::Bottom, LirType::Bottom) => true,
        (LirType::Closure(a, b), LirType::Closure(c, d)) => {
            a.len() == c.len()
                && a.iter()
                    .zip(c.iter())
                    .all(|(x, y)| unified_compare(unifier, x, y))
                && unified_compare(unifier, b, d)
        }
        (LirType::Object(a, b), LirType::Object(c, d)) => {
            a == c
                && b.len() == d.len()
                && b.iter()
                    .zip(d.iter())
                    .all(|(x, y)| unified_compare(unifier, x, y))
        }
        (LirType::Tuple(a), LirType::Tuple(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(x, y)| unified_compare(unifier, x, y))
        }
        (LirType::TypeVar(a), b) => {
            if let TypeVar::Plain(a) = a {
                if let Some(a) = unifier.get(a) {
                    return unified_compare(unifier, a, b);
                }
            }
            if let LirType::TypeVar(b) = b {
                return a == b;
            }
            false
        }
        (LirType::Hole(a), LirType::Hole(b)) => unified_compare(unifier, a, b),
        (LirType::Ref(a), LirType::Ref(b)) => unified_compare(unifier, a, b),
        (LirType::Token(a), LirType::Token(b)) => unified_compare(unifier, a, b),
        (LirType::Unique(a), LirType::Unique(b)) => unified_compare(unifier, a, b),
        _ => false,
    }
}
