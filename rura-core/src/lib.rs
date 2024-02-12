pub mod lir;

use syn::{Ident, TypeParam};

pub struct QualifiedName {
    pub module: Vec<Ident>,
    pub name: Ident,
}

pub enum ScalarType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,
}

pub struct Constructor {
    pub name: Ident,
    pub args: Vec<RuraType>,
}
pub struct InductiveType {
    pub name: syn::TypePath,
    pub type_params: Vec<TypeParam>,
    pub constructors: Vec<Constructor>,
}

pub struct ClosureType {
    pub args: Vec<RuraType>,
    pub ret: Box<RuraType>,
}

pub enum RuraType {
    Scalar(ScalarType),
    Unit,
    Bottom,
    Inductive(Box<InductiveType>),
    Closure(ClosureType),
    Tuple(Vec<RuraType>),
}
