pub mod lir;

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident(Box<str>);

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QualifiedName(Box<[Box<str>]>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ScalarType {
    I8,
    I16,
    I32,
    I64,
    ISize,
    I128,
    U8,
    U16,
    U32,
    U64,
    USize,
    U128,
    F32,
    F64,
    Bool,
    Char,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constructor {
    pub name: Ident,
    pub args: Box<[RuraType]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeParam {
    Resolved(Box<RuraType>),
    Pending(Ident),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InductiveType {
    pub qualified_name: QualifiedName,
    pub type_params: Box<[TypeParam]>,
    pub constructors: Box<[Constructor]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClosureType {
    pub args: Box<[RuraType]>,
    pub ret: RuraType,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RuraType {
    Scalar(ScalarType),
    Unit,
    Bottom,
    Inductive(Box<InductiveType>),
    Closure(Box<ClosureType>),
    Tuple(Box<[RuraType]>),
    TypeVar(Ident),
    TypeRef(QualifiedName),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Shape {
    Object,
    ErasedClosure,
    Closure,
    Tuple(Box<[Shape]>),
    Scalar(ScalarType),
    Unit,
    Composite(Box<[Shape]>),
    Alternative(Box<[Shape]>),
    TypeVar(Ident),
}

impl TryFrom<&'_ RuraType> for Shape {
    // TODO: Implement this
    type Error = ();

    fn try_from(value: &'_ RuraType) -> Result<Self, Self::Error> {
        match value {
            RuraType::Scalar(scalar) => Ok(Shape::Scalar(*scalar)),
            RuraType::Unit => Ok(Shape::Unit),
            RuraType::Bottom => Err(()),
            RuraType::Inductive(inductive) => {
                if inductive
                    .type_params
                    .iter()
                    .any(|t| matches!(t, TypeParam::Pending(_)))
                {
                    return Err(());
                }
                let mut shapes = inductive
                    .constructors
                    .iter()
                    .map(|c| {
                        c.args
                            .iter()
                            .map(|t| Shape::try_from(t))
                            .collect::<Result<Vec<_>, _>>()
                    })
                    .map(|x| x.map(Vec::into_boxed_slice))
                    .map(|x| x.map(Shape::Composite))
                    .collect::<Result<Vec<_>, _>>()?;
                shapes.sort_unstable();
                Ok(Shape::Alternative(shapes.into_boxed_slice()))
            }
            RuraType::Closure(x) => {
                if x.args.len() > 16 {
                    Ok(Shape::ErasedClosure)
                } else {
                    Ok(Shape::Closure)
                }
            }
            RuraType::Tuple(tuple) => {
                let shapes = tuple
                    .iter()
                    .map(|t| Shape::try_from(t))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Shape::Tuple(shapes.into_boxed_slice()))
            }
            RuraType::TypeVar(ident) => Ok(Shape::TypeVar(ident.clone())),
            RuraType::TypeRef(_) => Ok(Shape::Object),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_try_from_rura_type_for_shape() {
        let rura_type = RuraType::Tuple(Box::new([
            RuraType::Scalar(ScalarType::I32),
            RuraType::Scalar(ScalarType::F64),
        ]));
        let shape = Shape::try_from(&rura_type).unwrap();
        assert_eq!(
            shape,
            Shape::Tuple(Box::new([
                Shape::Scalar(ScalarType::I32),
                Shape::Scalar(ScalarType::F64)
            ]))
        );
    }
}
