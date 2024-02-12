use std::collections::HashMap;

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
pub struct InductiveType {
    pub qualified_name: QualifiedName,
    pub type_params: Box<[Ident]>,
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
    ResolvedInductive(Box<InductiveType>, Box<[RuraType]>),
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

#[derive(Debug, thiserror::Error)]
pub enum ShapeError {
    #[error("Inductive type has unsolved type parameters")]
    UnresolvedType,
    #[error("Bottom type does not have a shape")]
    BottomType,
}

fn get_shape_inductive<'a, 'b: 'a>(
    inductive: &'b InductiveType,
    context: &'a mut HashMap<&'b Ident, Shape>,
) -> Result<Shape, ShapeError> {
    let mut shapes = inductive
        .constructors
        .iter()
        .map(|c| {
            c.args
                .iter()
                .map(|t| get_shape(t, context))
                .collect::<Result<Vec<_>, _>>()
        })
        .map(|x| x.map(Vec::into_boxed_slice))
        .map(|x| x.map(Shape::Composite))
        .collect::<Result<Vec<_>, _>>()?;
    shapes.sort_unstable();
    Ok(Shape::Alternative(shapes.into_boxed_slice()))
}

fn get_shape<'a, 'b: 'a>(
    rura_type: &'b RuraType,
    context: &'a mut HashMap<&'b Ident, Shape>,
) -> Result<Shape, ShapeError> {
    match rura_type {
        RuraType::Scalar(scalar) => Ok(Shape::Scalar(*scalar)),
        RuraType::Unit => Ok(Shape::Unit),
        RuraType::Bottom => Err(ShapeError::BottomType),
        RuraType::Inductive(inductive) => {
            if !inductive.type_params.is_empty() {
                return Err(ShapeError::UnresolvedType);
            }
            get_shape_inductive(inductive.as_ref(), context)
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
        RuraType::TypeVar(ident) => {
            if let Some(t) = context.get(ident).cloned() {
                Ok(t)
            } else {
                Ok(Shape::TypeVar(ident.clone()))
            }
        }
        RuraType::TypeRef(_) => Ok(Shape::Object),
        RuraType::ResolvedInductive(inductive, args) => {
            let mut changelog = Vec::new();
            for (param, arg) in inductive.type_params.iter().zip(args.iter()) {
                let shape = get_shape(arg, context)?;
                changelog.push((param, context.insert(param, shape)));
            }
            let shape = get_shape_inductive(inductive.as_ref(), context);
            for (param, old) in changelog {
                if let Some(old) = old {
                    context.insert(param, old);
                } else {
                    context.remove(param);
                }
            }
            shape
        }
    }
}

impl TryFrom<&'_ RuraType> for Shape {
    // TODO: Implement this
    type Error = ShapeError;

    fn try_from(value: &'_ RuraType) -> Result<Self, Self::Error> {
        let mut context = HashMap::new();
        get_shape(value, &mut context)
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
