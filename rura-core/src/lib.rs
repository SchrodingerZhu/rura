use std::{any::Any, collections::HashMap, ops::Deref, rc::Rc};

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
    TypeRef(QualifiedName, Box<[RuraType]>),
    ResolvedInductive(Box<InductiveType>, Box<[RuraType]>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Layout(std::alloc::Layout);

impl Deref for Layout {
    type Target = std::alloc::Layout;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialOrd for Layout {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Layout {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0.size(), self.0.align()).cmp(&(other.0.size(), other.0.align()))
    }
}

impl Layout {
    pub fn new<T>() -> Self {
        Self(std::alloc::Layout::new::<T>())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Shape {
    Closed(Layout),
    OpenVar(Ident),
    Tuple(Box<[Shape]>),
    Composite(Box<[Shape]>),
    Alternative(Box<[Shape]>),
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
        .map(|x| {
            x.map(|x| {
                if x.len() != 1 {
                    Shape::Composite(x)
                } else {
                    x.into_vec().into_iter().next().unwrap()
                }
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if shapes.len() == 1 {
        return Ok(shapes.into_iter().next().unwrap());
    }
    shapes.sort_unstable();
    Ok(Shape::Alternative(shapes.into_boxed_slice()))
}

fn get_shape<'a, 'b: 'a>(
    rura_type: &'b RuraType,
    context: &'a mut HashMap<&'b Ident, Shape>,
) -> Result<Shape, ShapeError> {
    match rura_type {
        RuraType::Scalar(scalar) => match scalar {
            ScalarType::I8 => Ok(Shape::Closed(Layout::new::<i8>())),
            ScalarType::I16 => Ok(Shape::Closed(Layout::new::<i16>())),
            ScalarType::I32 => Ok(Shape::Closed(Layout::new::<i32>())),
            ScalarType::I64 => Ok(Shape::Closed(Layout::new::<i64>())),
            ScalarType::ISize => Ok(Shape::Closed(Layout::new::<isize>())),
            ScalarType::I128 => Ok(Shape::Closed(Layout::new::<i128>())),
            ScalarType::U8 => Ok(Shape::Closed(Layout::new::<u8>())),
            ScalarType::U16 => Ok(Shape::Closed(Layout::new::<u16>())),
            ScalarType::U32 => Ok(Shape::Closed(Layout::new::<u32>())),
            ScalarType::U64 => Ok(Shape::Closed(Layout::new::<u64>())),
            ScalarType::USize => Ok(Shape::Closed(Layout::new::<usize>())),
            ScalarType::U128 => Ok(Shape::Closed(Layout::new::<u128>())),
            ScalarType::F32 => Ok(Shape::Closed(Layout::new::<f32>())),
            ScalarType::F64 => Ok(Shape::Closed(Layout::new::<f64>())),
            ScalarType::Bool => Ok(Shape::Closed(Layout::new::<bool>())),
            ScalarType::Char => Ok(Shape::Closed(Layout::new::<char>())),
        },
        RuraType::Unit => Ok(Shape::Closed(Layout::new::<()>())),
        RuraType::Bottom => Err(ShapeError::BottomType),
        RuraType::Inductive(inductive) => {
            if !inductive.type_params.is_empty() {
                return Err(ShapeError::UnresolvedType);
            }
            get_shape_inductive(inductive.as_ref(), context)
        }
        RuraType::Closure(x) => {
            if x.args.len() > 16 {
                Ok(Shape::Closed(Layout::new::<Rc<()>>()))
            } else {
                Ok(Shape::Closed(Layout::new::<Rc<dyn Any>>()))
            }
        }
        RuraType::Tuple(tuple) => {
            let shapes = tuple
                .iter()
                .map(|t| Shape::try_from(t))
                .collect::<Result<Vec<_>, _>>()?;
            if shapes.len() == 1 {
                return Ok(shapes.into_iter().next().unwrap());
            }
            Ok(Shape::Tuple(shapes.into_boxed_slice()))
        }
        RuraType::TypeVar(ident) => {
            if let Some(t) = context.get(ident).cloned() {
                Ok(t)
            } else {
                Ok(Shape::OpenVar(ident.clone()))
            }
        }
        RuraType::TypeRef(_, _) => Ok(Shape::Closed(Layout::new::<Rc<()>>())),
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
                Shape::Closed(Layout::new::<i32>()),
                Shape::Closed(Layout::new::<f64>()),
            ]))
        );
    }

    #[test]
    fn test_rura_type_usize_object_reusable() {
        let box_type = InductiveType {
            qualified_name: QualifiedName(Box::new(["Box".into()])),
            type_params: Box::new([Ident("T".into())]),
            constructors: Box::new([Constructor {
                name: Ident("Box".into()),
                args: Box::new([RuraType::TypeVar(Ident("T".into()))]),
            }]),
        };
        let rura_type = RuraType::ResolvedInductive(
            Box::new(box_type.clone()),
            Box::new([RuraType::Scalar(ScalarType::USize)]),
        );
        let shape_x = Shape::try_from(&rura_type).unwrap();
        let rura_type = RuraType::ResolvedInductive(
            Box::new(box_type),
            Box::new([RuraType::TypeRef(
                QualifiedName(Box::new(["Box".into()])),
                Box::new([RuraType::Scalar(ScalarType::USize)]),
            )]),
        );
        let shape_y = Shape::try_from(&rura_type).unwrap();
        assert_eq!(shape_x, shape_y);
    }
}
