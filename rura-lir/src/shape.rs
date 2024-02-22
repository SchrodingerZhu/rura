use rura_parsing::PrimitiveType;
use std::{any::Any, collections::HashMap, ops::Deref, rc::Rc};

use crate::lir::InductiveTypeDef;
use crate::{types::TypeVar, Ident};

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
    OpenVar(Box<TypeVar>),
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
    inductive: &'b InductiveTypeDef,
    context: &'a mut HashMap<&'b Ident, Shape>,
) -> Result<Shape, ShapeError> {
    let mut shapes = inductive
        .ctors
        .iter()
        .map(|c| {
            c.params
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
            PrimitiveType::I8 => Ok(Shape::Closed(Layout::new::<i8>())),
            PrimitiveType::I16 => Ok(Shape::Closed(Layout::new::<i16>())),
            PrimitiveType::I32 => Ok(Shape::Closed(Layout::new::<i32>())),
            PrimitiveType::I64 => Ok(Shape::Closed(Layout::new::<i64>())),
            PrimitiveType::ISize => Ok(Shape::Closed(Layout::new::<isize>())),
            PrimitiveType::I128 => Ok(Shape::Closed(Layout::new::<i128>())),
            PrimitiveType::U8 => Ok(Shape::Closed(Layout::new::<u8>())),
            PrimitiveType::U16 => Ok(Shape::Closed(Layout::new::<u16>())),
            PrimitiveType::U32 => Ok(Shape::Closed(Layout::new::<u32>())),
            PrimitiveType::U64 => Ok(Shape::Closed(Layout::new::<u64>())),
            PrimitiveType::USize => Ok(Shape::Closed(Layout::new::<usize>())),
            PrimitiveType::U128 => Ok(Shape::Closed(Layout::new::<u128>())),
            PrimitiveType::F32 => Ok(Shape::Closed(Layout::new::<f32>())),
            PrimitiveType::F64 => Ok(Shape::Closed(Layout::new::<f64>())),
            PrimitiveType::Bool => Ok(Shape::Closed(Layout::new::<bool>())),
            PrimitiveType::Char => Ok(Shape::Closed(Layout::new::<char>())),
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
                .map(Shape::try_from)
                .collect::<Result<Vec<_>, _>>()?;
            if shapes.len() == 1 {
                return Ok(shapes.into_iter().next().unwrap());
            }
            Ok(Shape::Tuple(shapes.into_boxed_slice()))
        }
        RuraType::TypeVar(var) => match var.as_ref() {
            TypeVar::Plain(ident) => {
                if let Some(t) = context.get(ident).cloned() {
                    Ok(t)
                } else {
                    Ok(Shape::OpenVar(var.clone()))
                }
            }
            _ => Ok(Shape::OpenVar(var.clone())),
        },
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
    type Error = ShapeError;

    fn try_from(value: &'_ RuraType) -> Result<Self, Self::Error> {
        let mut context = HashMap::new();
        get_shape(value, &mut context)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ShapeSimilarity {
    // Identical shapes
    Equal,
    // Shapes differs in only type variables
    Similar,
    // Shapes are completely different
    Different,
}

impl ShapeSimilarity {
    pub fn combine(self, other: Self) -> Self {
        match (self, other) {
            (ShapeSimilarity::Equal, ShapeSimilarity::Equal) => ShapeSimilarity::Equal,
            (ShapeSimilarity::Different, _) | (_, ShapeSimilarity::Different) => {
                ShapeSimilarity::Different
            }
            _ => ShapeSimilarity::Similar,
        }
    }
}

pub fn compare_shapes(a: &Shape, b: &Shape) -> ShapeSimilarity {
    match (a, b) {
        (Shape::Closed(a), Shape::Closed(b)) => {
            if a == b {
                ShapeSimilarity::Equal
            } else {
                ShapeSimilarity::Different
            }
        }
        (Shape::OpenVar(a), Shape::OpenVar(b)) => {
            if a == b {
                ShapeSimilarity::Equal
            } else {
                ShapeSimilarity::Similar
            }
        }
        (Shape::Tuple(a), Shape::Tuple(b)) => {
            if a.len() != b.len() {
                return ShapeSimilarity::Different;
            }
            a.iter()
                .zip(b.iter())
                .map(|(a, b)| compare_shapes(a, b))
                .fold(ShapeSimilarity::Equal, ShapeSimilarity::combine)
        }
        (Shape::Composite(a), Shape::Composite(b)) => {
            if a.len() != b.len() {
                return ShapeSimilarity::Different;
            }
            a.iter()
                .zip(b.iter())
                .map(|(a, b)| compare_shapes(a, b))
                .fold(ShapeSimilarity::Equal, ShapeSimilarity::combine)
        }
        (Shape::Alternative(a), Shape::Alternative(b)) => {
            if a.len() != b.len() {
                return ShapeSimilarity::Different;
            }
            a.iter()
                .zip(b.iter())
                .map(|(a, b)| compare_shapes(a, b))
                .fold(ShapeSimilarity::Equal, ShapeSimilarity::combine)
        }
        _ => ShapeSimilarity::Different,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::types::*;
    use crate::QualifiedName;

    #[test]
    fn test_try_from_rura_type_for_shape() {
        let rura_type = RuraType::Tuple(Box::new([
            RuraType::Scalar(PrimitiveType::I32),
            RuraType::Scalar(PrimitiveType::F64),
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
                args: Box::new([RuraType::TypeVar(Box::new(TypeVar::Plain(Ident(
                    "T".into(),
                ))))]),
            }]),
        };
        let rura_type = RuraType::ResolvedInductive(
            Box::new(box_type.clone()),
            Box::new([RuraType::Scalar(PrimitiveType::USize)]),
        );
        let shape_x = Shape::try_from(&rura_type).unwrap();
        let rura_type = RuraType::ResolvedInductive(
            Box::new(box_type),
            Box::new([RuraType::TypeRef(
                QualifiedName(Box::new(["Box".into()])),
                Box::new([RuraType::Scalar(PrimitiveType::USize)]),
            )]),
        );
        let shape_y = Shape::try_from(&rura_type).unwrap();
        println!("shape: {:?}, type: {}", shape_y, rura_type);
        assert_eq!(shape_x, shape_y);
    }
}
