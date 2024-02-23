use rura_parsing::PrimitiveType;
use std::{any::Any, collections::HashMap, ops::Deref, rc::Rc};

use crate::lir::InductiveTypeDef;
use crate::types::LirType;
use crate::HashMapProxy;
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

/// A shape of a type decides the memory layout of the type
/// Tuple and Composite are separated since Tuple is a special case where
/// the elements are stored in an unboxed way
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
    #[error("Hole type is not materializable")]
    HoleType,
    #[error("Bottom type does not have a shape")]
    BottomType,
    #[error("Ref type is not materializable")]
    RefType,
    #[error("Inductive type is not fully applied")]
    IncompleteInductive,
}

pub fn get_inductive_shape<'a, 'b: 'a, I: ExactSizeIterator<Item = &'b LirType>>(
    inductive: &'b InductiveTypeDef,
    type_params: I,
    known_type_variable: &'_ mut HashMap<&'a Ident, &'b LirType>,
) -> Result<Shape, ShapeError> {
    if inductive.type_params.len() != type_params.len() {
        return Err(ShapeError::IncompleteInductive);
    }

    let mut known_type_variable = HashMapProxy::new(known_type_variable);

    for (param, ty) in inductive.type_params.iter().zip(type_params) {
        known_type_variable.insert(param, ty);
    }

    let mut shapes = inductive
        .ctors
        .iter()
        .map(|c| {
            c.params
                .iter()
                .map(|(_, ty)| get_value_shape(ty, known_type_variable.get_inner()))
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

pub fn get_value_shape<'a, 'b: 'a>(
    ty: &'b LirType,
    known_type_variable: &'_ HashMap<&'a Ident, &'b LirType>,
) -> Result<Shape, ShapeError> {
    match ty {
        LirType::Primitive(primitive) => match primitive {
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
            PrimitiveType::Str => Ok(Shape::Closed(Layout::new::<&'static str>())),
        },
        LirType::Unit => Ok(Shape::Closed(Layout::new::<()>())),
        LirType::Bottom => Err(ShapeError::BottomType),
        // value of an object is always a pointer
        LirType::Object(..) => Ok(Shape::Closed(Layout::new::<Rc<Option<usize>>>())),
        LirType::Closure { .. } => Ok(Shape::Closed(Layout::new::<Rc<dyn Any>>())),
        LirType::Tuple(tuple) => {
            let shapes = tuple
                .iter()
                .map(|x| get_value_shape(x, known_type_variable))
                .collect::<Result<Vec<_>, _>>()?;
            if shapes.len() == 1 {
                return Ok(shapes.into_iter().next().unwrap());
            }
            Ok(Shape::Tuple(shapes.into_boxed_slice()))
        }
        LirType::TypeVar(var) => match var {
            TypeVar::Plain(ident) => {
                if let Some(t) = known_type_variable.get(ident).copied() {
                    Ok(get_value_shape(t, known_type_variable)?)
                } else {
                    Ok(Shape::OpenVar(Box::new(var.clone())))
                }
            }
            _ => Ok(Shape::OpenVar(Box::new(var.clone()))),
        },
        LirType::Hole(..) => Err(ShapeError::HoleType),
        LirType::Ref(..) => Err(ShapeError::RefType),
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
    use rura_parsing::Member;

    use crate::lir::CtorDef;
    use crate::pprint::PrettyPrint;
    use crate::types::*;
    use crate::QualifiedName;

    use super::*;

    #[test]
    fn test_try_from_rura_type_for_shape() {
        let ty: LirType = LirType::Tuple(Box::new([
            LirType::Primitive(PrimitiveType::I32),
            LirType::Primitive(PrimitiveType::F64),
        ]));
        let shape = get_value_shape(&ty, &HashMap::new()).unwrap();
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
        let ind_type = InductiveTypeDef {
            name: QualifiedName(Box::new(["Box".into()])),
            type_params: Box::new([Ident("T".into())]),
            bounds: [].into(),
            ctors: Box::new([CtorDef {
                name: Ident("Box".into()),
                params: Box::new([(
                    Member::Index(0),
                    LirType::TypeVar(TypeVar::Plain(Ident("T".into()))),
                )]),
            }]),
        };
        let shape_x = get_inductive_shape(
            &ind_type,
            [&LirType::Primitive(PrimitiveType::USize)].into_iter(),
            &mut HashMap::new(),
        )
        .unwrap();
        let rura_type = LirType::Object(
            QualifiedName(Box::new(["Box".into()])),
            Box::new([LirType::Primitive(PrimitiveType::USize)]),
        );
        let shape_y =
            get_inductive_shape(&ind_type, [&rura_type].into_iter(), &mut HashMap::new()).unwrap();
        println!(
            "shape: {:?}, type: {}",
            shape_y,
            PrettyPrint::new(&rura_type)
        );
        assert_eq!(shape_x, shape_y);
    }
}
