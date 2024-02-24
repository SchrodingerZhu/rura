#![allow(unused_variables, dead_code, private_interfaces)] // FIXME

use std::fmt::{Display, Formatter};

use winnow::combinator::{alt, delimited, repeat, separated};
use winnow::{PResult, Parser};

use rura_parsing::keywords::{BOOL, BOTTOM, FALSE, TRUE, TYPE, UNIT};
use rura_parsing::{
    elidable, expect, fmt_delimited, function_type, identifier, members, opt_or_default,
    primitive_type, reference_type, skip_space, tuple_type, type_parameters, unique_type,
    Constructor, Name, PrimitiveType,
};

#[derive(Clone, Debug)]
struct Parameter<T> {
    name: Name,
    typ: Box<T>,
}

type Parameters<T> = Box<[Parameter<T>]>;

#[derive(Clone, Debug)]
struct Declaration<T> {
    name: Name,
    parameters: Parameters<T>,
    return_type: Box<T>,
    definition: Definition<T>,
}

#[derive(Clone, Debug)]
pub enum Definition<T> {
    Undefined,
    Function(Box<T>),
    Inductive(Box<[Constructor<Name, T>]>),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Type,

    BottomType,

    UnitType,

    PrimitiveType(PrimitiveType),

    BoolType,
    False,
    True,

    TupleType(Box<[Self]>),

    FunctionType {
        parameters: Box<[Self]>,
        return_type: Box<Self>,
    },

    /// Reference type (refcount-free), statically tracked by the borrow checker.
    ReferenceType(Box<Self>),

    /// Unique RC type.
    UniqueType(Box<Self>),
}

impl From<Box<[Self]>> for Expression {
    fn from(types: Box<[Self]>) -> Self {
        Self::TupleType(types)
    }
}

impl From<(Box<[Self]>, Box<Self>)> for Expression {
    fn from(f: (Box<[Self]>, Box<Self>)) -> Self {
        let (parameters, return_type) = f;
        Self::FunctionType {
            parameters,
            return_type,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Type => TYPE,
            Self::BottomType => BOTTOM,
            Self::UnitType => UNIT,
            Self::PrimitiveType(v) => return v.fmt(f),
            Self::BoolType => BOOL,
            Self::False => FALSE,
            Self::True => TRUE,
            Self::TupleType(types) => return fmt_delimited(f, "(", types.iter(), ", ", ")"),
            Self::FunctionType {
                parameters,
                return_type,
            } => {
                write!(f, "fn ")?;
                fmt_delimited(f, "(", parameters.iter(), ", ", ")")?;
                return write!(f, " -> {return_type}");
            }
            Self::ReferenceType(t) => return write!(f, "&{t}"),
            Self::UniqueType(t) => return write!(f, "!{t}"),
        })
    }
}

impl Parameter<Expression> {
    fn type_parameter(name: Name) -> Self {
        Self {
            name,
            typ: Box::new(Expression::Type),
        }
    }
}

type File = Box<[Declaration<Expression>]>;

pub fn file(i: &mut &str) -> PResult<File> {
    repeat(
        0..,
        skip_space(alt((function_declaration, inductive_declaration))),
    )
    .map(|f: Vec<_>| f.into_boxed_slice())
    .parse_next(i)
}

fn function_declaration(_: &mut &str) -> PResult<Declaration<Expression>> {
    todo!()
}

fn inductive_declaration(i: &mut &str) -> PResult<Declaration<Expression>> {
    (
        "enum",
        skip_space(identifier),
        opt_or_default(type_parameters),
        alt((
            inductive_braced_definition,
            inductive_brace_elided_definition,
        )),
    )
        .map(|(_, name, types, definition)| Declaration {
            name,
            parameters: types
                .into_vec()
                .into_iter()
                .map(Parameter::type_parameter)
                .collect(),
            return_type: Box::new(Expression::Type),
            definition,
        })
        .context(expect("inductive type"))
        .parse_next(i)
}

fn inductive_braced_definition(i: &mut &str) -> PResult<Definition<Expression>> {
    delimited("{", inductive_constructors, "}").parse_next(i)
}

fn inductive_brace_elided_definition(i: &mut &str) -> PResult<Definition<Expression>> {
    (inductive_constructors, elidable(";"))
        .map(|p| p.0)
        .parse_next(i)
}

fn inductive_constructors(i: &mut &str) -> PResult<Definition<Expression>> {
    separated(1.., skip_space(inductive_constructor), elidable(","))
        .map(|ctors: Vec<_>| Definition::Inductive(ctors.into_boxed_slice()))
        .parse_next(i)
}

fn inductive_constructor(i: &mut &str) -> PResult<Constructor<Name, Expression>> {
    (identifier, skip_space(members(type_expression)))
        .map(|(name, params)| Constructor { name, params })
        .context(expect("constructor"))
        .parse_next(i)
}

fn type_expression(i: &mut &str) -> PResult<Expression> {
    alt((
        UNIT.map(|_| Expression::UnitType),
        BOTTOM.map(|_| Expression::BottomType),
        primitive_type.map(Expression::PrimitiveType),
        tuple_type(type_expression),
        function_type(type_expression),
        reference_type(type_expression).map(Expression::ReferenceType),
        unique_type(type_expression).map(Expression::UniqueType),
    ))
    .context(expect("type expression"))
    .parse_next(i)
}

#[cfg(test)]
mod tests;
