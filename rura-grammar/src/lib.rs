#![allow(unused_variables, dead_code, private_interfaces)] // FIXME

use std::fmt::{Display, Formatter};

use winnow::combinator::{alt, delimited, repeat, separated};
use winnow::{PResult, Parser};

use rura_parsing::{
    expect, identifier, members, optional_type_parameters, skip_space, Constructor, Name,
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

    Bool,
    False,
    True,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Expression::Type => "type",
            Expression::Bool => "bool",
            Expression::False => "false",
            Expression::True => "true",
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
        skip_space(identifier::<Name>),
        optional_type_parameters::<Name>,
        "{",
        "}",
    )
        .map(|(_, name, types, _, _)| Declaration {
            name,
            parameters: types
                .into_vec()
                .into_iter()
                .map(Parameter::type_parameter)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            return_type: Box::new(Expression::Type),
            definition: Definition::Undefined, // TODO
        })
        .context(expect("inductive type declaration"))
        .parse_next(i)
}

fn inductive_braced_definition(i: &mut &str) -> PResult<Definition<Expression>> {
    delimited("{", inductive_constructors, "}").parse_next(i)
}

fn inductive_constructors(i: &mut &str) -> PResult<Definition<Expression>> {
    separated(1.., skip_space(inductive_constructor), ",")
        .map(|ctors: Vec<_>| Definition::Inductive(ctors.into_boxed_slice()))
        .parse_next(i)
}

fn inductive_constructor(i: &mut &str) -> PResult<Constructor<Name, Expression>> {
    (identifier, skip_space(members(expression)))
        .map(|(name, params)| Constructor { name, params })
        .context(expect("constructor definition"))
        .parse_next(i)
}

fn expression(_: &mut &str) -> PResult<Expression> {
    todo!()
}

#[cfg(test)]
mod tests;
