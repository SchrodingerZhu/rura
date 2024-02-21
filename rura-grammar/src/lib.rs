#![allow(unused_variables, dead_code, private_interfaces)] // FIXME

use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use winnow::combinator::{alt, repeat};
use winnow::{PResult, Parser};

/// Name of symbols (e.g. global definitions, local variables), with its raw text and an associated
/// globally unique ID. The ID is just the address of the internal RC-tracked string.
///
/// The technique of globally unique IDs for names is called [capture-avoiding substitution].
///
/// [capture-avoiding substitution]: https://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
#[derive(Debug, Clone, Eq)]
pub struct Name(Rc<String>);

impl Name {
    fn new<S: Into<String>>(raw: S) -> Self {
        Self(Rc::new(raw.into()))
    }

    fn id(&self) -> usize {
        Rc::as_ptr(&self.0) as _
    }

    fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl PartialEq<Self> for Name {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Hash for Name {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

pub trait Syntax: Display {}

#[derive(Clone, Debug)]
struct Parameter<T: Syntax> {
    name: Name,
    typ: Box<T>,
}

type Parameters<T> = Box<[Parameter<T>]>;

#[derive(Clone, Debug)]
struct Declaration<T: Syntax> {
    name: Name,
    parameters: Parameter<T>,
    return_type: Box<T>,
    definition: Definition<T>,
}

#[derive(Clone, Debug)]
pub enum Definition<T: Syntax> {
    Undefined,
    Function(FunctionDefinition<T>),
    Inductive(InductiveDefinition<T>),
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition<T: Syntax> {
    body: Box<T>,
}

#[derive(Clone, Debug)]
pub struct InductiveDefinition<T: Syntax> {
    constructors: Box<[Constructor<T>]>,
}

#[derive(Clone, Debug)]
struct Constructor<T: Syntax> {
    name: Name,
    arguments: Box<[T]>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Bool,
    False,
    True,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Expression::Bool => "bool",
            Expression::False => "false",
            Expression::True => "true",
        })
    }
}

impl Syntax for Expression {}

type File = Box<[Declaration<Expression>]>;

pub fn file(i: &mut &str) -> PResult<File> {
    repeat(0.., declaration)
        .parse_next(i)
        .map(|f: Vec<_>| f.into_boxed_slice())
}

fn declaration(i: &mut &str) -> PResult<Declaration<Expression>> {
    alt((function_declaration, inductive_declaration)).parse_next(i)
}

fn function_declaration(i: &mut &str) -> PResult<Declaration<Expression>> {
    todo!()
}

fn inductive_declaration(i: &mut &str) -> PResult<Declaration<Expression>> {
    todo!()
}

#[cfg(test)]
mod tests;
