#![allow(unused_variables, dead_code, private_interfaces)] // FIXME

use std::fmt::{Display, Formatter};

use winnow::combinator::{alt, opt, repeat, separated};
use winnow::error::ContextError;
use winnow::{PResult, Parser};

use rura_parsing::keywords::{BOOL, BOTTOM, FALSE, TRUE, TYPE, UNIT};
use rura_parsing::{
    braced, elidable, expect, field, fmt_delimited, function_type, identifier, members,
    opt_or_default, parenthesized, prefixed, primitive_type, qualified_name, reference_type,
    skip_space, suffixed, tuple_type, type_arguments, type_parameters, unique_type, Constructor,
    Name, PrimitiveType, QualifiedName,
};

#[derive(Clone, Debug)]
struct Parameter<T> {
    name: Name,
    typ: Box<T>,
}

impl<T> From<(Name, T)> for Parameter<T> {
    fn from(p: (Name, T)) -> Self {
        let (name, typ) = p;
        Self {
            name,
            typ: Box::new(typ),
        }
    }
}

type Parameters<T> = Box<[Parameter<T>]>;

#[derive(Clone, Debug)]
struct Declaration<T> {
    name: Name,
    type_parameters: Parameters<T>,
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
pub enum AST {
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
    TypeCall {
        type_callee: Box<Self>,
        type_arguments: Box<[Self]>,
    },
    Let {
        name: Name,
        typ: Box<Option<Self>>,
        value: Box<Self>,
        body: Box<Self>,
    },

    /// Reference type (refcount-free), statically tracked by the borrow checker.
    ReferenceType(Box<Self>),

    /// Unique RC type.
    UniqueType(Box<Self>),

    Variable(QualifiedName),
}

impl From<Box<[Self]>> for AST {
    fn from(types: Box<[Self]>) -> Self {
        Self::TupleType(types)
    }
}

impl From<(Box<[Self]>, Box<Self>)> for AST {
    fn from(f: (Box<[Self]>, Box<Self>)) -> Self {
        let (parameters, return_type) = f;
        Self::FunctionType {
            parameters,
            return_type,
        }
    }
}

impl Display for AST {
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
            Self::Let {
                name,
                typ,
                value,
                body,
            } => {
                write!(f, "let {name}")?;
                if let Some(typ) = typ.as_ref() {
                    write!(f, ": {typ}")?;
                }
                return write!(f, " = {value};\n\t{body}"); // TODO: pretty printing
            }
            Self::TypeCall {
                type_callee,
                type_arguments,
            } => {
                type_callee.fmt(f)?;
                write!(f, "::")?;
                return fmt_delimited(f, "<", type_arguments.iter(), ", ", ">");
            }
            Self::ReferenceType(t) => return write!(f, "&{t}"),
            Self::UniqueType(t) => return write!(f, "!{t}"),
            Self::Variable(n) => return n.fmt(f),
        })
    }
}

impl Parameter<AST> {
    fn type_parameter(name: Name) -> Self {
        Self {
            name,
            typ: Box::new(AST::Type),
        }
    }

    fn type_parameters(names: Box<[Name]>) -> Box<[Self]> {
        names
            .into_vec()
            .into_iter()
            .map(Self::type_parameter)
            .collect()
    }
}

type File = Box<[Declaration<AST>]>;

pub fn file(i: &mut &str) -> PResult<File> {
    repeat(
        0..,
        skip_space(alt((function_declaration, inductive_declaration))),
    )
    .map(|f: Vec<_>| f.into_boxed_slice())
    .parse_next(i)
}

fn elidable_definition<'a, F>(d: F) -> impl Parser<&'a str, Definition<AST>, ContextError>
where
    F: Copy + Parser<&'a str, Definition<AST>, ContextError>,
{
    alt((braced(d), suffixed(d, elidable(";"))))
}

fn function_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "fn",
        skip_space(identifier),
        opt_or_default(type_parameters).map(Parameter::type_parameters),
        function_parameters,
        opt(prefixed("->", skip_space(type_expression)))
            .map(|t| Box::new(t.unwrap_or(AST::UnitType))),
        elidable_definition(function_definition),
    )
        .map(
            |(_, name, type_parameters, parameters, return_type, definition)| Declaration {
                name,
                type_parameters,
                parameters,
                return_type,
                definition,
            },
        )
        .context(expect("function"))
        .parse_next(i)
}

fn function_parameters(i: &mut &str) -> PResult<Box<[Parameter<AST>]>> {
    parenthesized(separated(0.., field(type_expression), ","))
        .map(|p: Vec<_>| p.into_iter().map(From::from).collect())
        .parse_next(i)
}

fn function_definition(i: &mut &str) -> PResult<Definition<AST>> {
    block_statement
        .map(|e| Definition::Function(Box::new(e)))
        .parse_next(i)
}

fn inductive_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "enum",
        skip_space(identifier),
        opt_or_default(type_parameters),
        elidable_definition(inductive_definition),
    )
        .map(|(_, name, types, definition)| Declaration {
            name,
            type_parameters: types
                .into_vec()
                .into_iter()
                .map(Parameter::type_parameter)
                .collect(),
            parameters: Default::default(),
            return_type: Box::new(AST::Type),
            definition,
        })
        .context(expect("inductive type"))
        .parse_next(i)
}

fn inductive_definition(i: &mut &str) -> PResult<Definition<AST>> {
    separated(1.., skip_space(inductive_constructor), elidable(","))
        .map(|ctors: Vec<_>| Definition::Inductive(ctors.into_boxed_slice()))
        .parse_next(i)
}

fn inductive_constructor(i: &mut &str) -> PResult<Constructor<Name, AST>> {
    (identifier, skip_space(members(type_expression)))
        .map(|(name, params)| Constructor { name, params })
        .context(expect("constructor"))
        .parse_next(i)
}

fn block_statement(i: &mut &str) -> PResult<AST> {
    alt((let_statement, return_statement))
        .context(expect("block statement"))
        .parse_next(i)
}

fn let_statement(i: &mut &str) -> PResult<AST> {
    (
        "let",
        skip_space(identifier),
        opt(prefixed(":", skip_space(type_expression))).map(Box::new),
        "=",
        value_expression.map(Box::new),
        elidable(";"),
        block_statement.map(Box::new),
    )
        .map(|(_, name, typ, _, value, _, body)| AST::Let {
            name,
            typ,
            value,
            body,
        })
        .context(expect("let statement"))
        .parse_next(i)
}

fn return_statement(i: &mut &str) -> PResult<AST> {
    value_expression
        .context(expect("return statement"))
        .parse_next(i)
}

fn value_expression(i: &mut &str) -> PResult<AST> {
    todo!()
}

fn type_expression(i: &mut &str) -> PResult<AST> {
    alt((
        UNIT.map(|_| AST::UnitType),
        BOTTOM.map(|_| AST::BottomType),
        primitive_type.map(AST::PrimitiveType),
        tuple_type(type_expression),
        function_type(type_expression),
        reference_type(type_expression).map(AST::ReferenceType),
        unique_type(type_expression).map(AST::UniqueType),
        type_call,
        qualified_name.map(AST::Variable),
        parenthesized(type_expression),
    ))
    .context(expect("type expression"))
    .parse_next(i)
}

fn type_call(i: &mut &str) -> PResult<AST> {
    (qualified_name, "::", type_arguments(type_expression))
        .map(|(qn, _, type_arguments)| AST::TypeCall {
            type_callee: Box::new(AST::Variable(qn)),
            type_arguments,
        })
        .parse_next(i)
}

#[cfg(test)]
mod tests;
