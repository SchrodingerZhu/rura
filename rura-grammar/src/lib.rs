#![allow(unused_variables, dead_code, private_interfaces)] // FIXME

use std::fmt::{Display, Formatter};

use winnow::ascii::dec_uint;
use winnow::combinator::{alt, opt, repeat, separated};
use winnow::{PResult, Parser};

use rura_parsing::keywords::{BOTTOM, TYPE, UNIT};
use rura_parsing::{
    binary, braced, closure_parameters, constant, constructor_parameters, elidable, elidable_block,
    expect, fmt_delimited, function_parameters, function_type, identifier, members, parenthesized,
    prefixed, primitive_type, qualified_name, reference_type, skip_space, tuple, tuple_type,
    type_arguments, type_parameters, unary, unique_type, BinOp, Constant, Constructor, Name,
    PrimitiveType, QualifiedName, UnOp,
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

type Parameters<T> = Box<[Parameter<T>]>;

#[derive(Clone, Debug)]
struct Declaration<T> {
    name: Name,
    type_parameters: Option<Parameters<T>>,
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
    Constant(Constant),

    TupleType(Box<[Self]>),
    Tuple(Box<[Self]>),
    Indexing {
        tuple: Box<Self>,
        index: usize,
    },

    FunctionType {
        parameters: Box<[Self]>,
        return_type: Box<Self>,
    },
    Closure {
        parameters: Box<[Name]>,
        body: Box<Self>,
    },
    TypeCall {
        type_callee: Box<Self>,
        type_arguments: Box<[Self]>,
    },
    Let {
        name: Name,
        typ: Option<Box<Self>>,
        value: Box<Self>,
        body: Box<Self>,
    },
    Call {
        callee: Box<Self>,
        type_arguments: Option<Box<[Self]>>,
        arguments: Box<[Self]>,
    },
    UnaryOp {
        op: UnOp,
        argument: Box<Self>,
    },
    BinaryOp {
        lhs: Box<Self>,
        op: BinOp,
        rhs: Box<Self>,
    },
    IfThenElse {
        condition: Box<Self>,
        then_branch: Box<Self>,
        else_branch: Box<Self>,
    },
    Matching {
        argument: Box<Self>,
        matchers: Box<[Matcher]>,
    },
    Access {
        argument: Box<Self>,
        name: Name,
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

impl From<(UnOp, Box<Self>)> for AST {
    fn from(v: (UnOp, Box<Self>)) -> Self {
        let (op, argument) = v;
        Self::UnaryOp { op, argument }
    }
}

impl From<(Box<Self>, BinOp, Box<Self>)> for AST {
    fn from(v: (Box<Self>, BinOp, Box<Self>)) -> Self {
        let (lhs, op, rhs) = v;
        Self::BinaryOp { lhs, op, rhs }
    }
}

impl From<(Box<AST>, Box<[Matcher]>)> for AST {
    fn from(m: (Box<AST>, Box<[Matcher]>)) -> Self {
        let (argument, matchers) = m;
        Self::Matching { argument, matchers }
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Type => TYPE,
            Self::BottomType => BOTTOM,
            Self::UnitType => UNIT,
            Self::PrimitiveType(t) => return t.fmt(f),
            Self::Constant(v) => return v.fmt(f),
            Self::TupleType(xs) | Self::Tuple(xs) => {
                return fmt_delimited(f, "(", xs.iter(), ", ", ")");
            }
            Self::Indexing { tuple, index } => return write!(f, "{tuple}.{index}"),
            Self::FunctionType {
                parameters,
                return_type,
            } => {
                write!(f, "fn ")?;
                fmt_delimited(f, "(", parameters.iter(), ", ", ")")?;
                return write!(f, " -> {return_type}");
            }
            Self::Closure { parameters, body } => {
                fmt_delimited(f, "|", parameters.iter(), ", ", "|")?;
                return write!(f, " {{ {body} }}");
            }
            Self::TypeCall {
                type_callee,
                type_arguments,
            } => {
                type_callee.fmt(f)?;
                write!(f, "::")?;
                return fmt_delimited(f, "<", type_arguments.iter(), ", ", ">");
            }
            Self::Let {
                name,
                typ,
                value,
                body,
            } => {
                write!(f, "let {name}")?;
                if let Some(typ) = typ {
                    write!(f, ": {typ}")?;
                }
                return write!(f, " = {value};\n\t{body}"); // TODO: pretty printing
            }
            Self::Call {
                callee,
                type_arguments,
                arguments,
            } => {
                callee.fmt(f)?;
                if let Some(types) = type_arguments {
                    write!(f, "::")?;
                    fmt_delimited(f, "<", types.iter(), ", ", ">")?;
                }
                return fmt_delimited(f, "(", arguments.iter(), ", ", ")");
            }
            Self::UnaryOp { op, argument } => return write!(f, "{op}{argument}"),
            Self::BinaryOp { lhs, op, rhs } => return write!(f, "{lhs} {op} {rhs}"),
            Self::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => {
                return write!(
                    f,
                    "if {condition} {{ {then_branch} }} else {{ {else_branch} }}" // TODO: pretty printing
                );
            }
            Self::Matching { argument, matchers } => {
                writeln!(f, "match {argument} {{")?;
                for m in matchers.as_ref() {
                    writeln!(f, "\t{m}")?;
                }
                return writeln!(f, "}}");
            }
            Self::Access { argument, name } => return write!(f, "{argument}.{name}"),
            Self::ReferenceType(t) => return write!(f, "&{t}"),
            Self::UniqueType(t) => return write!(f, "!{t}"),
            Self::Variable(n) => return n.fmt(f),
        })
    }
}

#[derive(Clone, Debug)]
struct Matcher {
    constructor: QualifiedName,
    arguments: Option<Box<[Name]>>,
    body: AST,
}

impl Display for Matcher {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.constructor)?;
        if let Some(args) = &self.arguments {
            fmt_delimited(f, "(", args.iter(), ", ", ")")?;
        }
        write!(f, " => {{ {} }}", self.body)
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

fn function_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "fn",
        skip_space(identifier),
        opt(type_parameters).map(|ps| ps.map(Parameter::type_parameters)),
        skip_space(function_parameters(type_expression)),
        opt(prefixed("->", skip_space(type_expression)))
            .map(|t| Box::new(t.unwrap_or(AST::UnitType))),
        elidable_block(function_definition, ";"),
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

fn function_definition(i: &mut &str) -> PResult<Definition<AST>> {
    block_statement
        .map(|e| Definition::Function(Box::new(e)))
        .parse_next(i)
}

fn inductive_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "enum",
        skip_space(identifier),
        opt(type_parameters),
        elidable_block(inductive_definition, ";"),
    )
        .map(|(_, name, types, definition)| Declaration {
            name,
            type_parameters: types.map(Parameter::type_parameters),
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
        opt(prefixed(":", skip_space(type_expression))).map(|t| t.map(Box::new)),
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
    alt((
        unary(value_expression),
        binary(value_expression),
        closure,
        indexing,
        call,
        if_then_else,
        matching,
        access,
        primary_value_expression,
    ))
    .context(expect("value expression"))
    .parse_next(i)
}

fn closure(i: &mut &str) -> PResult<AST> {
    (
        closure_parameters,
        alt((braced(block_statement), value_expression)).map(Box::new),
    )
        .map(|(parameters, body)| AST::Closure { parameters, body })
        .context(expect("closure expression"))
        .parse_next(i)
}

fn indexing(i: &mut &str) -> PResult<AST> {
    (
        alt((call, primary_value_expression)).map(Box::new),
        ".",
        dec_uint,
    )
        .map(|(tuple, _, index)| AST::Indexing { tuple, index })
        .context(expect("tuple indexing expression"))
        .parse_next(i)
}

fn call(i: &mut &str) -> PResult<AST> {
    (
        primary_value_expression,
        repeat(
            1..,
            skip_space((
                opt(prefixed("::", type_arguments(type_expression))),
                tuple(value_expression),
            )),
        ),
    )
        .map(|(f, args): (AST, Vec<_>)| {
            args.into_iter()
                .fold(f, |f, (type_arguments, arguments)| AST::Call {
                    callee: Box::new(f),
                    type_arguments,
                    arguments,
                })
        })
        .context(expect("function call"))
        .parse_next(i)
}

fn if_then_else(i: &mut &str) -> PResult<AST> {
    (
        "if",
        skip_space(value_expression).map(Box::new),
        skip_space(block_statement).map(Box::new),
        "else",
        skip_space(block_statement).map(Box::new),
    )
        .map(
            |(_, condition, then_branch, _, else_branch)| AST::IfThenElse {
                condition,
                then_branch,
                else_branch,
            },
        )
        .context(expect("if expression"))
        .parse_next(i)
}

fn matching(i: &mut &str) -> PResult<AST> {
    (
        "match",
        skip_space(value_expression).map(Box::new),
        elidable_block(matchers, ";"),
    )
        .map(|(_, argument, matchers)| AST::Matching { argument, matchers })
        .context(expect("match expression"))
        .parse_next(i)
}

fn matchers(i: &mut &str) -> PResult<Box<[Matcher]>> {
    repeat(1.., matcher)
        .map(|v: Vec<_>| v.into_boxed_slice())
        .parse_next(i)
}

fn matcher(i: &mut &str) -> PResult<Matcher> {
    (
        skip_space(qualified_name),
        opt(skip_space(constructor_parameters())),
        "=>",
        elidable_block(block_statement, ","),
    )
        .map(|(constructor, arguments, _, body)| Matcher {
            constructor,
            arguments,
            body,
        })
        .context(expect("matcher"))
        .parse_next(i)
}

fn access(i: &mut &str) -> PResult<AST> {
    (
        primary_value_expression,
        repeat(1.., prefixed(".", identifier::<Name>)),
    )
        .map(|(arg, ns): (AST, Vec<_>)| {
            ns.into_iter().fold(arg, |a, name| AST::Access {
                argument: Box::new(a),
                name,
            })
        })
        .context(expect("access expression"))
        .parse_next(i)
}

fn primary_value_expression(i: &mut &str) -> PResult<AST> {
    alt((
        constant.map(AST::Constant),
        qualified_name.map(AST::Variable),
        tuple(value_expression).map(AST::Tuple),
        parenthesized(value_expression),
    ))
    .context(expect("primary value expression"))
    .parse_next(i)
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
        .context(expect("type call expression"))
        .parse_next(i)
}

#[cfg(test)]
mod tests;
