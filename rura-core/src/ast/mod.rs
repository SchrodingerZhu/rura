use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::keywords::{BOTTOM, TYPE, UNIT};
use crate::{fmt_delimited, fmt_separated, BinOp, Constant, Constructor, PrimitiveType, UnOp};

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

impl<'a> From<&'a str> for Name {
    fn from(s: &'a str) -> Self {
        Self::new(s)
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

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleID {
    modules: Box<[Name]>,
}

impl ModuleID {
    pub fn is_none(&self) -> bool {
        self.modules.is_empty()
    }
}

impl From<Box<[Name]>> for ModuleID {
    fn from(modules: Box<[Name]>) -> Self {
        Self { modules }
    }
}

impl Display for ModuleID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_separated(f, self.modules.iter(), "::")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualifiedName {
    pub module_id: ModuleID,
    pub name: Name,
}

impl From<Box<[Name]>> for QualifiedName {
    fn from(names: Box<[Name]>) -> Self {
        let l = names.len();
        let mut modules = Vec::default();
        for (i, name) in names.into_vec().into_iter().enumerate() {
            if i + 1 < l {
                modules.push(name);
                continue;
            }
            return Self {
                module_id: From::from(modules.into_boxed_slice()),
                name,
            };
        }
        unreachable!()
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.module_id, self.name)
    }
}

#[derive(Clone, Debug)]
pub struct Parameter<T> {
    pub name: Name,
    pub typ: Box<T>,
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
    pub fn type_parameter(name: Name) -> Self {
        Self {
            name,
            typ: Box::new(AST::Type),
        }
    }

    pub fn type_parameters(names: Box<[Name]>) -> Box<[Self]> {
        names
            .into_vec()
            .into_iter()
            .map(Self::type_parameter)
            .collect()
    }
}

pub type Parameters<T> = Box<[Parameter<T>]>;

#[derive(Clone, Debug)]
pub struct Declaration<T> {
    pub name: Name,
    pub type_parameters: Option<Parameters<T>>,
    pub parameters: Parameters<T>,
    pub return_type: Box<T>,
    pub definition: Definition<T>,
}

impl Declaration<AST> {
    pub fn type_declaration(
        name: Name,
        types: Option<Box<[Name]>>,
        definition: Definition<AST>,
    ) -> Self {
        Self {
            name,
            type_parameters: types.map(Parameter::type_parameters),
            parameters: Default::default(),
            return_type: Box::new(AST::Type),
            definition,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Definition<T> {
    Undefined,
    Function(Box<T>),
    Enum(Enum<T>),
    Struct(Struct<T>),
}

#[derive(Clone, Debug)]
pub enum Enum<T> {
    Unchecked(Box<[Constructor<Name, T>]>),
    Checked(HashMap<Name, Constructor<Name, T>>),
}

#[derive(Clone, Debug)]
pub enum Struct<T> {
    Unchecked(Box<[(Name, T)]>),
    Checked(HashMap<Name, T>),
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
pub struct Matcher {
    pub constructor: QualifiedName,
    pub arguments: Option<Box<[Name]>>,
    pub body: AST,
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

pub type File = Box<[Declaration<AST>]>;
