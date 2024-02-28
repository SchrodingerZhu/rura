use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::keywords::{BOTTOM, TYPE, UNIT};
use crate::{
    fmt_delimited, fmt_separated, BinOp, Constant, Constructor, Input, PrimitiveType, Span, UnOp,
};

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

impl<'a> From<Input<'a>> for Name {
    fn from(s: Input<'a>) -> Self {
        Self::new(s.to_string())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleID {
    crate_name: Name,
    modules: Box<[Name]>,
}

impl ModuleID {
    pub fn is_none(&self) -> bool {
        self.modules.is_empty()
    }
}

impl From<Box<[Name]>> for ModuleID {
    fn from(names: Box<[Name]>) -> Self {
        let mut it = names.into_vec().into_iter();
        let crate_name = it.next().unwrap();
        let modules = it.collect();
        Self {
            crate_name,
            modules,
        }
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
    pub typ: T,
}

impl<T> From<(Name, T)> for Parameter<T> {
    fn from(p: (Name, T)) -> Self {
        let (name, typ) = p;
        Self { name, typ }
    }
}

impl Parameter<AST> {
    pub fn type_parameter(name: Name, span: Span) -> Self {
        Self {
            name,
            typ: AST {
                span,
                expr: Box::from(Expression::Type),
            },
        }
    }

    pub fn type_parameters(names: Box<[(Name, Span)]>) -> Box<[Self]> {
        names
            .into_vec()
            .into_iter()
            .map(|(name, span)| Self::type_parameter(name, span))
            .collect()
    }
}

pub type Parameters<T> = Box<[Parameter<T>]>;

#[derive(Clone, Debug)]
pub struct Module {
    pub id: ModuleID,
    pub declarations: Box<[Declaration<AST>]>,
}

#[derive(Clone, Debug)]
pub struct Declaration<T> {
    pub span: Span,
    pub definition: Definition<T>,
}

#[derive(Clone, Debug)]
pub enum Definition<T> {
    Undefined(UndefinedDefinition<T>),
    Function(FunctionDefinition<T>),
    Enum(EnumDefinition<T>),
    Struct(StructDefinition<T>),
    NestedSubmodule(Module),
    ExternalSubmodule(ModuleID),
}

#[derive(Clone, Debug)]
pub struct UndefinedDefinition<T> {
    pub name: Name,
    pub type_parameters: Option<Parameters<T>>,
    pub parameters: Parameters<T>,
    pub return_type: T,
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition<T> {
    pub name: Name,
    pub type_parameters: Option<Parameters<T>>,
    pub parameters: Parameters<T>,
    pub return_type: T,
    pub body: T,
}

#[derive(Clone, Debug)]
pub struct EnumDefinition<T> {
    pub name: Name,
    pub type_parameters: Option<Parameters<T>>,
    pub members: DatatypeMembers<Constructor<Name, T>>,
}

#[derive(Clone, Debug)]
pub struct StructDefinition<T> {
    pub name: Name,
    pub type_parameters: Option<Parameters<T>>,
    pub members: DatatypeMembers<T>,
}

#[derive(Clone, Debug)]
pub enum DatatypeMembers<T> {
    Unchecked(Box<[(Name, T)]>),
    Checked(HashMap<Name, T>),
}

#[derive(Clone, Debug)]
pub struct AST {
    pub span: Span,
    pub expr: Box<Expression>,
}

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.expr.fmt(f)
    }
}

impl From<(Constant, Span)> for AST {
    fn from(v: (Constant, Span)) -> Self {
        let (c, span) = v;
        Self {
            span,
            expr: Box::new(Expression::Constant(c)),
        }
    }
}

impl From<(QualifiedName, Span)> for AST {
    fn from(v: (QualifiedName, Span)) -> Self {
        let (q, span) = v;
        Self {
            span,
            expr: Box::new(Expression::Variable(q)),
        }
    }
}

impl From<(Box<[Self]>, Span)> for AST {
    fn from(p: (Box<[Self]>, Span)) -> Self {
        let (types, span) = p;
        Self {
            span,
            expr: Box::new(Expression::TupleType(types)),
        }
    }
}

impl From<((Box<[Self]>, Self), Span)> for AST {
    fn from(p: ((Box<[Self]>, Self), Span)) -> Self {
        let ((parameters, return_type), span) = p;
        Self {
            span,
            expr: Box::new(Expression::FunctionType {
                parameters,
                return_type,
            }),
        }
    }
}

impl From<(UnOp, Self, Span)> for AST {
    fn from(v: (UnOp, Self, Span)) -> Self {
        let (op, argument, span) = v;
        Self {
            span,
            expr: Box::new(Expression::UnaryOp { op, argument }),
        }
    }
}

impl From<(Self, BinOp, Self, Span)> for AST {
    fn from(v: (Self, BinOp, Self, Span)) -> Self {
        let (lhs, op, rhs, span) = v;
        Self {
            span,
            expr: Box::new(Expression::BinaryOp { lhs, op, rhs }),
        }
    }
}

impl From<((Self, Box<[Matcher]>), Span)> for AST {
    fn from(m: ((Self, Box<[Matcher]>), Span)) -> Self {
        let ((argument, matchers), span) = m;
        Self {
            span,
            expr: Box::new(Expression::Matching { argument, matchers }),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Type,

    BottomType,

    UnitType,

    PrimitiveType(PrimitiveType),
    Constant(Constant),

    TupleType(Box<[AST]>),
    Tuple(Box<[AST]>),
    Indexing {
        tuple: AST,
        index: usize,
    },

    FunctionType {
        parameters: Box<[AST]>,
        return_type: AST,
    },
    Closure {
        parameters: Box<[Name]>,
        body: AST,
    },
    TypeCall {
        type_callee: AST,
        type_arguments: Box<[AST]>,
    },
    Let {
        name: Name,
        typ: Option<AST>,
        value: AST,
        body: AST,
    },
    Call {
        callee: AST,
        type_arguments: Option<Box<[AST]>>,
        arguments: Box<[AST]>,
    },
    UnaryOp {
        op: UnOp,
        argument: AST,
    },
    BinaryOp {
        lhs: AST,
        op: BinOp,
        rhs: AST,
    },
    IfThenElse {
        condition: AST,
        then_branch: AST,
        else_branch: AST,
    },
    Matching {
        argument: AST,
        matchers: Box<[Matcher]>,
    },
    Access {
        argument: AST,
        name: Name,
    },

    /// Reference type (refcount-free), statically tracked by the borrow checker.
    ReferenceType(AST),

    /// Unique RC type.
    UniqueType(AST),

    Variable(QualifiedName),
}

impl Display for Expression {
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
