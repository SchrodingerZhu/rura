use std::fmt::{Display, Formatter, Write};
use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};

pub mod ast;
pub mod lir;

pub fn fmt_separated<T, P, I, W: Write>(f: &mut W, args: I, sep: P) -> std::fmt::Result
where
    T: Display,
    P: Display,
    I: ExactSizeIterator<Item = T>,
{
    let length = args.len();
    for (i, arg) in args.enumerate() {
        write!(f, "{arg}")?;
        if i + 1 < length {
            write!(f, "{sep}")?;
        }
    }
    Ok(())
}

pub fn fmt_delimited<T, P, I, W: Write>(
    f: &mut W,
    left: P,
    args: I,
    sep: P,
    right: P,
) -> std::fmt::Result
where
    T: Display,
    P: Display,
    I: ExactSizeIterator<Item = T>,
{
    write!(f, "{left}")?;
    fmt_separated(f, args, sep)?;
    write!(f, "{right}")
}

pub mod keywords {
    pub const TYPE: &str = "type";
    pub const UNIT: &str = "()";
    pub const BOTTOM: &str = "!";
    pub const I8: &str = "i8";
    pub const I16: &str = "i16";
    pub const I32: &str = "i32";
    pub const I64: &str = "i64";
    pub const ISIZE: &str = "isize";
    pub const I128: &str = "i128";
    pub const U8: &str = "u8";
    pub const U16: &str = "u16";
    pub const U32: &str = "u32";
    pub const U64: &str = "u64";
    pub const USIZE: &str = "usize";
    pub const U128: &str = "u128";
    pub const F32: &str = "f32";
    pub const F64: &str = "f64";
    pub const BOOL: &str = "bool";
    pub const CHAR: &str = "char";
    pub const STR: &str = "str";

    pub const FALSE: &str = "false";
    pub const TRUE: &str = "true";
}

#[derive(Clone, Debug)]
pub enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    U128(u128),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    Literal(String),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::I8(v) => v.fmt(f),
            Constant::I16(v) => v.fmt(f),
            Constant::I32(v) => v.fmt(f),
            Constant::I64(v) => v.fmt(f),
            Constant::ISize(v) => v.fmt(f),
            Constant::I128(v) => v.fmt(f),
            Constant::U8(v) => v.fmt(f),
            Constant::U16(v) => v.fmt(f),
            Constant::U32(v) => v.fmt(f),
            Constant::U64(v) => v.fmt(f),
            Constant::USize(v) => v.fmt(f),
            Constant::U128(v) => v.fmt(f),
            Constant::F32(v) => v.fmt(f),
            Constant::F64(v) => v.fmt(f),
            Constant::Bool(v) => v.fmt(f),
            Constant::Char(c) => write!(f, "'{c}'"),
            Constant::Literal(s) => write!(f, "\"{s}\""),
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I8(a), Self::I8(b)) => a == b,
            (Self::I16(a), Self::I16(b)) => a == b,
            (Self::I32(a), Self::I32(b)) => a == b,
            (Self::I64(a), Self::I64(b)) => a == b,
            (Self::ISize(a), Self::ISize(b)) => a == b,
            (Self::I128(a), Self::I128(b)) => a == b,
            (Self::U8(a), Self::U8(b)) => a == b,
            (Self::U16(a), Self::U16(b)) => a == b,
            (Self::U32(a), Self::U32(b)) => a == b,
            (Self::U64(a), Self::U64(b)) => a == b,
            (Self::USize(a), Self::USize(b)) => a == b,
            (Self::U128(a), Self::U128(b)) => a == b,
            (Self::F32(a), Self::F32(b)) => a.to_bits() == b.to_bits(),
            (Self::F64(a), Self::F64(b)) => a.to_bits() == b.to_bits(),
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::Literal(a), Self::Literal(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Constant {}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::I8(a) => a.hash(state),
            Self::I16(a) => a.hash(state),
            Self::I32(a) => a.hash(state),
            Self::I64(a) => a.hash(state),
            Self::ISize(a) => a.hash(state),
            Self::I128(a) => a.hash(state),
            Self::U8(a) => a.hash(state),
            Self::U16(a) => a.hash(state),
            Self::U32(a) => a.hash(state),
            Self::U64(a) => a.hash(state),
            Self::USize(a) => a.hash(state),
            Self::U128(a) => a.hash(state),
            Self::F32(a) => a.to_bits().hash(state),
            Self::F64(a) => a.to_bits().hash(state),
            Self::Bool(a) => a.hash(state),
            Self::Char(a) => a.hash(state),
            Self::Literal(a) => a.hash(state),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum PrimitiveType {
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
    Str,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            PrimitiveType::I8 => keywords::I8,
            PrimitiveType::I16 => keywords::I16,
            PrimitiveType::I32 => keywords::I32,
            PrimitiveType::I64 => keywords::I64,
            PrimitiveType::ISize => keywords::ISIZE,
            PrimitiveType::I128 => keywords::I128,
            PrimitiveType::U8 => keywords::U8,
            PrimitiveType::U16 => keywords::U16,
            PrimitiveType::U32 => keywords::U32,
            PrimitiveType::U64 => keywords::U64,
            PrimitiveType::USize => keywords::USIZE,
            PrimitiveType::U128 => keywords::U128,
            PrimitiveType::F32 => keywords::F32,
            PrimitiveType::F64 => keywords::F64,
            PrimitiveType::Bool => keywords::BOOL,
            PrimitiveType::Char => keywords::CHAR,
            PrimitiveType::Str => keywords::STR,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,
    Not,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constructor<Identifier, Type> {
    pub name: Identifier,
    pub params: Members<Identifier, Type>,
}

pub type Members<Identifier, Type> = Box<[(Member<Identifier>, Type)]>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Member<Identifier> {
    Named(Identifier),
    Index(usize),
}

impl<I> Member<I> {
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Named(_))
    }
}
