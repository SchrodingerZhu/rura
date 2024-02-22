use std::fmt::{Display, Formatter};
use std::hash::Hash;

use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::ascii::{
    alpha1, alphanumeric1, dec_int, dec_uint, float, multispace1, till_line_ending,
};
use winnow::combinator::{alt, delimited, empty, fail, opt, repeat, separated};
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::token::{one_of, take_until, take_while};
use winnow::{dispatch, PResult, Parser};

pub mod keywords {
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

    pub const FALSE: &str = "false";
    pub const TRUE: &str = "true";
}

#[derive(Clone, Debug)]
pub enum ScalarConstant {
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
}

impl PartialEq for ScalarConstant {
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
            _ => false,
        }
    }
}

impl Eq for ScalarConstant {}

impl Hash for ScalarConstant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ScalarType {
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
}

impl Display for ScalarType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ScalarType::I8 => keywords::I8,
            ScalarType::I16 => keywords::I16,
            ScalarType::I32 => keywords::I32,
            ScalarType::I64 => keywords::I64,
            ScalarType::ISize => keywords::ISIZE,
            ScalarType::I128 => keywords::I128,
            ScalarType::U8 => keywords::U8,
            ScalarType::U16 => keywords::U16,
            ScalarType::U32 => keywords::U32,
            ScalarType::U64 => keywords::U64,
            ScalarType::USize => keywords::USIZE,
            ScalarType::U128 => keywords::U128,
            ScalarType::F32 => keywords::F32,
            ScalarType::F64 => keywords::F64,
            ScalarType::Bool => keywords::BOOL,
            ScalarType::Char => keywords::CHAR,
        })
    }
}

pub fn expect(x: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(x))
}

pub fn eol_comment(i: &mut &str) -> PResult<()> {
    ("//", till_line_ending).void().parse_next(i)
}

pub fn multiline_comment(i: &mut &str) -> PResult<()> {
    ("/*", take_until(0.., "*/"), "*/").void().parse_next(i)
}

pub fn ws_or_comment(i: &mut &str) -> PResult<()> {
    repeat(
        0..,
        alt((multispace1.void(), eol_comment, multiline_comment)),
    )
    .parse_next(i)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading
/// and trailing whitespace, returning the output of `inner`.
pub fn skip_space<'a, F, O>(inner: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    delimited(ws_or_comment, inner, ws_or_comment)
}

pub fn identifier<'a, T>(i: &mut &'a str) -> PResult<T>
where
    T: From<&'a str>,
{
    (one_of(is_xid_start), take_while(0.., is_xid_continue))
        .recognize()
        .map(|s| T::from(s))
        .context(expect("identifier"))
        .parse_next(i)
}

pub fn qualified_name<'a, Identifier, Qualified>(input: &mut &'a str) -> PResult<Qualified>
where
    Identifier: From<&'a str>,
    Qualified: From<Box<[Identifier]>>,
{
    separated(1.., identifier::<Identifier>, "::")
        .map(|idents: Vec<_>| Qualified::from(idents.into_boxed_slice()))
        .context(expect("qualified name"))
        .parse_next(input)
}

pub fn optional_type_parameters<'a, T>(i: &mut &'a str) -> PResult<Box<[T]>>
where
    T: From<&'a str>,
{
    opt(("<", separated(1.., skip_space(identifier::<T>), ","), ">"))
        .context(expect("type parameters"))
        .map(|x| Vec::into_boxed_slice(x.unwrap_or_default().1))
        .parse_next(i)
}

pub fn scalar_type(i: &mut &str) -> PResult<ScalarType> {
    let mut dispatch = dispatch! {
        alphanumeric1;
        keywords::I8 => empty.value(ScalarType::I8),
        keywords::I16 => empty.value(ScalarType::I16),
        keywords::I32 => empty.value(ScalarType::I32),
        keywords::I64 => empty.value(ScalarType::I64),
        keywords::ISIZE => empty.value(ScalarType::ISize),
        keywords::I128 => empty.value(ScalarType::I128),
        keywords::U8 => empty.value(ScalarType::U8),
        keywords::U16 => empty.value(ScalarType::U16),
        keywords::U32 => empty.value(ScalarType::U32),
        keywords::U64 => empty.value(ScalarType::U64),
        keywords::USIZE => empty.value(ScalarType::USize),
        keywords::U128 => empty.value(ScalarType::U128),
        keywords::F32 => empty.value(ScalarType::F32),
        keywords::F64 => empty.value(ScalarType::F64),
        keywords::BOOL => empty.value(ScalarType::Bool),
        keywords::CHAR => empty.value(ScalarType::Char),
        _ => fail,
    };
    dispatch.parse_next(i)
}

// TODO: Parse chars.

pub fn boolean(i: &mut &str) -> PResult<ScalarConstant> {
    alpha1
        .try_map(|s: &str| s.parse::<bool>())
        .map(ScalarConstant::Bool)
        .parse_next(i)
}

macro_rules! number {
    (float $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            float.map(|n| ScalarConstant::$ctor(n)).parse_next(i)
        }
    };
    (signed $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            dec_int.map(|n| ScalarConstant::$ctor(n)).parse_next(i)
        }
    };
    (unsigned $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            dec_uint.map(|n| ScalarConstant::$ctor(n)).parse_next(i)
        }
    };
}

number!(float f32 F32);
number!(float f64 F64);
number!(signed i8 I8);
number!(signed i16 I16);
number!(signed i32 I32);
number!(signed i64 I64);
number!(signed i128 I128);
number!(signed isize ISize);
number!(unsigned u8 U8);
number!(unsigned u16 U16);
number!(unsigned u32 U32);
number!(unsigned u64 U64);
number!(unsigned u128 U128);
number!(unsigned usize USize);
