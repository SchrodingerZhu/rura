use std::fmt::{Display, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::ascii::{
    alpha1, alphanumeric1, dec_int, dec_uint, float, multispace1, till_line_ending,
};
use winnow::combinator::{alt, delimited, empty, fail, opt, preceded, repeat, separated};
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::token::{none_of, one_of, take_till, take_until, take_while};
use winnow::{dispatch, PResult, Parser};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Constructor<Identifier, Type> {
    pub name: Identifier,
    pub params: Members<Identifier, Type>,
}

type Members<Identifier, Type> = Box<[(Member<Identifier>, Type)]>;

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

pub fn expect(x: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(x))
}

pub fn eol_comment(i: &mut &str) -> PResult<()> {
    ("//", till_line_ending).void().parse_next(i)
}

pub fn multiline_comment(i: &mut &str) -> PResult<()> {
    ("/*", take_until(0.., "*/"), "*/").void().parse_next(i)
}

/// Parse whitespaces within a line.
fn lws1(input: &mut &str) -> PResult<()> {
    take_while(1.., (' ', '\t', '\r')).void().parse_next(input)
}

fn lws_or_multiline_comment(i: &mut &str) -> PResult<()> {
    repeat(0.., alt((lws1, multiline_comment))).parse_next(i)
}

pub fn elidable<'a, F, O>(end: F) -> impl Parser<&'a str, (), ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    (
        lws_or_multiline_comment,
        alt(("\n".void(), end.void(), eol_comment)),
    )
        .void()
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

pub fn opt_or_default<'a, F, O>(f: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
    O: Default,
{
    opt(f).map(|o| o.unwrap_or_default())
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

pub fn primitive_type(i: &mut &str) -> PResult<PrimitiveType> {
    let mut dispatch = dispatch! {
        alphanumeric1;
        keywords::I8 => empty.value(PrimitiveType::I8),
        keywords::I16 => empty.value(PrimitiveType::I16),
        keywords::I32 => empty.value(PrimitiveType::I32),
        keywords::I64 => empty.value(PrimitiveType::I64),
        keywords::ISIZE => empty.value(PrimitiveType::ISize),
        keywords::I128 => empty.value(PrimitiveType::I128),
        keywords::U8 => empty.value(PrimitiveType::U8),
        keywords::U16 => empty.value(PrimitiveType::U16),
        keywords::U32 => empty.value(PrimitiveType::U32),
        keywords::U64 => empty.value(PrimitiveType::U64),
        keywords::USIZE => empty.value(PrimitiveType::USize),
        keywords::U128 => empty.value(PrimitiveType::U128),
        keywords::F32 => empty.value(PrimitiveType::F32),
        keywords::F64 => empty.value(PrimitiveType::F64),
        keywords::BOOL => empty.value(PrimitiveType::Bool),
        keywords::CHAR => empty.value(PrimitiveType::Char),
        keywords::STR => empty.value(PrimitiveType::Str),
        _ => fail,
    };
    dispatch.parse_next(i)
}

pub fn boolean(i: &mut &str) -> PResult<Constant> {
    alpha1
        .try_map(|s: &str| s.parse::<bool>())
        .map(Constant::Bool)
        .parse_next(i)
}

macro_rules! number {
    (float $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<Constant> {
            float.map(|n| Constant::$ctor(n)).parse_next(i)
        }
    };
    (signed $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<Constant> {
            dec_int.map(|n| Constant::$ctor(n)).parse_next(i)
        }
    };
    (unsigned $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<Constant> {
            dec_uint.map(|n| Constant::$ctor(n)).parse_next(i)
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

pub fn character(input: &mut &str) -> PResult<char> {
    delimited('\'', alt((plain_char, escape)), '\'')
        .context(expect("character"))
        .parse_next(input)
}

fn plain_char(input: &mut &str) -> PResult<char> {
    none_of(['\'', '\\']).parse_next(input)
}

fn escape(input: &mut &str) -> PResult<char> {
    preceded(
        '\\',
        alt((
            unicode,
            'n'.value('\n'),
            'r'.value('\r'),
            't'.value('\t'),
            'b'.value('\u{08}'),
            'f'.value('\u{0C}'),
            '\\'.value('\\'),
            '/'.value('/'),
            '"'.value('"'),
            '\''.value('\''),
        )),
    )
    .parse_next(input)
}

enum StringFragment<'a> {
    Literal(&'a str),
    Escape(char),
}

fn string_literal<'a>(input: &mut &'a str) -> PResult<&'a str> {
    take_till(1.., ['"', '\\'])
        .verify(|s: &str| !s.is_empty())
        .parse_next(input)
}

fn unicode(input: &mut &str) -> PResult<char> {
    let parse_hex = take_while(1..=6, |c: char| c.is_ascii_hexdigit());
    let parse_delimited_hex = preceded('u', delimited('{', parse_hex, '}'));
    let parse_u32 = parse_delimited_hex.try_map(move |hex| u32::from_str_radix(hex, 16));
    parse_u32.verify_map(char::from_u32).parse_next(input)
}

pub fn string(input: &mut &str) -> PResult<String> {
    let build_string = repeat(
        0..,
        alt((
            string_literal.map(StringFragment::Literal),
            escape.map(StringFragment::Escape),
        )),
    )
    .fold(String::new, |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(s),
            StringFragment::Escape(c) => string.push(c),
        }
        string
    });
    delimited('"', build_string, '"')
        .context(expect("string"))
        .parse_next(input)
}

pub fn members<'a, F, I, T>(typ: F) -> impl Parser<&'a str, Members<I, T>, ContextError>
where
    F: Copy + Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
{
    opt(alt((named_members(typ), unnamed_members(typ))))
        .map(|x| x.unwrap_or_default())
        .context(expect("members"))
}

fn named_members<'a, F, I, T>(typ: F) -> impl Parser<&'a str, Members<I, T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
{
    let field = (identifier::<I>, skip_space(":"), typ).map(|(n, _, t)| (Member::Named(n), t));
    delimited("{", separated(1.., skip_space(field), ","), "}")
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("named members"))
}

fn unnamed_members<'a, F, I, T>(typ: F) -> impl Parser<&'a str, Members<I, T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
{
    let ty = separated(1.., skip_space(typ), ",").map(|x: Vec<_>| {
        x.into_iter()
            .enumerate()
            .map(|(idx, ty)| (Member::<I>::Index(idx), ty))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    delimited("(", ty, ")").context(expect("unnamed members"))
}

pub fn type_parameters<'a, T>(i: &mut &'a str) -> PResult<Box<[T]>>
where
    T: From<&'a str>,
{
    delimited("<", separated(1.., skip_space(identifier::<T>), ","), ">")
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("type parameters"))
        .parse_next(i)
}

pub fn type_arguments<'a, F, T>(typ: F) -> impl Parser<&'a str, Box<[T]>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
{
    delimited("<", separated(1.., skip_space(typ), ","), ">")
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("type arguments"))
}

pub fn tuple_type<'a, F, T, TupleT>(typ: F) -> impl Parser<&'a str, TupleT, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    TupleT: From<Box<[T]>>,
{
    delimited("(", separated(1.., skip_space(typ), ","), ")")
        .map(|types: Vec<_>| From::from(types.into_boxed_slice()))
        .context(expect("tuple type"))
}

pub fn function_type<'a, F, T, FuncT>(typ: F) -> impl Parser<&'a str, FuncT, ContextError>
where
    F: Copy + Parser<&'a str, T, ContextError>,
    FuncT: From<(Box<[T]>, Box<T>)>,
{
    (
        "fn",
        skip_space(delimited("(", separated(0.., skip_space(typ), ","), ")"))
            .map(|p: Vec<_>| p.into_boxed_slice()),
        "->",
        skip_space(typ).map(Box::new),
    )
        .map(|(_, params, _, ret)| From::from((params, ret)))
        .context(expect("function type"))
}

fn prefixed_type<'a, P, O, F, T>(prefix: P, typ: F) -> impl Parser<&'a str, Box<T>, ContextError>
where
    P: Parser<&'a str, O, ContextError>,
    F: Parser<&'a str, T, ContextError>,
{
    (prefix, skip_space(typ)).map(|p| Box::new(p.1))
}

pub fn reference_type<'a, F, T>(typ: F) -> impl Parser<&'a str, Box<T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
{
    prefixed_type("&", typ).context(expect("reference type"))
}

pub fn unique_type<'a, F, T>(typ: F) -> impl Parser<&'a str, Box<T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
{
    prefixed_type("!", typ).context(expect("unique type"))
}

#[cfg(test)]
mod tests;
