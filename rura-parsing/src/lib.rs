use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::ascii::{
    alpha1, alphanumeric1, dec_int, dec_uint, float, multispace1, till_line_ending,
};
use winnow::combinator::{alt, delimited, empty, fail, opt, preceded, repeat, separated};
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::token::{none_of, one_of, take_till, take_until, take_while};
use winnow::{dispatch, PResult, Parser};

use rura_core::{keywords, BinOp, Constant, Constructor, Member, Members, PrimitiveType, UnOp};

pub fn expect(x: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(x))
}

pub fn opt_or_default<'a, F, O>(f: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
    O: Default,
{
    opt(f).map(|o| o.unwrap_or_default())
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

pub fn elidable_block<'a, F, T>(
    val: F,
    terminator: &'static str,
) -> impl Parser<&'a str, T, ContextError>
where
    F: Copy + Parser<&'a str, T, ContextError>,
{
    skip_space(alt((braced(val), suffixed(val, elidable(terminator)))))
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

pub fn parenthesized<'a, F, O>(f: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    delimited("(", skip_space(f), ")")
}

pub fn braced<'a, F, O>(f: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    delimited("{", skip_space(f), "}")
}

pub fn bracketed<'a, F, O>(f: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    delimited("<", skip_space(f), ">")
}

pub fn piped<'a, F, O>(f: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    delimited("|", skip_space(f), "|")
}

pub fn prefixed<'a, P, O, F, T>(prefix: P, f: F) -> impl Parser<&'a str, T, ContextError>
where
    P: Parser<&'a str, O, ContextError>,
    F: Parser<&'a str, T, ContextError>,
{
    (prefix, skip_space(f)).map(|p| p.1)
}

pub fn suffixed<'a, S, O, F, T>(f: F, suffix: S) -> impl Parser<&'a str, T, ContextError>
where
    S: Parser<&'a str, O, ContextError>,
    F: Parser<&'a str, T, ContextError>,
{
    (f, skip_space(suffix)).map(|p| p.0)
}

pub fn infixed<'a, I, O, F, T>(
    lhs: F,
    infix: I,
    rhs: F,
) -> impl Parser<&'a str, (T, T), ContextError>
where
    I: Parser<&'a str, O, ContextError>,
    F: Parser<&'a str, T, ContextError>,
{
    (lhs, skip_space(infix), rhs).map(|(l, _, r)| (l, r))
}

pub fn identifier<'a, T>(i: &mut &'a str) -> PResult<T>
where
    T: From<&'a str>,
{
    (one_of(is_xid_start), take_while(0.., is_xid_continue))
        .recognize()
        .map(T::from)
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
            float.map(Constant::$ctor).parse_next(i)
        }
    };
    (signed $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<Constant> {
            dec_int.map(Constant::$ctor).parse_next(i)
        }
    };
    (unsigned $name:ident $ctor:ident) => {
        pub fn $name(i: &mut &str) -> PResult<Constant> {
            dec_uint.map(Constant::$ctor).parse_next(i)
        }
    };
}

number!(float number_f32 F32);
number!(float number_f64 F64);
number!(signed number_i8 I8);
number!(signed number_i16 I16);
number!(signed number_i32 I32);
number!(signed number_i64 I64);
number!(signed number_i128 I128);
number!(signed number_isize ISize);
number!(unsigned number_u8 U8);
number!(unsigned number_u16 U16);
number!(unsigned number_u32 U32);
number!(unsigned number_u64 U64);
number!(unsigned number_u128 U128);
number!(unsigned number_usize USize);

pub fn character(input: &mut &str) -> PResult<Constant> {
    delimited('\'', alt((plain_char, escape)), '\'')
        .map(Constant::Char)
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

pub fn string(input: &mut &str) -> PResult<Constant> {
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
        .map(Constant::Literal)
        .context(expect("string"))
        .parse_next(input)
}

pub fn constant(i: &mut &str) -> PResult<Constant> {
    alt((
        character,
        boolean,
        number_f32,
        number_f64,
        number_i8,
        number_i16,
        number_i32,
        number_i64,
        number_i128,
        number_isize,
        number_u8,
        number_u16,
        number_u32,
        number_u64,
        number_u128,
        number_usize,
        string,
    ))
    .parse_next(i)
}

pub fn prefix_op<'a, F, V, T>(
    prefix: &'static str,
    op: UnOp,
    val: F,
) -> impl Parser<&'a str, T, ContextError>
where
    F: Parser<&'a str, V, ContextError>,
    T: From<(UnOp, Box<V>)>,
{
    prefixed(prefix, val.map(Box::new))
        .map(move |v| From::from((op, v)))
        .context(expect("prefix operation"))
}

pub fn unary<'a, F, V, T>(val: F) -> impl Parser<&'a str, T, ContextError>
where
    F: Copy + Parser<&'a str, V, ContextError>,
    T: From<(UnOp, Box<V>)>,
{
    alt((
        prefix_op("-", UnOp::Neg, val),
        prefix_op("!", UnOp::Not, val),
    ))
}

pub fn infix_op<'a, F, V, T>(
    infix: &'static str,
    op: BinOp,
    val: F,
) -> impl Parser<&'a str, T, ContextError>
where
    F: Copy + Parser<&'a str, V, ContextError>,
    T: From<(Box<V>, BinOp, Box<V>)>,
{
    infixed(val.map(Box::new), infix, val.map(Box::new))
        .map(move |(lhs, rhs)| From::from((lhs, op, rhs)))
        .context(expect("infix operation"))
}

pub fn binary<'a, F, V, T>(val: F) -> impl Parser<&'a str, T, ContextError>
where
    F: Copy + Parser<&'a str, V, ContextError>,
    T: From<(Box<V>, BinOp, Box<V>)>,
{
    alt((
        infix_op(">>", BinOp::Shr, val),
        infix_op("<<", BinOp::Shl, val),
        infix_op("*", BinOp::Mul, val),
        infix_op("/", BinOp::Div, val),
        infix_op("+", BinOp::Add, val),
        infix_op("-", BinOp::Sub, val),
        infix_op("%", BinOp::Rem, val),
        infix_op("==", BinOp::Eq, val),
        infix_op("!=", BinOp::Ne, val),
        infix_op("<", BinOp::Lt, val),
        infix_op("<=", BinOp::Le, val),
        infix_op(">", BinOp::Gt, val),
        infix_op(">=", BinOp::Ge, val),
        infix_op("&&", BinOp::And, val),
        infix_op("||", BinOp::Or, val),
    ))
}

pub fn field<'a, F, I, T>(typ: F) -> impl Parser<&'a str, (I, T), ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
{
    (identifier::<I>, skip_space(":"), typ).map(|(n, _, t)| (n, t))
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
    braced(separated(
        1..,
        skip_space(field(typ).map(|(n, t)| (Member::Named(n), t))),
        ",",
    ))
    .map(|v: Vec<_>| v.into_boxed_slice())
    .context(expect("named members"))
}

fn unnamed_members<'a, F, I, T>(typ: F) -> impl Parser<&'a str, Members<I, T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
{
    parenthesized(separated(1.., skip_space(typ), ",").map(|x: Vec<_>| {
        x.into_iter()
            .enumerate()
            .map(|(idx, ty)| (Member::Index(idx), ty))
            .collect()
    }))
    .context(expect("unnamed members"))
}

pub fn constructor<'a, F, I, T>(typ: F) -> impl Parser<&'a str, Constructor<I, T>, ContextError>
where
    F: Copy + Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
{
    (identifier, skip_space(members(typ)))
        .map(|(name, params)| Constructor { name, params })
        .context(expect("constructor"))
}

pub fn function_parameters<'a, F, I, T, P>(typ: F) -> impl Parser<&'a str, Box<[P]>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    I: From<&'a str>,
    P: From<(I, T)>,
{
    parenthesized(separated(0.., field(typ), ","))
        .map(|p: Vec<_>| p.into_iter().map(From::from).collect())
}

pub fn constructor_parameters<'a, I>() -> impl Parser<&'a str, Box<[I]>, ContextError>
where
    I: From<&'a str>,
{
    parenthesized(separated(1.., identifier::<I>, ",")).map(|v: Vec<_>| v.into_boxed_slice())
}

pub fn closure_parameters<'a, I>(i: &mut &'a str) -> PResult<Box<[I]>>
where
    I: From<&'a str>,
{
    piped(separated(0.., identifier, ","))
        .map(|p: Vec<_>| p.into_boxed_slice())
        .parse_next(i)
}

pub fn type_parameters<'a, T>(i: &mut &'a str) -> PResult<Box<[T]>>
where
    T: From<&'a str>,
{
    bracketed(separated(1.., skip_space(identifier::<T>), ","))
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("type parameters"))
        .parse_next(i)
}

pub fn type_arguments<'a, F, T>(typ: F) -> impl Parser<&'a str, Box<[T]>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
{
    bracketed(separated(1.., skip_space(typ), ","))
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("type arguments"))
}

pub fn tuple<'a, F, T>(val: F) -> impl Parser<&'a str, Box<[T]>, ContextError>
where
    F: Copy + Parser<&'a str, T, ContextError>,
{
    parenthesized(alt((
        separated(2.., skip_space(val), ","),
        suffixed(val, ",").map(|v| vec![v]),
    )))
    .map(|v| v.into_boxed_slice())
    .context(expect("tuple"))
}

pub fn tuple_type<'a, F, T, TupleT>(typ: F) -> impl Parser<&'a str, TupleT, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
    TupleT: From<Box<[T]>>,
{
    parenthesized(separated(1.., skip_space(typ), ","))
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
        skip_space(parenthesized(separated(0.., skip_space(typ), ",")))
            .map(|p: Vec<_>| p.into_boxed_slice()),
        "->",
        skip_space(typ).map(Box::new),
    )
        .map(|(_, params, _, ret)| From::from((params, ret)))
        .context(expect("function type"))
}

pub fn reference_type<'a, F, T>(typ: F) -> impl Parser<&'a str, Box<T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
{
    prefixed("&", typ)
        .map(Box::new)
        .context(expect("reference type"))
}

pub fn unique_type<'a, F, T>(typ: F) -> impl Parser<&'a str, Box<T>, ContextError>
where
    F: Parser<&'a str, T, ContextError>,
{
    prefixed("!", typ)
        .map(Box::new)
        .context(expect("unique type"))
}

#[cfg(test)]
mod tests;
