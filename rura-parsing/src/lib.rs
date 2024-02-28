use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::ascii::{
    alpha1, alphanumeric1, dec_int, dec_uint, float, multispace1, till_line_ending,
};
use winnow::combinator::{alt, delimited, empty, fail, opt, preceded, repeat, separated};
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::token::{none_of, one_of, take_till, take_until, take_while};
use winnow::{dispatch, Located, PResult, Parser};

use rura_core::{
    keywords, BinOp, Constant, Constructor, Input, Member, Members, PrimitiveType, Span, UnOp,
};

pub fn expect(x: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(x))
}

pub fn opt_or_default<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    O: Default,
    F: Parser<Input<'a>, O, ContextError>,
{
    opt(f).map(|o| o.unwrap_or_default())
}

pub fn eol_comment(i: &mut Input) -> PResult<()> {
    ("//", till_line_ending).void().parse_next(i)
}

pub fn multiline_comment(i: &mut Input) -> PResult<()> {
    ("/*", take_until(0.., "*/"), "*/").void().parse_next(i)
}

/// Parse whitespaces within a line.
fn lws1(i: &mut Input) -> PResult<()> {
    take_while(1.., (' ', '\t', '\r')).void().parse_next(i)
}

fn lws_or_multiline_comment(i: &mut Input) -> PResult<()> {
    repeat(0.., alt((lws1, multiline_comment))).parse_next(i)
}

pub fn ws_or_comment(i: &mut Input) -> PResult<()> {
    repeat(
        0..,
        alt((multispace1.void(), eol_comment, multiline_comment)),
    )
    .parse_next(i)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading
/// and trailing whitespace, returning the output of `inner`.
pub fn skip_space<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    delimited(ws_or_comment, f, ws_or_comment)
}

pub fn skip_lws<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    delimited(ws_or_comment, f, lws_or_multiline_comment)
}

pub fn elidable<'a, O, F>(end: F) -> impl Parser<Input<'a>, (), ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    (
        lws_or_multiline_comment,
        alt(("\n".void(), end.void(), eol_comment)),
    )
        .void()
}

pub fn elidable_block<'a, O, O2, F, G>(
    f: F,
    terminator: G,
) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Copy + Parser<Input<'a>, O, ContextError>,
    G: Parser<Input<'a>, O2, ContextError>,
{
    alt((braced(f), elided_block(f, terminator)))
}

pub fn elided_block<'a, O, O2, F, G>(f: F, terminator: G) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
    G: Parser<Input<'a>, O2, ContextError>,
{
    (skip_lws(f), elidable(terminator)).map(|p| p.0)
}

pub fn parenthesized<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    delimited("(", skip_space(f), ")")
}

pub fn braced<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    delimited("{", skip_space(f), "}")
}

pub fn bracketed<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    delimited("<", skip_space(f), ">")
}

pub fn piped<'a, O, F>(f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    delimited("|", skip_space(f), "|")
}

pub fn prefixed<'a, O, O2, P, F>(prefix: P, f: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
    P: Parser<Input<'a>, O2, ContextError>,
{
    (prefix, skip_space(f)).map(|p| p.1)
}

pub fn suffixed<'a, O, O2, F, S>(f: F, suffix: S) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
    S: Parser<Input<'a>, O2, ContextError>,
{
    (skip_space(f), suffix).map(|p| p.0)
}

pub fn infixed<'a, O, O2, L, R, II>(
    lhs: L,
    infix: II,
    rhs: R,
) -> impl Parser<Input<'a>, (O, O), ContextError>
where
    L: Parser<Input<'a>, O, ContextError>,
    R: Parser<Input<'a>, O, ContextError>,
    II: Parser<Input<'a>, O2, ContextError>,
{
    (lhs, skip_space(infix), rhs).map(|(l, _, r)| (l, r))
}

pub fn identifier<'a, ID>(i: &mut Input<'a>) -> PResult<ID>
where
    ID: From<Input<'a>>,
{
    (one_of(is_xid_start), take_while(0.., is_xid_continue))
        .recognize()
        .map(|s| From::from(Located::new(s)))
        .context(expect("identifier"))
        .parse_next(i)
}

pub fn qualified_name<'a, ID, Q>(i: &mut Input<'a>) -> PResult<Q>
where
    ID: From<Input<'a>>,
    Q: From<Box<[ID]>>,
{
    separated(1.., identifier, "::")
        .map(|idents: Vec<_>| Q::from(idents.into_boxed_slice()))
        .context(expect("qualified name"))
        .parse_next(i)
}

pub fn primitive_type(i: &mut Input) -> PResult<PrimitiveType> {
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

pub fn boolean(i: &mut Input) -> PResult<Constant> {
    alpha1
        .try_map(|s: &str| s.parse::<bool>())
        .map(Constant::Bool)
        .parse_next(i)
}

macro_rules! number {
    (float $name:ident $ctor:ident) => {
        pub fn $name(i: &mut Input) -> PResult<Constant> {
            float.map(Constant::$ctor).parse_next(i)
        }
    };
    (signed $name:ident $ctor:ident) => {
        pub fn $name(i: &mut Input) -> PResult<Constant> {
            dec_int.map(Constant::$ctor).parse_next(i)
        }
    };
    (unsigned $name:ident $ctor:ident) => {
        pub fn $name(i: &mut Input) -> PResult<Constant> {
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

pub fn character(i: &mut Input) -> PResult<Constant> {
    delimited('\'', alt((plain_char, escape)), '\'')
        .map(Constant::Char)
        .context(expect("character"))
        .parse_next(i)
}

fn plain_char(i: &mut Input) -> PResult<char> {
    none_of(['\'', '\\']).parse_next(i)
}

fn escape(i: &mut Input) -> PResult<char> {
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
    .parse_next(i)
}

enum StringFragment<'a> {
    Literal(&'a str),
    Escape(char),
}

fn string_literal<'a>(i: &mut Input<'a>) -> PResult<&'a str> {
    take_till(1.., ['"', '\\'])
        .verify(|s: &str| !s.is_empty())
        .parse_next(i)
}

fn unicode(i: &mut Input) -> PResult<char> {
    let parse_hex = take_while(1..=6, |c: char| c.is_ascii_hexdigit());
    let parse_delimited_hex = preceded('u', delimited('{', parse_hex, '}'));
    let parse_u32 = parse_delimited_hex.try_map(move |hex| u32::from_str_radix(hex, 16));
    parse_u32.verify_map(char::from_u32).parse_next(i)
}

pub fn string(i: &mut Input) -> PResult<Constant> {
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
        .parse_next(i)
}

pub fn constant(i: &mut Input) -> PResult<Constant> {
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

pub fn prefix_op<'a, O, O2, O3, P, F>(
    prefix: P,
    op: UnOp,
    val: F,
) -> impl Parser<Input<'a>, O3, ContextError>
where
    O3: From<(UnOp, O, Span)>,
    F: Parser<Input<'a>, O, ContextError>,
    P: Parser<Input<'a>, O2, ContextError>,
{
    prefixed(prefix, val)
        .with_span()
        .map(move |(v, span)| From::from((op, v, span)))
        .context(expect("prefix operation"))
}

pub fn unary<'a, O, O2, F>(val: F) -> impl Parser<Input<'a>, O2, ContextError>
where
    O2: From<(UnOp, O, Span)>,
    F: Copy + Parser<Input<'a>, O, ContextError>,
{
    alt((
        prefix_op("-", UnOp::Neg, val),
        prefix_op("!", UnOp::Not, val),
    ))
}

pub fn infix_op<'a, O, O2, O3, II, L, R>(
    lhs: L,
    infix: II,
    op: BinOp,
    rhs: R,
) -> impl Parser<Input<'a>, O3, ContextError>
where
    O3: From<(O, BinOp, O, Span)>,
    L: Copy + Parser<Input<'a>, O, ContextError>,
    R: Copy + Parser<Input<'a>, O, ContextError>,
    II: Parser<Input<'a>, O2, ContextError>,
{
    infixed(lhs, infix, rhs)
        .with_span()
        .map(move |((lhs, rhs), span)| From::from((lhs, op, rhs, span)))
        .context(expect("infix operation"))
}

pub fn binary<'a, O, O2, L, R>(lhs: L, rhs: R) -> impl Parser<Input<'a>, O2, ContextError>
where
    O2: From<(O, BinOp, O, Span)>,
    L: Copy + Parser<Input<'a>, O, ContextError>,
    R: Copy + Parser<Input<'a>, O, ContextError>,
{
    alt((
        infix_op(lhs, ">>", BinOp::Shr, rhs),
        infix_op(lhs, "<<", BinOp::Shl, rhs),
        infix_op(lhs, "*", BinOp::Mul, rhs),
        infix_op(lhs, "/", BinOp::Div, rhs),
        infix_op(lhs, "+", BinOp::Add, rhs),
        infix_op(lhs, "-", BinOp::Sub, rhs),
        infix_op(lhs, "%", BinOp::Rem, rhs),
        infix_op(lhs, "==", BinOp::Eq, rhs),
        infix_op(lhs, "!=", BinOp::Ne, rhs),
        infix_op(lhs, "<", BinOp::Lt, rhs),
        infix_op(lhs, "<=", BinOp::Le, rhs),
        infix_op(lhs, ">", BinOp::Gt, rhs),
        infix_op(lhs, ">=", BinOp::Ge, rhs),
        infix_op(lhs, "&&", BinOp::And, rhs),
        infix_op(lhs, "||", BinOp::Or, rhs),
    ))
}

pub fn field<'a, O, F, ID>(typ: F) -> impl Parser<Input<'a>, (ID, O), ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
    ID: From<Input<'a>>,
{
    (identifier, skip_space(":"), typ).map(|(n, _, t)| (n, t))
}

pub fn members<'a, O, F, ID>(typ: F) -> impl Parser<Input<'a>, Members<ID, O>, ContextError>
where
    F: Copy + Parser<Input<'a>, O, ContextError>,
    ID: From<Input<'a>>,
{
    opt(alt((named_members(typ), unnamed_members(typ))))
        .map(|x| x.unwrap_or_default())
        .context(expect("members"))
}

fn named_members<'a, O, F, ID>(typ: F) -> impl Parser<Input<'a>, Members<ID, O>, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
    ID: From<Input<'a>>,
{
    braced(separated(
        1..,
        skip_space(field(typ).map(|(n, t)| (Member::Named(n), t))),
        ",",
    ))
    .map(|v: Vec<_>| v.into_boxed_slice())
    .context(expect("named members"))
}

fn unnamed_members<'a, O, F, ID>(typ: F) -> impl Parser<Input<'a>, Members<ID, O>, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
    ID: From<Input<'a>>,
{
    parenthesized(separated(1.., skip_space(typ), ",").map(|x: Vec<_>| {
        x.into_iter()
            .enumerate()
            .map(|(idx, ty)| (Member::Index(idx), ty))
            .collect()
    }))
    .context(expect("unnamed members"))
}

pub fn constructor<'a, O, F, ID>(typ: F) -> impl Parser<Input<'a>, Constructor<ID, O>, ContextError>
where
    F: Copy + Parser<Input<'a>, O, ContextError>,
    ID: From<Input<'a>>,
{
    (skip_space(identifier), members(typ))
        .with_span()
        .map(|((name, params), span)| Constructor { span, name, params })
        .context(expect("constructor"))
}

pub fn function_parameters<'a, O, O2, ID, F>(
    typ: F,
) -> impl Parser<Input<'a>, Box<[O2]>, ContextError>
where
    ID: From<Input<'a>>,
    O2: From<(ID, O)>,
    F: Parser<Input<'a>, O, ContextError>,
{
    parenthesized(separated(0.., field(typ), ","))
        .map(|p: Vec<_>| p.into_iter().map(From::from).collect())
}

pub fn constructor_parameters<'a, ID>(i: &mut Input<'a>) -> PResult<Box<[ID]>>
where
    ID: From<Input<'a>>,
{
    parenthesized(separated(1.., skip_space(identifier), ","))
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("constructor parameters"))
        .parse_next(i)
}

pub fn closure_parameters<'a, ID>(i: &mut Input<'a>) -> PResult<Box<[ID]>>
where
    ID: From<Input<'a>>,
{
    piped(separated(0.., skip_space(identifier), ","))
        .map(|p: Vec<_>| p.into_boxed_slice())
        .context(expect("closure parameters"))
        .parse_next(i)
}

pub fn type_parameters<'a, ID>(i: &mut Input<'a>) -> PResult<Box<[(ID, Span)]>>
where
    ID: From<Input<'a>>,
{
    bracketed(separated(1.., skip_space(identifier).with_span(), ","))
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("type parameters"))
        .parse_next(i)
}

pub fn type_arguments<'a, O, F>(typ: F) -> impl Parser<Input<'a>, Box<[O]>, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    bracketed(separated(1.., skip_space(typ), ","))
        .map(|v: Vec<_>| v.into_boxed_slice())
        .context(expect("type arguments"))
}

pub fn tuple<'a, O, F>(val: F) -> impl Parser<Input<'a>, Box<[O]>, ContextError>
where
    F: Copy + Parser<Input<'a>, O, ContextError>,
{
    parenthesized(alt((
        separated(2.., skip_space(val), ","),
        suffixed(val, ",").map(|v| vec![v]),
    )))
    .map(|v| v.into_boxed_slice())
    .context(expect("tuple"))
}

pub fn tuple_type<'a, O, F>(typ: F) -> impl Parser<Input<'a>, Box<[O]>, ContextError>
where
    F: Copy + Parser<Input<'a>, O, ContextError>,
{
    parenthesized(alt((
        separated(2.., skip_space(typ), ","),
        suffixed(typ, ",").map(|v| vec![v]),
    )))
    .map(|v: Vec<_>| v.into_boxed_slice())
    .context(expect("tuple type"))
}

pub fn function_type<'a, O, F>(typ: F) -> impl Parser<Input<'a>, (Box<[O]>, O), ContextError>
where
    F: Copy + Parser<Input<'a>, O, ContextError>,
{
    (
        "fn",
        skip_space(parenthesized(separated(0.., skip_space(typ), ",")))
            .map(|p: Vec<_>| p.into_boxed_slice()),
        "->",
        skip_space(typ),
    )
        .map(|(_, params, _, ret)| (params, ret))
        .context(expect("function type"))
}

pub fn reference_type<'a, O, F>(typ: F) -> impl Parser<Input<'a>, O, ContextError>
where
    F: Parser<Input<'a>, O, ContextError>,
{
    prefixed("&", typ).context(expect("reference type"))
}

#[cfg(test)]
mod tests;
