use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::ascii::{multispace1, till_line_ending};
use winnow::combinator::{alt, delimited, repeat};
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::token::{one_of, take_until, take_while};
use winnow::{PResult, Parser};

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
}
