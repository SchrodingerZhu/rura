use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::{ascii, combinator, token, PResult, Parser};

pub fn eol_comment(i: &mut &str) -> PResult<()> {
    ("//", winnow::ascii::till_line_ending).void().parse_next(i)
}

pub fn multiline_comment(i: &mut &str) -> PResult<()> {
    ("/*", token::take_until(0.., "*/"), "*/")
        .void()
        .parse_next(i)
}

pub fn ws_or_comment(i: &mut &str) -> PResult<()> {
    combinator::repeat(
        0..,
        combinator::alt((ascii::multispace1.void(), eol_comment, multiline_comment)),
    )
    .parse_next(i)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn skip_space<'a, F, O>(inner: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    combinator::delimited(ws_or_comment, inner, ws_or_comment)
}

pub fn expect(x: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(x))
}
