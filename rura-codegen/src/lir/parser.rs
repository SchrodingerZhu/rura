use rura_core::types::SurfaceType;
use winnow::prelude::*;
use winnow::*;

fn eol_comment<'a, E: error::ParserError<&'a str>>(i: &mut &'a str) -> PResult<(), E> {
    ("//", winnow::ascii::till_line_ending)
        .void() // Output is thrown away.
        .parse_next(i)
}

fn multiline_comment<'a, E: error::ParserError<&'a str>>(i: &mut &'a str) -> PResult<(), E> {
    ("/*", token::take_until(0.., "*/"), "*/")
        .void() // Output is thrown away.
        .parse_next(i)
}

fn ws_or_comment<'a, E: error::ParserError<&'a str>>(i: &mut &'a str) -> PResult<(), E> {
    combinator::repeat(
        0..,
        combinator::alt((ascii::multispace1.void(), eol_comment, multiline_comment)),
    )
    .parse_next(i)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn skip_space<'a, F, O, E: error::ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    combinator::delimited(ws_or_comment, inner, ws_or_comment)
}

fn parse_unit<'a, E: error::ParserError<&'a str>>(i: &mut &'a str) -> PResult<SurfaceType, E> {
    "()".map(|_| SurfaceType::Unit).parse_next(i)
}

fn parse_bottom<'a, E: error::ParserError<&'a str>>(i: &mut &'a str) -> PResult<SurfaceType, E> {
    "!".map(|_| SurfaceType::Bottom).parse_next(i)
}

fn parse_scalar<'a, E: error::ParserError<&'a str>>(i: &mut &'a str) -> PResult<SurfaceType, E> {
    use combinator::{empty, fail};
    use rura_core::types::ScalarType::*;
    use SurfaceType::Scalar;
    combinator::dispatch! { ascii::alpha1;
        "i8" => empty.value(Scalar(I8)),
        "i16" => empty.value(Scalar(I16)),
        "i32" => empty.value(Scalar(I32)),
        "i64" => empty.value(Scalar(I64)),
        "isize" => empty.value(Scalar(ISize)),
        "i128" => empty.value(Scalar(I128)),
        "u8" => empty.value(Scalar(U8)),
        "u16" => empty.value(Scalar(U16)),
        "u32" => empty.value(Scalar(U32)),
        "u64" => empty.value(Scalar(U64)),
        "usize" => empty.value(Scalar(USize)),
        "u128" => empty.value(Scalar(U128)),
        "f32" => empty.value(Scalar(F32)),
        "f64" => empty.value(Scalar(F64)),
        "bool" => empty.value(Scalar(Bool)),
        "char" => empty.value(Scalar(Char)),
        _ => fail,
    }
    .parse_next(i)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_eol_comment() {
        let mut input = "// Hello, world!\n";
        eol_comment::<()>(&mut input).unwrap();
        assert_eq!(input, "\n");
    }
    #[test]
    fn test_skip_space() {
        let mut input = r#"  
          // Hello, world!
          /* sdad */
          213
          // Hello, world!
        "#;
        let result = skip_space::<_, _, winnow::error::ErrorKind>(ascii::digit0).parse(&mut input);
        assert_eq!(result, Ok("213"));
    }
}
