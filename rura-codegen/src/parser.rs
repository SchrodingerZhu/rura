#![allow(unused)]
use rura_core::types::LirType;
use rura_core::{types::TypeVar, Ident, QualifiedName};
use winnow::error::ContextError;
use winnow::prelude::*;
use winnow::*;

fn eol_comment(i: &mut &str) -> PResult<()> {
    ("//", winnow::ascii::till_line_ending)
        .void() // Output is thrown away.
        .parse_next(i)
}

fn multiline_comment(i: &mut &str) -> PResult<()> {
    ("/*", token::take_until(0.., "*/"), "*/")
        .void() // Output is thrown away.
        .parse_next(i)
}

fn ws_or_comment(i: &mut &str) -> PResult<()> {
    combinator::repeat(
        0..,
        combinator::alt((ascii::multispace1.void(), eol_comment, multiline_comment)),
    )
    .parse_next(i)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn skip_space<'a, F, O>(inner: F) -> impl Parser<&'a str, O, ContextError>
where
    F: Parser<&'a str, O, ContextError>,
{
    combinator::delimited(ws_or_comment, inner, ws_or_comment)
}

fn parse_unit(i: &mut &str) -> PResult<LirType> {
    "()".map(|_| LirType::Unit).parse_next(i)
}

fn parse_bottom(i: &mut &str) -> PResult<LirType> {
    "!".map(|_| LirType::Bottom).parse_next(i)
}

fn parse_scalar(i: &mut &str) -> PResult<LirType> {
    use combinator::{empty, fail};
    use rura_core::types::ScalarType::*;
    use LirType::Scalar;
    combinator::dispatch! { ascii::alphanumeric1;
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

fn identifier(input: &mut &str) -> PResult<Ident> {
    (
        token::one_of(|c: char| unicode_ident::is_xid_start(c)),
        token::take_while(0.., |c: char| unicode_ident::is_xid_continue(c)),
    )
        .recognize()
        .map(Ident::new)
        .parse_next(input)
}

fn qualified_name(input: &mut &str) -> PResult<QualifiedName> {
    combinator::separated(1.., identifier, "::")
        .map(|idents: Vec<Ident>| QualifiedName::new(idents.into_boxed_slice()))
        .parse_next(input)
}

fn parse_type_hole(i: &mut &str) -> PResult<LirType> {
    ("◊", parse_lir_type)
        .map(|(_, x)| LirType::Hole(Box::new(x)))
        .parse_next(i)
}

fn parse_type_ref(i: &mut &str) -> PResult<LirType> {
    ("&", parse_lir_type)
        .map(|(_, x)| LirType::Ref(Box::new(x)))
        .parse_next(i)
}

fn parse_object_type(i: &mut &str) -> PResult<LirType> {
    let inner = skip_space(parse_lir_type);
    let type_parameters = combinator::opt(
        (
            "<",
            combinator::separated(1.., skip_space(parse_lir_type), ","),
            ">",
        )
            .map(|(_, x, _): (_, Vec<_>, _)| x),
    );
    (qualified_name, ws_or_comment, type_parameters)
        .map(|(qn, _, type_params)| {
            LirType::Object(qn, type_params.unwrap_or_default().into_boxed_slice())
        })
        .parse_next(i)
}

fn parse_closure_type(i: &mut &str) -> PResult<LirType> {
    let types = combinator::separated(0.., skip_space(parse_lir_type), ",");
    (
        "fn",
        skip_space("("),
        types,
        skip_space(")"),
        "->",
        skip_space(parse_lir_type),
    )
        .map(|(_, _, args, _, _, ret)| (args, ret))
        .map(|(args, ret): (Vec<_>, _)| LirType::Closure(args.into_boxed_slice(), Box::new(ret)))
        .parse_next(i)
}

fn parse_lir_type(i: &mut &str) -> PResult<LirType> {
    combinator::alt((
        parse_unit,
        parse_bottom,
        parse_scalar,
        parse_tuple_type,
        parse_type_variable.map(LirType::TypeVar),
        parse_type_hole,
        parse_type_ref,
        parse_closure_type,
        parse_object_type,
    ))
    .parse_next(i)
}

fn parse_tuple_type(i: &mut &str) -> PResult<LirType> {
    let delimited = skip_space(parse_lir_type);
    let inner = combinator::repeat(1.., (delimited, ",").map(|x| x.0));
    ("(", inner, ")")
        .map(|x| x.1)
        .map(|inner: Vec<_>| LirType::Tuple(inner.into_boxed_slice()))
        .parse_next(i)
}

// for now, only allow simple one-level type vars
fn parse_type_variable(i: &mut &str) -> PResult<TypeVar> {
    let plain = identifier.map(TypeVar::Plain);
    let associated =
        (identifier, skip_space("::"), identifier).map(|(a, _, b)| TypeVar::Asscoiated(a, b));
    let as_expr = (
        "<",
        skip_space(parse_type_variable),
        "as",
        skip_space(qualified_name),
        ">",
        skip_space("::"),
        identifier,
    )
        .map(|(_, nested, _, qn, _, _, id)| TypeVar::AsExpr(Box::new(nested), qn, id));
    ("@", combinator::alt((associated, plain, as_expr)))
        .map(|x| x.1)
        .parse_next(i)
}

#[cfg(test)]
mod test {
    use rura_core::types::ScalarType;

    use super::*;
    #[test]
    fn test_eol_comment() {
        let mut input = "// Hello, world!\n";
        eol_comment(&mut input).unwrap();
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
        let result = skip_space(ascii::digit0).parse(input);
        assert_eq!(result, Ok("213"));
    }

    #[test]
    fn test_parse_plain_type_var() {
        let mut input = "@T";
        let result = parse_type_variable(&mut input);
        assert_eq!(result, Ok(TypeVar::Plain(Ident::new("T"))));
    }

    #[test]
    fn test_parse_associated_type_var() {
        let mut input = "@T::U";
        let result = parse_type_variable(&mut input);
        assert_eq!(
            result,
            Ok(TypeVar::Asscoiated(Ident::new("T"), Ident::new("U")))
        );
    }

    #[test]
    fn test_parse_as_expr_type_var() {
        let mut input = "@<@T::U as std::V>::W";
        let result = parse_type_variable(&mut input);
        assert_eq!(
            result,
            Ok(TypeVar::AsExpr(
                Box::new(TypeVar::Asscoiated(Ident::new("T"), Ident::new("U"))),
                QualifiedName::new(Box::new([Ident::new("std"), Ident::new("V")])),
                Ident::new("W")
            ))
        );
    }

    #[test]
    fn test_parse_tuple_of_scalars() {
        let mut input = "(i32,f64,)";
        let result = parse_lir_type(&mut input);
        assert_eq!(
            result,
            Ok(LirType::Tuple(Box::new([
                LirType::Scalar(ScalarType::I32),
                LirType::Scalar(ScalarType::F64)
            ])))
        );
    }

    #[test]
    fn test_parse_object_type_without_params() {
        let mut input = "std::Vec";
        let result = parse_lir_type(&mut input);
        assert_eq!(
            result,
            Ok(LirType::Object(
                QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                Box::new([])
            ))
        );
    }

    #[test]
    fn test_parse_object_type_with_params() {
        let mut input = "std::Vec<i32, f64>";
        let result = parse_lir_type(&mut input);
        assert_eq!(
            result,
            Ok(LirType::Object(
                QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                Box::new([
                    LirType::Scalar(ScalarType::I32),
                    LirType::Scalar(ScalarType::F64)
                ])
            ))
        );
    }

    #[test]
    fn test_parse_closure_type() {
        let mut input = "fn (i32, f64) -> i32";
        let result = parse_lir_type(&mut input);
        assert_eq!(
            result,
            Ok(LirType::Closure(
                Box::new([
                    LirType::Scalar(ScalarType::I32),
                    LirType::Scalar(ScalarType::F64)
                ]),
                Box::new(LirType::Scalar(ScalarType::I32))
            ))
        );
    }
}
