#![allow(unused)]
use rura_core::types::{LirType, ScalarType};
use rura_core::{types::TypeVar, Ident, QualifiedName};
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::prelude::*;
use winnow::*;

use crate::lir::{Lir, ScalarConstant};

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

fn expect(x: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(x))
}

fn parse_scalar_type(i: &mut &str) -> PResult<ScalarType> {
    use combinator::{empty, fail};
    use rura_core::types::ScalarType::*;
    combinator::dispatch! { ascii::alphanumeric1;
        "i8" => empty.value(I8),
        "i16" => empty.value(I16),
        "i32" => empty.value(I32),
        "i64" => empty.value(I64),
        "isize" => empty.value(ISize),
        "i128" => empty.value(I128),
        "u8" => empty.value(U8),
        "u16" => empty.value(U16),
        "u32" => empty.value(U32),
        "u64" => empty.value(U64),
        "usize" => empty.value(USize),
        "u128" => empty.value(U128),
        "f32" => empty.value(F32),
        "f64" => empty.value(F64),
        "bool" => empty.value(Bool),
        "char" => empty.value(Char),
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
        .context(expect("identifier"))
        .parse_next(input)
}

fn qualified_name(input: &mut &str) -> PResult<QualifiedName> {
    combinator::separated(1.., identifier, "::")
        .map(|idents: Vec<Ident>| QualifiedName::new(idents.into_boxed_slice()))
        .context(expect("qualified name"))
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
        parse_scalar_type.map(LirType::Scalar),
        parse_tuple_type,
        parse_type_variable.map(LirType::TypeVar),
        parse_type_hole,
        parse_type_ref,
        parse_closure_type,
        parse_object_type,
    ))
    .context(expect("lir type"))
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

fn parse_operand(i: &mut &str) -> PResult<usize> {
    combinator::preceded("%", ascii::digit1.try_map(|x: &str| x.parse()))
        .map(|x: usize| x)
        .parse_next(i)
}

fn parse_typed_char(i: &mut &str) -> PResult<ScalarConstant> {
    (ascii::digit1, skip_space(":"), skip_space("char"))
        .try_map(|(data, _, _)| data.parse::<u32>())
        .verify_map(char::from_u32)
        .map(ScalarConstant::Char)
        .parse_next(i)
}

fn parse_typed_bool(i: &mut &str) -> PResult<ScalarConstant> {
    (ascii::alpha1, skip_space(":"), skip_space("bool"))
        .try_map(|(data, _, _)| data.parse::<bool>())
        .map(ScalarConstant::Bool)
        .parse_next(i)
}

fn parse_typed_f32(i: &mut &str) -> PResult<ScalarConstant> {
    (ascii::float, skip_space(":"), skip_space("f32"))
        .map(|(data, _, _)| ScalarConstant::F32(data))
        .parse_next(i)
}

fn parse_typed_f64(i: &mut &str) -> PResult<ScalarConstant> {
    (ascii::float, skip_space(":"), skip_space("f64"))
        .map(|(data, _, _)| ScalarConstant::F64(data))
        .parse_next(i)
}

macro_rules! parse_typed_int {
    (signed $name:ident $ctor:ident $tag:literal) => {
        fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            (ascii::dec_int, skip_space(":"), skip_space($tag))
                .map(|(data, _, _)| ScalarConstant::$ctor(data))
                .parse_next(i)
        }
    };
    (unsigned $name:ident $ctor:ident $tag:literal) => {
        fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            (ascii::dec_uint, skip_space(":"), skip_space($tag))
                .map(|(data, _, _)| ScalarConstant::$ctor(data))
                .parse_next(i)
        }
    };
}

parse_typed_int!(signed parse_typed_i8 I8 "i8");
parse_typed_int!(signed parse_typed_i16 I16 "i16");
parse_typed_int!(signed parse_typed_i32 I32 "i32");
parse_typed_int!(signed parse_typed_i64 I64 "i64");
parse_typed_int!(signed parse_typed_i128 I128 "i128");
parse_typed_int!(signed parse_typed_isize ISize "isize");
parse_typed_int!(unsigned parse_typed_u8 U8 "u8");
parse_typed_int!(unsigned parse_typed_u16 U16 "u16");
parse_typed_int!(unsigned parse_typed_u32 U32 "u32");
parse_typed_int!(unsigned parse_typed_u64 U64 "u64");
parse_typed_int!(unsigned parse_typed_u128 U128 "u128");
parse_typed_int!(unsigned parse_typed_usize USize "usize");

fn parse_constant_instr(i: &mut &str) -> PResult<Lir> {
    let inner = combinator::alt((
        parse_typed_char,
        parse_typed_bool,
        parse_typed_f32,
        parse_typed_f64,
        parse_typed_i8,
        parse_typed_i16,
        parse_typed_i32,
        parse_typed_i64,
        parse_typed_i128,
        parse_typed_isize,
        parse_typed_u8,
        parse_typed_u16,
        parse_typed_u32,
        parse_typed_u64,
        parse_typed_u128,
        parse_typed_usize,
    ));
    (
        parse_operand,
        skip_space("="),
        "constant",
        skip_space(inner).context(expect("typed constant value")),
        ";",
    )
        .map(|(op, _, _, value, _)| Lir::ConstantScalar {
            value: Box::new(value),
            result: op,
        })
        .parse_next(i)
}

fn parse_apply_instr(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space("="),
        "apply",
        skip_space(parse_operand),
        ",",
        skip_space(parse_operand),
        ";",
    )
        .map(|(op, _, _, closure, _, arg, _)| Lir::Apply {
            closure,
            arg,
            result: op,
        })
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

    #[test]
    fn test_parse_constant_float_instr() {
        let mut input = "%1 = constant 3.14 : f64;";
        let result = parse_constant_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::ConstantScalar {
                value: Box::new(ScalarConstant::F64(3.14)),
                result: 1
            }
        );
    }

    #[test]
    fn test_parse_usize() {
        let mut input = "%1 = constant 3 : usize;";
        let result = parse_constant_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::ConstantScalar {
                value: Box::new(ScalarConstant::USize(3)),
                result: 1
            }
        );
    }

    #[test]
    fn test_parse_apply_instr() {
        let mut input = "%1 = apply %2, %3;";
        let result = parse_apply_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::Apply {
                closure: 2,
                arg: 3,
                result: 1
            }
        );
    }
}
