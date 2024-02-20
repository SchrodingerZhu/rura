#![allow(unused)]
use rura_core::types::{LirType, ScalarType};
use rura_core::Member;
use rura_core::{types::TypeVar, Ident, QualifiedName};
use winnow::ascii::alpha1;
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::prelude::*;
use winnow::*;

use crate::lir::{
    ArithMode, BinOp, BinaryOp, Block, ClosureCreation, EliminationStyle, IfThenElse,
    InductiveEliminator, Lir, MakeMutReceiver, ScalarConstant, UnOp, UnaryOp,
};

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
    ("(", inner, skip_space(")"))
        .map(|x| x.1)
        .map(|inner: Vec<_>| LirType::Tuple(inner.into_boxed_slice()))
        .parse_next(i)
}

// for now, only allow simple one-level type vars
fn parse_type_variable(i: &mut &str) -> PResult<TypeVar> {
    let plain = identifier.map(TypeVar::Plain);
    let associated =
        (identifier, skip_space("::"), identifier).map(|(a, _, b)| TypeVar::Associated(a, b));
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

fn parse_tuple_intro_instr(i: &mut &str) -> PResult<Lir> {
    let inner = combinator::separated(1.., skip_space(parse_operand), ",");
    (
        parse_operand,
        skip_space("="),
        "(",
        inner,
        skip_space(")"),
        ";",
    )
        .map(|(op, _, _, elements, _, _)| {
            let elements: Vec<_> = elements;
            Lir::TupleIntro {
                elements: elements.into_boxed_slice(),
                result: op,
            }
        })
        .parse_next(i)
}

fn parse_tuple_elim_instr(i: &mut &str) -> PResult<Lir> {
    let inner = combinator::separated(1.., skip_space(parse_operand), ",");
    (
        "(",
        inner,
        skip_space(")"),
        "=",
        skip_space(parse_operand),
        ";",
    )
        .map(|(_, elements, _, _, tuple, _)| {
            let elements: Vec<_> = elements;
            Lir::TupleElim {
                tuple,
                eliminator: elements.into_boxed_slice(),
            }
        })
        .parse_next(i)
}

fn parse_unary_op<'a>(x: char, op: UnOp) -> impl Parser<&'a str, Lir, ContextError> {
    (
        parse_operand,
        skip_space('='),
        x,
        skip_space(parse_operand),
        ";",
    )
        .map(move |(result, _, _, operand, _)| {
            Lir::UnaryOp(Box::new(UnaryOp {
                op,
                operand,
                result,
            }))
        })
}

fn parse_arith_mode(i: &mut &str) -> PResult<ArithMode> {
    ('[', skip_space(alpha1), ']')
        .verify_map(|(_, data, _)| match data {
            "default" => Some(ArithMode::Default),
            "wrapping" => Some(ArithMode::Wrapping),
            "saturating" => Some(ArithMode::Saturating),
            _ => None,
        })
        .parse_next(i)
}

fn parse_binary_op(x: &str, op: BinOp) -> impl Parser<&'_ str, Lir, ContextError> {
    (
        parse_operand,
        skip_space('='),
        parse_operand,
        skip_space(x),
        parse_operand,
        skip_space(combinator::opt(parse_arith_mode)),
        ";",
    )
        .map(move |(result, _, lhs, _, rhs, mode, _)| {
            Lir::BinaryOp(Box::new(BinaryOp {
                op,
                mode,
                lhs,
                rhs,
                result,
            }))
        })
}

fn parse_bin_ops(i: &mut &str) -> PResult<Lir> {
    combinator::alt((
        parse_binary_op("+", BinOp::Add),
        parse_binary_op("-", BinOp::Sub),
        parse_binary_op("*", BinOp::Mul),
        parse_binary_op("/", BinOp::Div),
        parse_binary_op("%", BinOp::Rem),
        parse_binary_op("==", BinOp::Eq),
        parse_binary_op("!=", BinOp::Ne),
        parse_binary_op("<", BinOp::Lt),
        parse_binary_op("<=", BinOp::Le),
        parse_binary_op(">", BinOp::Gt),
        parse_binary_op(">=", BinOp::Ge),
        parse_binary_op("&&", BinOp::And),
        parse_binary_op("||", BinOp::Or),
        parse_binary_op(">>", BinOp::Shr),
        parse_binary_op("<<", BinOp::Shl),
    ))
    .parse_next(i)
}

fn parse_unary_ops(i: &mut &str) -> PResult<Lir> {
    combinator::alt((
        parse_unary_op('-', UnOp::Neg),
        parse_unary_op('!', UnOp::Not),
    ))
    .parse_next(i)
}

fn parse_block(i: &mut &str) -> PResult<Block> {
    let inner = combinator::repeat(0.., skip_space(parse_lir_instr));
    ("{", inner, "}")
        .map(|(_, x, _)| Block(x))
        .context(expect("lir block"))
        .parse_next(i)
}

fn parse_closure_params(i: &mut &str) -> PResult<Box<[(usize, LirType)]>> {
    let param_pair = (parse_operand, skip_space(":"), parse_lir_type).map(|(x, _, y)| (x, y));
    let inner = combinator::separated(1.., skip_space(param_pair), ",")
        .map(|x: Vec<_>| x.into_boxed_slice());
    ("(", inner, ")")
        .map(|(_, x, _)| x)
        .context(expect("closure parameters"))
        .parse_next(i)
}

fn parse_closure_hoas(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space('='),
        parse_closure_params,
        skip_space("->"),
        parse_lir_type,
        ws_or_comment,
        parse_block,
    )
        .map(|(result, _, params, _, return_type, _, body)| {
            Lir::Closure(Box::new(ClosureCreation {
                result,
                params,
                body,
                return_type,
            }))
        })
        .parse_next(i)
}

fn parse_return_instr(i: &mut &str) -> PResult<Lir> {
    ("return", skip_space(parse_operand), ";")
        .map(|(_, value, _)| Lir::Return { value })
        .parse_next(i)
}

fn parse_if_then_else_instr(i: &mut &str) -> PResult<Lir> {
    (
        "if",
        skip_space(parse_operand),
        ws_or_comment,
        parse_block,
        skip_space("else"),
        parse_block,
    )
        .map(|(_, condition, _, then_branch, _, else_branch)| {
            Lir::IfThenElse(Box::new(IfThenElse {
                condition,
                then_branch,
                else_branch,
            }))
        })
        .parse_next(i)
}

fn parse_lir_instr(i: &mut &str) -> PResult<Lir> {
    combinator::alt((
        parse_constant_instr,
        parse_apply_instr,
        parse_tuple_intro_instr,
        parse_tuple_elim_instr,
        parse_unary_ops,
        parse_bin_ops,
        parse_return_instr,
        parse_closure_hoas,
        parse_if_then_else_instr,
    ))
    .context(expect("lir instruction"))
    .parse_next(i)
}

fn parse_named_value_bindings(i: &mut &str) -> PResult<Vec<(Ident, usize)>> {
    let single = (identifier, skip_space(":"), parse_operand).map(|(name, _, value)| (name, value));
    (
        "{",
        combinator::separated(1.., skip_space(single), ","),
        "}",
    )
        .map(|(_, x, _)| x)
        .context(expect("named value bindings"))
        .parse_next(i)
}

fn parse_unnamed_value_bindings(i: &mut &str) -> PResult<Vec<usize>> {
    let inner = combinator::separated(1.., skip_space(parse_operand), ",");
    ("(", inner, ")")
        .map(|(_, x, _)| x)
        .context(expect("unnamed value bindings"))
        .parse_next(i)
}

fn parse_member_bindings(i: &mut &str) -> PResult<Box<[(Member, usize)]>> {
    let named = parse_named_value_bindings.map(|inner| {
        inner
            .into_iter()
            .map(|(name, value)| (Member::Named(name.clone()), value))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    let unnamed = parse_unnamed_value_bindings.map(|inner| {
        inner
            .into_iter()
            .enumerate()
            .map(|(idx, value)| (Member::Index(idx), value))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    combinator::alt((named, unnamed)).parse_next(i)
}

fn parse_hole_operand(i: &mut &str) -> PResult<usize> {
    ("[", skip_space(parse_operand), "]")
        .map(|(_, x, _)| x)
        .context(expect("hole operand"))
        .parse_next(i)
}

fn parse_named_value_bindings_with_holes(i: &mut &str) -> PResult<Vec<(usize, Ident, usize)>> {
    let single = (
        parse_hole_operand,
        skip_space(identifier),
        ":",
        ws_or_comment,
        parse_operand,
    )
        .map(|(hole, name, _, _, value)| (hole, name, value));
    (
        "{",
        combinator::separated(1.., skip_space(single), ","),
        "}",
    )
        .map(|x| x.1)
        .context(expect("named value bindings"))
        .parse_next(i)
}

fn parse_unnamed_value_bindings_with_holes(i: &mut &str) -> PResult<Vec<(usize, usize)>> {
    let single =
        (parse_hole_operand, ws_or_comment, parse_operand).map(|(hole, _, value)| (hole, value));
    (
        "(",
        combinator::separated(1.., skip_space(single), ","),
        ")",
    )
        .map(|x| x.1)
        .context(expect("unnamed value bindings"))
        .parse_next(i)
}

fn parse_member_bindings_with_holes(i: &mut &str) -> PResult<Box<[MakeMutReceiver]>> {
    let named = parse_named_value_bindings_with_holes.map(|inner| {
        inner
            .into_iter()
            .map(|(hole, target, value)| MakeMutReceiver {
                hole,
                target: Member::Named(target),
                value,
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    let unnamed = parse_unnamed_value_bindings_with_holes.map(|inner| {
        inner
            .into_iter()
            .enumerate()
            .map(|(idx, (hole, value))| MakeMutReceiver {
                hole,
                target: Member::Index(idx),
                value,
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    combinator::alt((named, unnamed)).parse_next(i)
}

fn parse_fixpoint_eliminator_header(i: &mut &str) -> PResult<(QualifiedName, EliminationStyle)> {
    (
        "[fixpoint(",
        skip_space(parse_operand),
        ")]",
        ws_or_comment,
        qualified_name,
    )
        .map(|(_, idx, _, _, name)| (name, EliminationStyle::Fixpoint(idx)))
        .context(expect("fixpoint eliminator header"))
        .parse_next(i)
}

fn parse_unwrap_eliminator_header(i: &mut &str) -> PResult<(QualifiedName, EliminationStyle)> {
    (
        "[unwrap(",
        skip_space(parse_operand),
        ")]",
        skip_space(qualified_name),
        parse_member_bindings,
    )
        .map(|(_, token, _, name, fields)| (name, EliminationStyle::Unwrap { fields, token }))
        .context(expect("unwrap eliminator header"))
        .parse_next(i)
}

fn parse_mutation_eliminator_header(i: &mut &str) -> PResult<(QualifiedName, EliminationStyle)> {
    (
        "[mutation]",
        skip_space(qualified_name),
        parse_member_bindings_with_holes,
    )
        .map(|(_, name, fields)| (name, EliminationStyle::Mutation(fields)))
        .context(expect("mutation eliminator header"))
        .parse_next(i)
}

fn parse_ref_eliminator_header(i: &mut &str) -> PResult<(QualifiedName, EliminationStyle)> {
    ("[ref]", skip_space(qualified_name), parse_member_bindings)
        .map(|(_, name, fields)| (name, EliminationStyle::Ref(fields)))
        .context(expect("ref eliminator header"))
        .parse_next(i)
}

fn parse_inductive_elimination_instr(i: &mut &str) -> PResult<Lir> {
    let header = combinator::alt((
        parse_fixpoint_eliminator_header,
        parse_unwrap_eliminator_header,
        parse_mutation_eliminator_header,
        parse_ref_eliminator_header,
    ));
    let eliminator = (header, skip_space("=>"), parse_block)
        .map(|((ctor, style), _, body)| InductiveEliminator { ctor, style, body });
    let rules = combinator::repeat(1.., skip_space(eliminator)).map(Vec::into_boxed_slice);

    ("match", skip_space(parse_operand), "{", rules, "}")
        .map(
            |(_, inductive, _, eliminator, _)| Lir::InductiveElimination {
                inductive,
                eliminator,
            },
        )
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
            Ok(TypeVar::Associated(Ident::new("T"), Ident::new("U")))
        );
    }

    #[test]
    fn test_parse_as_expr_type_var() {
        let mut input = "@<@T::U as std::V>::W";
        let result = parse_type_variable(&mut input);
        assert_eq!(
            result,
            Ok(TypeVar::AsExpr(
                Box::new(TypeVar::Associated(Ident::new("T"), Ident::new("U"))),
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
                #[allow(clippy::approx_constant)]
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
    #[test]
    fn test_parse_tuple_intro_instr() {
        let mut input = "%1 = ( %2, %3 );";
        let result = parse_tuple_intro_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::TupleIntro {
                elements: Box::new([2, 3]),
                result: 1
            }
        );
    }

    #[test]
    fn test_parse_tuple_elim_instr() {
        let mut input = "( %1, %2 ) = %3;";
        let result = parse_tuple_elim_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::TupleElim {
                tuple: 3,
                eliminator: Box::new([1, 2])
            }
        );
    }

    #[test]
    fn test_parse_unary_neg_instr() {
        let mut input = "%1 = - %2;";
        let result = parse_unary_op('-', UnOp::Neg)
            .parse_next(&mut input)
            .unwrap();
        assert_eq!(
            result,
            Lir::UnaryOp(Box::new(UnaryOp {
                op: UnOp::Neg,
                operand: 2,
                result: 1
            }))
        );
    }

    #[test]
    fn test_parse_binary_wrapping_add_instr() {
        let mut input = "%1 = %2 + %3 [wrapping];";
        let result = parse_binary_op("+", BinOp::Add)
            .parse_next(&mut input)
            .unwrap();
        assert_eq!(
            result,
            Lir::BinaryOp(Box::new(BinaryOp {
                op: BinOp::Add,
                mode: Some(ArithMode::Wrapping),
                lhs: 2,
                rhs: 3,
                result: 1
            }))
        );
    }

    #[test]
    fn test_parse_closure_hoas() {
        let mut input = r#"%1 = ( %2 : i32, %3 : f64 ) -> i32 { 
            %4 = constant 3 : i32;
            return %4;
        };"#;
        let result = parse_closure_hoas(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::Closure(Box::new(ClosureCreation {
                result: 1,
                params: Box::new([
                    (2, LirType::Scalar(ScalarType::I32),),
                    (3, LirType::Scalar(ScalarType::F64),)
                ]),
                body: Block(vec![
                    Lir::ConstantScalar {
                        value: Box::new(ScalarConstant::I32(3)),
                        result: 4
                    },
                    Lir::Return { value: 4 },
                ]),
                return_type: LirType::Scalar(ScalarType::I32)
            }))
        );
    }

    #[test]
    fn test_parse_if_then_else_instr_in_block() {
        let mut text = r#"{
            %1 = constant true : bool;
            %2 = constant 3 : i32;
            %3 = constant 4 : i32;
            // block terminator
            if %1 { return %2; } else { return %3; }
        }"#;
        let result = parse_block(&mut text).unwrap();
        assert_eq!(
            result,
            Block(vec![
                Lir::ConstantScalar {
                    value: Box::new(ScalarConstant::Bool(true)),
                    result: 1
                },
                Lir::ConstantScalar {
                    value: Box::new(ScalarConstant::I32(3)),
                    result: 2
                },
                Lir::ConstantScalar {
                    value: Box::new(ScalarConstant::I32(4)),
                    result: 3
                },
                Lir::IfThenElse(Box::new(IfThenElse {
                    condition: 1,
                    then_branch: Block(vec![Lir::Return { value: 2 }]),
                    else_branch: Block(vec![Lir::Return { value: 3 }]),
                }))
            ])
        );
    }

    #[test]
    fn test_parse_fixpoint_eliminator_header() {
        let mut input = r#"[fixpoint(%0)] std::Vec"#;
        let result = parse_fixpoint_eliminator_header(&mut input).unwrap();
        assert_eq!(
            result,
            (
                QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                EliminationStyle::Fixpoint(0)
            )
        );
    }

    #[test]
    fn test_parse_unwrap_eliminator_header() {
        let mut input = r#"[unwrap(%0)] std::Vec { head: %1, tail: %2 }"#;
        let result = parse_unwrap_eliminator_header(&mut input).unwrap();
        assert_eq!(
            result,
            (
                QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                EliminationStyle::Unwrap {
                    token: 0,
                    fields: Box::new([
                        (Member::Named("head".into()), 1),
                        (Member::Named("tail".into()), 2),
                    ])
                }
            )
        );
    }

    #[test]
    fn test_parse_mutation_eliminator_header() {
        let mut input = r#"[mutation] std::Vec ([%2] %1, [%4] %3)"#;
        let result = parse_mutation_eliminator_header(&mut input).unwrap();
        assert_eq!(
            result,
            (
                QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                EliminationStyle::Mutation(Box::new([
                    MakeMutReceiver {
                        hole: 2,
                        target: Member::Index(0),
                        value: 1
                    },
                    MakeMutReceiver {
                        hole: 4,
                        target: Member::Index(1),
                        value: 3
                    }
                ]))
            )
        );
    }
    #[test]
    fn test_parse_inductive_elimination_instr() {
        let mut input = r#"match %0 {
            [fixpoint(%1)] std::Vec => { %2 = constant 3 : i32; return %2; }
            [unwrap(%3)] std::Vec { head: %4, tail: %5 } => { %6 = constant 4 : i32; return %6; }
        }"#;
        let result = parse_inductive_elimination_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::InductiveElimination {
                inductive: 0,
                eliminator: Box::new([
                    InductiveEliminator {
                        ctor: QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                        style: EliminationStyle::Fixpoint(1),
                        body: Block(vec![
                            Lir::ConstantScalar {
                                value: Box::new(ScalarConstant::I32(3)),
                                result: 2
                            },
                            Lir::Return { value: 2 }
                        ])
                    },
                    InductiveEliminator {
                        ctor: QualifiedName::new(Box::new([Ident::new("std"), Ident::new("Vec")])),
                        style: EliminationStyle::Unwrap {
                            token: 3,
                            fields: Box::new([
                                (Member::Named("head".into()), 4),
                                (Member::Named("tail".into()), 5)
                            ])
                        },
                        body: Block(vec![
                            Lir::ConstantScalar {
                                value: Box::new(ScalarConstant::I32(4)),
                                result: 6
                            },
                            Lir::Return { value: 6 }
                        ])
                    }
                ])
            }
        );
    }
}
