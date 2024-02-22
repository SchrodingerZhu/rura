use winnow::ascii::{alpha1, alphanumeric1, dec_int, dec_uint, digit1, float};
use winnow::combinator::{alt, dispatch, empty, fail, opt, preceded, repeat, separated};
use winnow::error::ContextError;
use winnow::{PResult, Parser};

use rura_core::types::{LirType, ScalarType};
use rura_core::Member;
use rura_core::{types::TypeVar, Ident, QualifiedName};
use rura_parsing::keywords::{BOTTOM, UNIT};
use rura_parsing::{
    expect, identifier, keywords, optional_type_parameters, qualified_name, skip_space,
    ws_or_comment,
};

use crate::lir::{
    ArithMode, BinOp, BinaryOp, Block, Bound, ClosureCreation, CtorCall, CtorDef, EliminationStyle,
    FunctionCall, FunctionDef, FunctionPrototype, IfThenElse, InductiveEliminator,
    InductiveTypeDef, Lir, MakeMutReceiver, Module, ScalarConstant, TraitExpr, UnOp, UnaryOp,
};

fn parse_scalar_type(i: &mut &str) -> PResult<ScalarType> {
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

fn parse_object_type_content(i: &mut &str) -> PResult<(QualifiedName, Box<[LirType]>)> {
    let type_parameters = opt(("<", separated(1.., skip_space(parse_lir_type), ","), ">")
        .map(|(_, x, _): (_, Vec<_>, _)| x));
    (qualified_name, ws_or_comment, type_parameters)
        .map(|(qn, _, type_params)| (qn, type_params.unwrap_or_default().into_boxed_slice()))
        .parse_next(i)
}

fn parse_closure_type(i: &mut &str) -> PResult<LirType> {
    let types = separated(0.., skip_space(parse_lir_type), ",");
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
    alt((
        UNIT.map(|_| LirType::Unit),
        BOTTOM.map(|_| LirType::Bottom),
        parse_scalar_type.map(LirType::Scalar),
        parse_tuple_type,
        parse_type_variable.map(LirType::TypeVar),
        parse_type_hole,
        parse_type_ref,
        parse_closure_type,
        parse_object_type_content.map(|(name, params)| LirType::Object(name, params)),
    ))
    .context(expect("lir type"))
    .parse_next(i)
}

fn parse_tuple_type(i: &mut &str) -> PResult<LirType> {
    let delimited = skip_space(parse_lir_type);
    let inner = repeat(1.., (delimited, ",").map(|x| x.0));
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
    ("@", alt((associated, plain, as_expr)))
        .map(|x| x.1)
        .parse_next(i)
}

fn parse_operand(i: &mut &str) -> PResult<usize> {
    preceded("%", digit1.try_map(|x: &str| x.parse()))
        .map(|x: usize| x)
        .parse_next(i)
}

fn parse_typed_char(i: &mut &str) -> PResult<ScalarConstant> {
    (digit1, skip_space(":"), skip_space("char"))
        .try_map(|(data, _, _)| data.parse::<u32>())
        .verify_map(char::from_u32)
        .map(ScalarConstant::Char)
        .parse_next(i)
}

fn parse_typed_bool(i: &mut &str) -> PResult<ScalarConstant> {
    (alpha1, skip_space(":"), skip_space("bool"))
        .try_map(|(data, _, _)| data.parse::<bool>())
        .map(ScalarConstant::Bool)
        .parse_next(i)
}

fn parse_typed_f32(i: &mut &str) -> PResult<ScalarConstant> {
    (float, skip_space(":"), skip_space("f32"))
        .map(|(data, _, _)| ScalarConstant::F32(data))
        .parse_next(i)
}

fn parse_typed_f64(i: &mut &str) -> PResult<ScalarConstant> {
    (float, skip_space(":"), skip_space("f64"))
        .map(|(data, _, _)| ScalarConstant::F64(data))
        .parse_next(i)
}

macro_rules! parse_typed_int {
    (signed $name:ident $ctor:ident $tag:literal) => {
        fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            (dec_int, skip_space(":"), skip_space($tag))
                .map(|(data, _, _)| ScalarConstant::$ctor(data))
                .parse_next(i)
        }
    };
    (unsigned $name:ident $ctor:ident $tag:literal) => {
        fn $name(i: &mut &str) -> PResult<ScalarConstant> {
            (dec_uint, skip_space(":"), skip_space($tag))
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
    let inner = alt((
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
    let inner = separated(1.., skip_space(parse_operand), ",");
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
    let inner = separated(1.., skip_space(parse_operand), ",");
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
        skip_space(opt(parse_arith_mode)),
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
    alt((
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
    alt((
        parse_unary_op('-', UnOp::Neg),
        parse_unary_op('!', UnOp::Not),
    ))
    .parse_next(i)
}

fn parse_block(i: &mut &str) -> PResult<Block> {
    let inner = repeat(0.., skip_space(parse_lir_instr));
    ("{", inner, "}")
        .map(|(_, x, _)| Block(x))
        .context(expect("lir block"))
        .parse_next(i)
}

fn parse_closure_params(i: &mut &str) -> PResult<Box<[(usize, LirType)]>> {
    let param_pair = (parse_operand, skip_space(":"), parse_lir_type).map(|(x, _, y)| (x, y));
    let inner = separated(0.., skip_space(param_pair), ",").map(|x: Vec<_>| x.into_boxed_slice());
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
    alt((
        parse_closure_hoas,
        parse_constant_instr,
        parse_apply_instr,
        parse_tuple_intro_instr,
        parse_tuple_elim_instr,
        parse_unary_ops,
        parse_bin_ops,
        parse_return_instr,
        parse_if_then_else_instr,
        parse_inductive_elimination_instr,
        parse_fill_instr,
        parse_call_instr,
        parse_ctor_call_instr,
        parse_clone_instr,
        parse_drop_instr,
        parse_drop_for_reuse_instr,
        parse_curry_instr,
    ))
    .context(expect("lir instruction"))
    .parse_next(i)
}

fn parse_named_value_bindings(i: &mut &str) -> PResult<Vec<(Ident, usize)>> {
    let single = (identifier, skip_space(":"), parse_operand).map(|(name, _, value)| (name, value));
    ("{", separated(1.., skip_space(single), ","), "}")
        .map(|(_, x, _)| x)
        .context(expect("named value bindings"))
        .parse_next(i)
}

fn parse_unnamed_value_bindings(i: &mut &str) -> PResult<Vec<usize>> {
    let inner = separated(1.., skip_space(parse_operand), ",");
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
    alt((named, unnamed)).parse_next(i)
}

fn parse_squared_operand(i: &mut &str) -> PResult<usize> {
    ("[", skip_space(parse_operand), "]")
        .map(|(_, x, _)| x)
        .context(expect("squared operand"))
        .parse_next(i)
}

fn parse_named_value_bindings_with_holes(i: &mut &str) -> PResult<Vec<(usize, Ident, usize)>> {
    let single = (
        parse_squared_operand,
        skip_space(identifier),
        ":",
        ws_or_comment,
        parse_operand,
    )
        .map(|(hole, name, _, _, value)| (hole, name, value));
    ("{", separated(1.., skip_space(single), ","), "}")
        .map(|x| x.1)
        .context(expect("named value bindings"))
        .parse_next(i)
}

fn parse_unnamed_value_bindings_with_holes(i: &mut &str) -> PResult<Vec<(usize, usize)>> {
    let single =
        (parse_squared_operand, ws_or_comment, parse_operand).map(|(hole, _, value)| (hole, value));
    ("(", separated(1.., skip_space(single), ","), ")")
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
    alt((named, unnamed)).parse_next(i)
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
    let header = alt((
        parse_fixpoint_eliminator_header,
        parse_unwrap_eliminator_header,
        parse_mutation_eliminator_header,
        parse_ref_eliminator_header,
    ));
    let eliminator = (header, skip_space("=>"), parse_block)
        .map(|((ctor, style), _, body)| InductiveEliminator { ctor, style, body });
    let rules = repeat(1.., skip_space(eliminator)).map(Vec::into_boxed_slice);

    ("match", skip_space(parse_operand), "{", rules, "}")
        .map(
            |(_, inductive, _, eliminator, _)| Lir::InductiveElimination {
                inductive,
                eliminator,
            },
        )
        .parse_next(i)
}

fn parse_fill_instr(i: &mut &str) -> PResult<Lir> {
    (
        "fill",
        skip_space(parse_operand),
        "<-",
        skip_space(parse_operand),
        ";",
    )
        .map(|(_, hole, _, value, _)| Lir::Fill { hole, value })
        .parse_next(i)
}

fn parse_operand_list<'a>() -> impl Parser<&'a str, Box<[usize]>, ContextError> {
    ("(", separated(0.., skip_space(parse_operand), ","), ")")
        .map(|(_, x, _)| Vec::into_boxed_slice(x))
}

fn parse_call_instr(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space("="),
        "call",
        skip_space(qualified_name),
        parse_operand_list(),
        ";",
    )
        .map(|(result, _, _, function, args, _)| {
            Lir::Call(Box::new(FunctionCall {
                result,
                function,
                args,
            }))
        })
        .parse_next(i)
}

fn parse_ctor_call_instr(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space("="),
        "new",
        skip_space(opt(parse_squared_operand)),
        parse_object_type_content,
        skip_space("@"),
        identifier,
        skip_space(parse_operand_list()),
        opt("[unique]"),
        ws_or_comment,
        ";",
    )
        .map(
            |(result, _, _, token, (type_name, type_params), _, ctor, args, unique_rc, _, _)| {
                Lir::CtorCall(Box::new(CtorCall {
                    result,
                    token,
                    type_name,
                    type_params,
                    args,
                    ctor,
                    unique_rc: unique_rc.is_some(),
                }))
            },
        )
        .parse_next(i)
}

fn parse_clone_instr(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space("="),
        "clone",
        skip_space(parse_operand),
        ";",
    )
        .map(|(result, _, _, value, _)| Lir::Clone { result, value })
        .parse_next(i)
}

fn parse_drop_instr(i: &mut &str) -> PResult<Lir> {
    ("drop", skip_space(parse_operand), ";")
        .map(|(_, value, _)| Lir::Drop { value, token: None })
        .parse_next(i)
}

fn parse_drop_for_reuse_instr(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space("="),
        "drop",
        skip_space(parse_operand),
        ";",
    )
        .map(|(token, _, _, value, _)| Lir::Drop {
            value,
            token: Some(token),
        })
        .parse_next(i)
}

fn parse_curry_instr(i: &mut &str) -> PResult<Lir> {
    (
        parse_operand,
        skip_space("="),
        "curry",
        skip_space(qualified_name),
        ";",
    )
        .map(|(result, _, _, function, _)| Lir::Curry { result, function })
        .parse_next(i)
}

fn parse_trait_expr(i: &mut &str) -> PResult<TraitExpr> {
    let named_param =
        (identifier, skip_space("="), parse_lir_type).map(|(name, _, ty)| (Some(name), ty));
    let unnamed_param = parse_lir_type.map(|ty| (None, ty));
    let param = alt((named_param, unnamed_param));
    let params = opt(("<", separated(1.., skip_space(param), ","), ">"))
        .map(|x| Vec::into_boxed_slice(x.unwrap_or_default().1));
    (qualified_name, params)
        .map(|(name, params)| TraitExpr { name, params })
        .parse_next(i)
}

fn parse_bounded_type_var(i: &mut &str) -> PResult<Bound> {
    (
        parse_type_variable,
        skip_space(":"),
        separated(1.., skip_space(parse_trait_expr), "+"),
    )
        .map(|(target, _, traits)| Bound {
            target,
            bounds: Vec::into_boxed_slice(traits),
        })
        .parse_next(i)
}

fn parse_function_prototype(i: &mut &str) -> PResult<FunctionPrototype> {
    let type_params = opt((
        "<",
        separated(1.., skip_space(identifier::<Ident>), ","),
        ">",
    ))
    .map(|x| Vec::into_boxed_slice(x.unwrap_or_default().1));
    let params = (
        "(",
        separated(
            0..,
            skip_space((parse_operand, skip_space(":"), parse_lir_type)).map(|(x, _, y)| (x, y)),
            ",",
        ),
        ")",
    )
        .map(|x| Vec::into_boxed_slice(x.1));

    let bounds = opt((
        "where",
        separated(1.., skip_space(parse_bounded_type_var), ","),
    ))
    .map(|x| Vec::into_boxed_slice(x.unwrap_or_default().1));
    (
        "fn",
        skip_space(qualified_name),
        type_params,
        skip_space(params),
        "->",
        skip_space(parse_lir_type),
        bounds,
    )
        .map(
            |(_, name, type_params, params, _, return_type, bounds)| FunctionPrototype {
                name,
                type_params,
                params,
                return_type,
                bounds,
            },
        )
        .parse_next(i)
}

fn parse_function_def(i: &mut &str) -> PResult<FunctionDef> {
    (
        parse_function_prototype,
        ws_or_comment,
        parse_block.context(expect("function body")),
    )
        .map(|(prototype, _, body)| FunctionDef { prototype, body })
        .parse_next(i)
}

fn parse_extern_function_def(i: &mut &str) -> PResult<FunctionPrototype> {
    (parse_function_prototype, ";")
        .map(|(x, _)| x)
        .parse_next(i)
}

fn parse_named_member_list(i: &mut &str) -> PResult<Box<[(Member, LirType)]>> {
    let field = (identifier::<Ident>, skip_space(":"), parse_lir_type)
        .map(|(name, _, ty)| (Member::Named(name.clone()), ty));
    ("{", separated(1.., skip_space(field), ","), "}")
        .map(|(_, x, _)| Vec::into_boxed_slice(x))
        .context(expect("named member list"))
        .parse_next(i)
}

fn parse_unnamed_member_list(i: &mut &str) -> PResult<Box<[(Member, LirType)]>> {
    let inner = separated(1.., skip_space(parse_lir_type), ",").map(|x: Vec<_>| {
        x.into_iter()
            .enumerate()
            .map(|(idx, ty)| (Member::Index(idx), ty))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });
    ("(", inner, ")")
        .map(|(_, x, _)| x)
        .context(expect("unnamed member list"))
        .parse_next(i)
}

fn parse_ctor_def(i: &mut &str) -> PResult<CtorDef> {
    let member_list = opt(alt((parse_named_member_list, parse_unnamed_member_list)))
        .map(|x| x.unwrap_or_default());
    (identifier, skip_space(member_list))
        .map(|(name, params)| CtorDef { name, params })
        .context(expect("constructor definition"))
        .parse_next(i)
}

fn parse_inductive_type_def(i: &mut &str) -> PResult<InductiveTypeDef> {
    let bounds = opt((
        "where",
        separated(1.., skip_space(parse_bounded_type_var), ","),
    ))
    .context(expect("type bounds"))
    .map(|x| Vec::into_boxed_slice(x.unwrap_or_default().1));
    let ctors = separated(1.., skip_space(parse_ctor_def), ",")
        .context(expect("constructors"))
        .map(Vec::into_boxed_slice);
    (
        "enum",
        skip_space(qualified_name).context(expect("qualified name")),
        optional_type_parameters,
        skip_space(bounds),
        "{",
        ctors,
        "}",
    )
        .map(
            |(_, name, type_params, bounds, _, ctors, _)| InductiveTypeDef {
                name,
                type_params,
                bounds,
                ctors,
            },
        )
        .context(expect("inductive type definition"))
        .parse_next(i)
}

pub fn parse_module(i: &mut &str) -> PResult<Module> {
    enum ModuleItem {
        Function(FunctionDef),
        ExternFunction(FunctionPrototype),
        InductiveType(InductiveTypeDef),
    }
    let inner = alt((
        parse_function_def.map(ModuleItem::Function),
        parse_extern_function_def.map(ModuleItem::ExternFunction),
        parse_inductive_type_def.map(ModuleItem::InductiveType),
    ));
    let items = separated(0.., inner, ws_or_comment);
    (
        skip_space("module"),
        qualified_name,
        skip_space("{"),
        items.context(expect("module items")),
        skip_space("}"),
    )
        .map(|(_, name, _, items, _)| {
            let mut functions = Vec::new();
            let mut external_functions = Vec::new();
            let mut inductive_types = Vec::new();
            for item in Vec::into_iter(items) {
                match item {
                    ModuleItem::Function(x) => functions.push(x),
                    ModuleItem::ExternFunction(x) => external_functions.push(x),
                    ModuleItem::InductiveType(x) => inductive_types.push(x),
                }
            }
            Module {
                name,
                functions: functions.into_boxed_slice(),
                external_functions: external_functions.into_boxed_slice(),
                inductive_types: inductive_types.into_boxed_slice(),
            }
        })
        .context(expect("module"))
        .parse_next(i)
}

#[cfg(test)]
mod test {
    use rura_parsing::eol_comment;
    use winnow::ascii::digit0;

    use super::*;
    #[test]
    fn test_eol_comment() {
        let mut input = "// Hello, world!\n";
        eol_comment(&mut input).unwrap();
        assert_eq!(input, "\n");
    }
    #[test]
    fn test_skip_space() {
        let input = r#"
          // Hello, world!
          /* sdad */
          213
          // Hello, world!
        "#;
        let result = skip_space(digit0).parse(input);
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

    #[test]
    fn test_parse_fill_instr() {
        let mut input = r#"fill %1 <- %2;"#;
        let result = parse_fill_instr(&mut input).unwrap();
        assert_eq!(result, Lir::Fill { hole: 1, value: 2 });
    }

    #[test]
    fn test_parse_call_instr() {
        let mut input = r#"%1 = call test(%2, %3);"#;
        let result = parse_call_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::Call(Box::new(FunctionCall {
                result: 1,
                function: QualifiedName::new(Box::new([Ident::new("test")])),
                args: Box::new([2, 3])
            }))
        );
    }

    #[test]
    fn test_parse_ctor_call_instr() {
        let mut input = r#"%1 = new [%2] List @Cons(%4, %6);"#;
        let result = parse_ctor_call_instr(&mut input).unwrap();
        assert_eq!(
            result,
            Lir::CtorCall(Box::new(CtorCall {
                result: 1,
                token: Some(2),
                type_name: QualifiedName::new(Box::new([Ident::new("List")])),
                type_params: Box::new([]),
                ctor: "Cons".into(),
                args: Box::new([4, 6]),
                unique_rc: false,
            }))
        );
    }

    #[test]
    fn test_parse_clone_drop_in_block() {
        let mut input = r#"{
            %1 = clone %2;
            drop %3;
            drop %4;
            drop %5;
        }"#;
        let result = parse_block(&mut input).unwrap();
        assert_eq!(
            result,
            Block(vec![
                Lir::Clone {
                    result: 1,
                    value: 2
                },
                Lir::Drop {
                    value: 3,
                    token: None
                },
                Lir::Drop {
                    value: 4,
                    token: None
                },
                Lir::Drop {
                    value: 5,
                    token: None
                }
            ])
        );
    }

    #[test]
    fn test_parse_bounded_type_var() {
        let mut input = r#"@T: std::TraitFoo + std::TraitBar<Head = ()>"#;
        let result = parse_bounded_type_var(&mut input).unwrap();
        assert_eq!(
            result,
            Bound {
                target: TypeVar::Plain(Ident::new("T")),
                bounds: Box::new([
                    TraitExpr {
                        name: QualifiedName::new(Box::new([
                            Ident::new("std"),
                            Ident::new("TraitFoo")
                        ])),
                        params: Box::new([])
                    },
                    TraitExpr {
                        name: QualifiedName::new(Box::new([
                            Ident::new("std"),
                            Ident::new("TraitBar")
                        ])),
                        params: Box::new([(Some(Ident::new("Head")), LirType::Unit,)])
                    }
                ])
            }
        );
    }

    #[test]
    fn test_parse_function_prototype() {
        let mut test = r#"fn test<T>(%1: i32, %2: f64) -> i32 where @T: std::TraitFoo + std::TraitBar<Head = ()>"#;
        let result = parse_function_prototype(&mut test).unwrap();
        assert_eq!(
            result,
            FunctionPrototype {
                name: QualifiedName::new(Box::new([Ident::new("test")])),
                type_params: Box::new([Ident::new("T")]),
                params: Box::new([
                    (1, LirType::Scalar(ScalarType::I32)),
                    (2, LirType::Scalar(ScalarType::F64))
                ]),
                return_type: LirType::Scalar(ScalarType::I32),
                bounds: Box::new([Bound {
                    target: TypeVar::Plain(Ident::new("T")),
                    bounds: Box::new([
                        TraitExpr {
                            name: QualifiedName::new(Box::new([
                                Ident::new("std"),
                                Ident::new("TraitFoo")
                            ])),
                            params: Box::new([])
                        },
                        TraitExpr {
                            name: QualifiedName::new(Box::new([
                                Ident::new("std"),
                                Ident::new("TraitBar")
                            ])),
                            params: Box::new([(Some(Ident::new("Head")), LirType::Unit,)])
                        }
                    ])
                }])
            }
        );
    }

    #[test]
    fn test_parse_inductive_type_def() {
        let mut test = r#"enum List<T>
            where @T: Foo
        { Nil, Cons(@T, List<@T>) }"#;
        let result = parse_inductive_type_def(&mut test).unwrap();
        assert_eq!(
            result,
            InductiveTypeDef {
                name: QualifiedName::new(Box::new([Ident::new("List")])),
                type_params: Box::new([Ident::new("T")]),
                bounds: Box::new([Bound {
                    target: TypeVar::Plain(Ident::new("T")),
                    bounds: Box::new([TraitExpr {
                        name: QualifiedName::new(Box::new([Ident::new("Foo")])),
                        params: Box::new([])
                    }])
                }]),
                ctors: Box::new([
                    CtorDef {
                        name: "Nil".into(),
                        params: Box::new([])
                    },
                    CtorDef {
                        name: "Cons".into(),
                        params: Box::new([
                            (
                                Member::Index(0),
                                LirType::TypeVar(TypeVar::Plain(Ident::new("T")))
                            ),
                            (
                                Member::Index(1),
                                LirType::Object(
                                    QualifiedName::new(Box::new([Ident::new("List")])),
                                    Box::new([LirType::TypeVar(TypeVar::Plain(Ident::new("T")))])
                                )
                            )
                        ])
                    }
                ])
            }
        );
    }

    const MODULE: &str = r#"
module test {
    enum List<T>
        where @T: Foo
    { Nil, Cons(@T, List<@T>) }
    fn test<T>(%1: i32, %2: f64) -> i32 where @T: std::TraitFoo + std::TraitBar<Head = ()> {
        %3 = constant 3 : i32;
        return %3;
    }
    fn test2() -> i32 {
        %1 = constant 3 : i32;
        return %1;
    }
    fn extern_test<T>(%1: i32, %2: f64) -> i32 where @T: std::TraitFoo + std::TraitBar<Head = ()>;
}
"#;
    #[test]
    fn parse_module_test() {
        let mut input = MODULE;
        let module = parse_module(&mut input).unwrap();
        println!("{:#?}\n", module);
    }
}
