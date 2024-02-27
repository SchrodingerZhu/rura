#![allow(unused_variables, dead_code, private_interfaces)] // FIXME

use rura_core::ast::{Declaration, Definition, Enum, File, Matcher, Name, Parameter, Struct, AST};
use winnow::ascii::dec_uint;
use winnow::combinator::{alt, opt, repeat, separated};
use winnow::{PResult, Parser};

use rura_core::keywords::{BOTTOM, UNIT};
use rura_parsing::{
    binary, braced, closure_parameters, constant, constructor, constructor_parameters, elidable,
    elidable_block, expect, field, function_parameters, function_type, identifier, parenthesized,
    prefixed, primitive_type, qualified_name, reference_type, skip_space, tuple, tuple_type,
    type_arguments, type_parameters, unary, unique_type,
};

pub fn file(i: &mut &str) -> PResult<File> {
    repeat(
        0..,
        skip_space(alt((
            function_declaration,
            enum_declaration,
            struct_declaration,
        ))),
    )
    .map(|f: Vec<_>| f.into_boxed_slice())
    .parse_next(i)
}

fn function_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "fn",
        skip_space(identifier),
        opt(type_parameters).map(|ps| ps.map(Parameter::type_parameters)),
        skip_space(function_parameters(type_expression)),
        opt(prefixed("->", skip_space(type_expression)))
            .map(|t| Box::new(t.unwrap_or(AST::UnitType))),
        elidable_block(function_definition, ";"),
    )
        .map(
            |(_, name, type_parameters, parameters, return_type, definition)| Declaration {
                name,
                type_parameters,
                parameters,
                return_type,
                definition,
            },
        )
        .context(expect("function declaration"))
        .parse_next(i)
}

fn function_definition(i: &mut &str) -> PResult<Definition<AST>> {
    block_statement
        .map(|e| Definition::Function(Box::new(e)))
        .parse_next(i)
}

fn enum_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "enum",
        skip_space(identifier),
        opt(type_parameters),
        elidable_block(enum_definition, ";"),
    )
        .map(|(_, name, types, def)| Declaration::type_declaration(name, types, def))
        .context(expect("enum declaration"))
        .parse_next(i)
}

fn enum_definition(i: &mut &str) -> PResult<Definition<AST>> {
    separated(1.., skip_space(constructor(type_expression)), elidable(","))
        .map(|ctors: Vec<_>| Definition::Enum(Enum::Unchecked(ctors.into_boxed_slice())))
        .parse_next(i)
}

fn struct_declaration(i: &mut &str) -> PResult<Declaration<AST>> {
    (
        "struct",
        skip_space(identifier),
        opt(type_parameters),
        elidable_block(struct_definition, ";"),
    )
        .map(|(_, name, types, def)| Declaration::type_declaration(name, types, def))
        .context(expect("struct declaration"))
        .parse_next(i)
}

fn struct_definition(i: &mut &str) -> PResult<Definition<AST>> {
    separated(1.., skip_space(field(type_expression)), elidable(","))
        .map(|f: Vec<_>| Definition::Struct(Struct::Unchecked(f.into_boxed_slice())))
        .parse_next(i)
}

fn block_statement(i: &mut &str) -> PResult<AST> {
    alt((let_statement, return_statement))
        .context(expect("block statement"))
        .parse_next(i)
}

fn let_statement(i: &mut &str) -> PResult<AST> {
    (
        "let",
        skip_space(identifier),
        opt(prefixed(":", skip_space(type_expression))).map(|t| t.map(Box::new)),
        "=",
        value_expression.map(Box::new),
        elidable(";"),
        block_statement.map(Box::new),
    )
        .map(|(_, name, typ, _, value, _, body)| AST::Let {
            name,
            typ,
            value,
            body,
        })
        .context(expect("let statement"))
        .parse_next(i)
}

fn return_statement(i: &mut &str) -> PResult<AST> {
    value_expression
        .context(expect("return statement"))
        .parse_next(i)
}

fn value_expression(i: &mut &str) -> PResult<AST> {
    alt((
        unary(value_expression),
        binary(value_expression),
        closure,
        indexing,
        call,
        if_then_else,
        matching,
        access,
        primary_value_expression,
    ))
    .context(expect("value expression"))
    .parse_next(i)
}

fn closure(i: &mut &str) -> PResult<AST> {
    (
        closure_parameters,
        alt((braced(block_statement), value_expression)).map(Box::new),
    )
        .map(|(parameters, body)| AST::Closure { parameters, body })
        .context(expect("closure expression"))
        .parse_next(i)
}

fn indexing(i: &mut &str) -> PResult<AST> {
    (
        alt((call, primary_value_expression)).map(Box::new),
        ".",
        dec_uint,
    )
        .map(|(tuple, _, index)| AST::Indexing { tuple, index })
        .context(expect("tuple indexing expression"))
        .parse_next(i)
}

fn call(i: &mut &str) -> PResult<AST> {
    (
        primary_value_expression,
        repeat(
            1..,
            skip_space((
                opt(prefixed("::", type_arguments(type_expression))),
                tuple(value_expression),
            )),
        ),
    )
        .map(|(f, args): (AST, Vec<_>)| {
            args.into_iter()
                .fold(f, |f, (type_arguments, arguments)| AST::Call {
                    callee: Box::new(f),
                    type_arguments,
                    arguments,
                })
        })
        .context(expect("function call"))
        .parse_next(i)
}

fn if_then_else(i: &mut &str) -> PResult<AST> {
    (
        "if",
        skip_space(value_expression).map(Box::new),
        skip_space(block_statement).map(Box::new),
        "else",
        skip_space(block_statement).map(Box::new),
    )
        .map(
            |(_, condition, then_branch, _, else_branch)| AST::IfThenElse {
                condition,
                then_branch,
                else_branch,
            },
        )
        .context(expect("if expression"))
        .parse_next(i)
}

fn matching(i: &mut &str) -> PResult<AST> {
    (
        "match",
        skip_space(value_expression).map(Box::new),
        elidable_block(matchers, ";"),
    )
        .map(|(_, argument, matchers)| AST::Matching { argument, matchers })
        .context(expect("match expression"))
        .parse_next(i)
}

fn matchers(i: &mut &str) -> PResult<Box<[Matcher]>> {
    repeat(1.., matcher)
        .map(|v: Vec<_>| v.into_boxed_slice())
        .parse_next(i)
}

fn matcher(i: &mut &str) -> PResult<Matcher> {
    (
        skip_space(qualified_name),
        opt(skip_space(constructor_parameters())),
        "=>",
        elidable_block(block_statement, ","),
    )
        .map(|(constructor, arguments, _, body)| Matcher {
            constructor,
            arguments,
            body,
        })
        .context(expect("matcher"))
        .parse_next(i)
}

fn access(i: &mut &str) -> PResult<AST> {
    (
        primary_value_expression,
        repeat(1.., prefixed(".", identifier::<Name>)),
    )
        .map(|(arg, ns): (AST, Vec<_>)| {
            ns.into_iter().fold(arg, |a, name| AST::Access {
                argument: Box::new(a),
                name,
            })
        })
        .context(expect("access expression"))
        .parse_next(i)
}

fn primary_value_expression(i: &mut &str) -> PResult<AST> {
    alt((
        constant.map(AST::Constant),
        qualified_name.map(AST::Variable),
        tuple(value_expression).map(AST::Tuple),
        parenthesized(value_expression),
    ))
    .context(expect("primary value expression"))
    .parse_next(i)
}

fn type_expression(i: &mut &str) -> PResult<AST> {
    alt((
        UNIT.map(|_| AST::UnitType),
        BOTTOM.map(|_| AST::BottomType),
        primitive_type.map(AST::PrimitiveType),
        tuple_type(type_expression),
        function_type(type_expression),
        reference_type(type_expression).map(AST::ReferenceType),
        unique_type(type_expression).map(AST::UniqueType),
        type_call,
        qualified_name.map(AST::Variable),
        parenthesized(type_expression),
    ))
    .context(expect("type expression"))
    .parse_next(i)
}

fn type_call(i: &mut &str) -> PResult<AST> {
    (qualified_name, "::", type_arguments(type_expression))
        .map(|(qn, _, type_arguments)| AST::TypeCall {
            type_callee: Box::new(AST::Variable(qn)),
            type_arguments,
        })
        .context(expect("type call expression"))
        .parse_next(i)
}

#[cfg(test)]
mod tests;
