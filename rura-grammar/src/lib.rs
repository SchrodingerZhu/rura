use winnow::ascii::dec_uint;
use winnow::combinator::{alt, eof, opt, repeat};
use winnow::{PResult, Parser};

use rura_core::ast::{
    DatatypeMembers, Declaration, Definition, EnumDefinition, Expression, FunctionDefinition,
    Matcher, Module, ModuleID, Name, Parameter, QualifiedName, StructDefinition, AST,
};
use rura_core::keywords::{BOTTOM, UNIT};
use rura_core::{Constructor, Error, Input, Span};
use rura_parsing::{
    binary, braced, closure_parameters, constant, constructor, constructor_parameters, elidable,
    elidable_block, elided_block, expect, field, function_arguments, function_parameters,
    function_type, identifier, parenthesized, prefixed, primitive_type, qualified_name,
    reference_type, skip_space, tuple, tuple_type, type_arguments, type_parameters, unary,
};

pub fn module(i: &str, id: ModuleID) -> Result<Module, Error> {
    let mut input = Input::new(i);
    let declarations = declarations(&mut input)?;
    eof(&mut input)?;
    Ok(Module { id, declarations })
}

fn declarations(i: &mut Input) -> PResult<Box<[Declaration<AST>]>> {
    repeat(
        0..,
        skip_space(alt((
            function_declaration,
            enum_declaration,
            struct_declaration,
            nested_submodule_declaration,
            external_submodule_declaration,
        ))),
    )
    .map(|f: Vec<_>| f.into_boxed_slice())
    .parse_next(i)
}

fn function_declaration(i: &mut Input) -> PResult<Declaration<AST>> {
    (
        "fn",
        skip_space(identifier),
        opt(type_parameters.map(Parameter::type_parameters)),
        skip_space(function_parameters(type_expression)),
        opt(prefixed("->", skip_space(type_expression))),
        elidable_block(block_statement, ";"),
    )
        .with_span()
        .map(
            |((_, name, type_parameters, parameters, ret, body), span)| Declaration {
                span: span.clone(),
                definition: Definition::Function(FunctionDefinition {
                    name,
                    type_parameters,
                    parameters,
                    return_type: ret.unwrap_or(AST {
                        span,
                        expr: Box::new(Expression::UnitType),
                    }),
                    body,
                }),
            },
        )
        .context(expect("function declaration"))
        .parse_next(i)
}

fn enum_declaration(i: &mut Input) -> PResult<Declaration<AST>> {
    (
        "enum",
        skip_space(identifier),
        opt(skip_space(type_parameters).map(Parameter::type_parameters)),
        elidable_block(enum_constructors, ";"),
    )
        .with_span()
        .map(|((_, name, type_parameters, members), span)| Declaration {
            span,
            definition: Definition::Enum(EnumDefinition {
                name,
                type_parameters,
                members,
            }),
        })
        .context(expect("enum declaration"))
        .parse_next(i)
}

fn enum_constructors(i: &mut Input) -> PResult<DatatypeMembers<Constructor<Name, AST>>> {
    repeat(1.., elided_block(constructor(type_expression), ","))
        .map(|v: Vec<_>| {
            DatatypeMembers::Unchecked(
                v.into_iter()
                    .map(|c: Constructor<Name, AST>| (c.name.clone(), c))
                    .collect(),
            )
        })
        .parse_next(i)
}

fn struct_declaration(i: &mut Input) -> PResult<Declaration<AST>> {
    (
        "struct",
        skip_space(identifier),
        opt(skip_space(type_parameters).map(Parameter::type_parameters)),
        elidable_block(struct_fields, ";"),
    )
        .with_span()
        .map(|((_, name, type_parameters, members), span)| Declaration {
            span,
            definition: Definition::Struct(StructDefinition {
                name,
                type_parameters,
                members,
            }),
        })
        .context(expect("struct declaration"))
        .parse_next(i)
}

fn struct_fields(i: &mut Input) -> PResult<DatatypeMembers<AST>> {
    repeat(1.., elided_block(field(type_expression), ","))
        .map(|f: Vec<_>| DatatypeMembers::Unchecked(f.into_boxed_slice()))
        .parse_next(i)
}

fn nested_submodule_declaration(i: &mut Input) -> PResult<Declaration<AST>> {
    ("mod", skip_space(qualified_name), braced(declarations))
        .with_span()
        .map(|((_, id, declarations), span)| Declaration {
            span,
            definition: Definition::NestedSubmodule(Module { id, declarations }),
        })
        .context(expect("nested submodule declaration"))
        .parse_next(i)
}

fn external_submodule_declaration(i: &mut Input) -> PResult<Declaration<AST>> {
    (
        "mod",
        elided_block(qualified_name, ";").map(Definition::ExternalSubmodule),
    )
        .with_span()
        .map(|((_, definition), span)| Declaration { span, definition })
        .context(expect("external submodule declaration"))
        .parse_next(i)
}

fn block_statement(i: &mut Input) -> PResult<AST> {
    opt(alt((let_statement, return_statement)))
        .with_span()
        .map(|(v, span)| {
            v.unwrap_or(AST {
                span,
                expr: Box::new(Expression::Unit),
            })
        })
        .context(expect("block statement"))
        .parse_next(i)
}

fn let_statement(i: &mut Input) -> PResult<AST> {
    (
        "let",
        skip_space(identifier),
        opt(prefixed(":", skip_space(type_expression))),
        "=",
        skip_space(value_expression),
        elidable(";"),
        skip_space(block_statement),
    )
        .with_span()
        .map(|((_, name, typ, _, value, _, body), span)| AST {
            span,
            expr: Box::new(Expression::Let {
                name,
                typ,
                value,
                body,
            }),
        })
        .context(expect("let statement"))
        .parse_next(i)
}

fn return_statement(i: &mut Input) -> PResult<AST> {
    value_expression
        .context(expect("return statement"))
        .parse_next(i)
}

fn value_expression(i: &mut Input) -> PResult<AST> {
    alt((
        binary(primary_value_expression, value_expression),
        unary(value_expression),
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

fn closure(i: &mut Input) -> PResult<AST> {
    (
        closure_parameters,
        skip_space(alt((braced(block_statement), value_expression))),
    )
        .with_span()
        .map(|((parameters, body), span)| AST {
            span,
            expr: Box::new(Expression::Closure { parameters, body }),
        })
        .context(expect("closure expression"))
        .parse_next(i)
}

fn indexing(i: &mut Input) -> PResult<AST> {
    (primary_value_expression, skip_space("."), dec_uint)
        .with_span()
        .map(|((tuple, _, index), span)| AST {
            span,
            expr: Box::new(Expression::Indexing { tuple, index }),
        })
        .context(expect("tuple indexing expression"))
        .parse_next(i)
}

fn call(i: &mut Input) -> PResult<AST> {
    (
        primary_value_expression,
        repeat(
            1..,
            skip_space((
                opt(prefixed("::", type_arguments(type_expression))),
                function_arguments(value_expression),
            )),
        ),
    )
        .with_span()
        .map(|((f, args), span): ((AST, Vec<_>), Span)| {
            args.into_iter()
                .fold(f, |f, (type_arguments, arguments)| AST {
                    span: span.clone(),
                    expr: Box::new(Expression::Call {
                        callee: f,
                        type_arguments,
                        arguments,
                    }),
                })
        })
        .context(expect("function call"))
        .parse_next(i)
}

fn if_then_else(i: &mut Input) -> PResult<AST> {
    (
        "if",
        skip_space(value_expression),
        braced(block_statement),
        skip_space("else"),
        braced(block_statement),
    )
        .with_span()
        .map(|((_, condition, then_branch, _, else_branch), span)| AST {
            span,
            expr: Box::new(Expression::IfThenElse {
                condition,
                then_branch,
                else_branch,
            }),
        })
        .context(expect("if expression"))
        .parse_next(i)
}

fn matching(i: &mut Input) -> PResult<AST> {
    (
        "match",
        skip_space(value_expression),
        elidable_block(matchers, ";"),
    )
        .with_span()
        .map(|((_, argument, matchers), span)| AST {
            span,
            expr: Box::new(Expression::Matching { argument, matchers }),
        })
        .context(expect("match expression"))
        .parse_next(i)
}

fn matchers(i: &mut Input) -> PResult<Box<[Matcher]>> {
    repeat(1.., matcher)
        .map(|v: Vec<_>| v.into_boxed_slice())
        .parse_next(i)
}

fn matcher(i: &mut Input) -> PResult<Matcher> {
    (
        skip_space(qualified_name),
        opt(skip_space(constructor_parameters)),
        "=>",
        skip_space((
            alt((braced(block_statement), value_expression)),
            elidable(","),
        )),
    )
        .map(|(constructor, arguments, _, (body, _))| Matcher {
            constructor,
            arguments,
            body,
        })
        .context(expect("matcher"))
        .parse_next(i)
}

fn access(i: &mut Input) -> PResult<AST> {
    (
        skip_space(primary_value_expression),
        repeat(1.., prefixed(".", identifier::<Name>)),
    )
        .with_span()
        .map(|((arg, ns), span): ((AST, Vec<_>), Span)| {
            ns.into_iter().fold(arg, |argument, name| AST {
                span: span.clone(),
                expr: Box::from(Expression::Access { argument, name }),
            })
        })
        .context(expect("access expression"))
        .parse_next(i)
}

fn primary_value_expression(i: &mut Input) -> PResult<AST> {
    alt((
        parenthesized(value_expression),
        tuple(value_expression).with_span().map(|(v, span)| AST {
            span,
            expr: Box::new(Expression::Tuple(v)),
        }),
        constant.with_span().map(From::from),
        qualified_name::<Name, QualifiedName>
            .with_span()
            .map(From::from),
    ))
    .context(expect("primary value expression"))
    .parse_next(i)
}

fn type_expression(i: &mut Input) -> PResult<AST> {
    alt((
        UNIT.with_span().map(|(_, span)| AST {
            span,
            expr: Box::new(Expression::UnitType),
        }),
        BOTTOM.with_span().map(|(_, span)| AST {
            span,
            expr: Box::new(Expression::BottomType),
        }),
        primitive_type.with_span().map(|(p, span)| AST {
            span,
            expr: Box::new(Expression::PrimitiveType(p)),
        }),
        tuple_type(type_expression).with_span().map(From::from),
        function_type(type_expression).with_span().map(From::from),
        reference_type(type_expression)
            .with_span()
            .map(|(t, span)| AST {
                span,
                expr: Box::new(Expression::ReferenceType(t)),
            }),
        unique_type,
        type_call,
        qualified_name::<Name, QualifiedName>
            .with_span()
            .map(From::from),
        parenthesized(type_expression),
    ))
    .context(expect("type expression"))
    .parse_next(i)
}

fn unique_type(i: &mut Input) -> PResult<AST> {
    prefixed("!", type_expression)
        .with_span()
        .map(|(t, span)| AST {
            span,
            expr: Box::new(Expression::UniqueType(t)),
        })
        .context(expect("unique type"))
        .parse_next(i)
}

fn type_call(i: &mut Input) -> PResult<AST> {
    (qualified_name, "::", type_arguments(type_expression))
        .with_span()
        .map(|((qn, _, type_arguments), span)| AST {
            span: span.clone(),
            expr: Box::new(Expression::TypeCall {
                type_callee: AST {
                    span,
                    expr: Box::new(Expression::Variable(qn)),
                },
                type_arguments,
            }),
        })
        .context(expect("type call expression"))
        .parse_next(i)
}

#[cfg(test)]
mod tests;
