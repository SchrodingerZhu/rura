use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::quote;

use rura_parsing::Constant;

use crate::types::{LirType, TypeVar};
use crate::{Ident, Member, QualifiedName};

/**
 * LIR (Low-level Intermediate Representation) is a low-level intermediate representation designed for `rura`.
 */

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block(pub Vec<Lir>);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArithMode {
    Default,
    Wrapping,
    Saturating,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MakeMutReceiver {
    pub target: Member,
    pub hole: usize,
    pub value: usize,
}

impl MakeMutReceiver {
    pub fn is_named(&self) -> bool {
        matches!(self.target, Member::Named(_),)
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EliminationStyle {
    /// Destruct the pattern into values and memory token.
    /// All values must be captured by some variable.
    Unwrap {
        fields: Box<[(Member, usize)]>,
        token: usize,
    },
    /// Prepare the pattern for inplace mutation.
    /// Partial field capture is allowed.
    Mutation(Box<[MakeMutReceiver]>),
    /// Obtain the original value
    Fixpoint(usize),
    /// Get reference of fields
    /// Partial field capture is allowed.
    Ref(Box<[(Member, usize)]>),
}

fn member_list(members: &[(Member, usize)]) -> TokenStream {
    let fields = members.iter().map(|(member, value)| match member {
        Member::Named(name) => {
            let name = ident(name);
            let value = variable(*value);
            quote! { #name : #value }
        }
        Member::Index(_) => {
            let value = variable(*value);
            quote! { #value }
        }
    });
    if matches!(members[0].0, Member::Named(_)) {
        quote! {
            { #(#fields),* }
        }
    } else {
        quote! {
            ( #(#fields),* )
        }
    }
}

fn make_mut_receivers(holes: &[MakeMutReceiver]) -> TokenStream {
    let fields = holes.iter().map(|hole| match hole {
        MakeMutReceiver {
            target: Member::Named(name),
            value,
            ..
        } => {
            let name = ident(name);
            let value = variable(*value);
            quote! { #name: ref mut #value }
        }
        MakeMutReceiver {
            target: Member::Index(_),
            value,
            ..
        } => {
            let value = variable(*value);
            quote! { ref mut #value }
        }
    });
    if holes[0].is_named() {
        quote! {
            { #(#fields,)* .. }
        }
    } else {
        quote! {
            ( #(#fields,)* .. )
        }
    }
}

fn hole_declarations(holes: &[MakeMutReceiver]) -> impl Iterator<Item = TokenStream> + '_ {
    holes.iter().map(|hole| {
        let MakeMutReceiver { hole, value, .. } = hole;

        let value = variable(*value);
        let hole = variable(*hole);
        quote! {
            let (#hole, #value) = ::rura_runtime::Hole::new(#value);
        }
    })
}

impl EliminationStyle {
    pub fn lower_to_rust(&self, old_value: usize, ctor: &QualifiedName) -> TokenStream {
        match self {
            Self::Fixpoint(x) => {
                let x = variable(*x);
                let old_value = variable(old_value);
                quote! {
                    let #x = #old_value;
                }
            }
            Self::Unwrap { fields, token } => {
                let qualified_name = qualified_name(ctor);
                let fields = member_list(fields);
                let token = variable(*token);
                let old_value = variable(old_value);
                quote! {
                    let (#token, #qualified_name #fields) = #old_value.unwrap_for_reuse() else {
                        unsafe { ::core::hint::unreachable_unchecked() }
                    };
                }
            }
            Self::Mutation(holes) => {
                let qualified_name = qualified_name(ctor);
                let old_value = variable(old_value);
                let fields = make_mut_receivers(holes);
                let holes = hole_declarations(holes);
                quote! {
                    let mut #old_value = #old_value;
                    let #qualified_name #fields = *#old_value.make_mut() else {
                        unsafe { ::core::hint::unreachable_unchecked() }
                    };
                    #(#holes)*
                }
            }
            Self::Ref(..) => todo!("Ref"),
        }
    }
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InductiveEliminator {
    pub ctor: QualifiedName,
    pub style: EliminationStyle,
    pub body: Block,
}

impl InductiveEliminator {
    pub fn lower_to_rust(&self, old_value: usize) -> TokenStream {
        let ctor = qualified_name(&self.ctor);
        let body = self.body.0.iter().map(|lir| lir.lower_to_rust());
        let header = self.style.lower_to_rust(old_value, &self.ctor);
        quote! {
            #ctor { .. } => {
                #header
                #(#body)*
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CtorCall {
    /// Identifier of the inductive type
    pub type_name: QualifiedName,
    /// Type parameters
    pub type_params: Box<[LirType]>,
    /// Identifier of the constructor
    pub ctor: Ident,
    /// Identifiers of the arguments
    pub args: Box<[usize]>,
    /// Reuse token
    pub token: Option<usize>,
    /// Identifier of the result
    pub result: usize,
    /// Create unique rc instead of normal Rc (should be used for optimization only)
    pub unique_rc: bool,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,
    Not,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BinaryOp {
    /// The binary operation to perform
    pub op: BinOp,
    /// Arithmetic mode (only relevant for arithmetic operations)
    pub mode: Option<ArithMode>,
    /// Identifier of the left-hand side operand
    pub lhs: usize,
    /// Identifier of the right-hand side operand
    pub rhs: usize,
    /// Identifier of the result
    pub result: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UnaryOp {
    /// The unary operation to perform
    pub op: UnOp,
    /// Identifier of the operand
    pub operand: usize,
    /// Identifier of the result
    pub result: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    /// Identifier of the function to call
    pub function: QualifiedName,
    /// Identifiers of the arguments
    pub args: Box<[usize]>,
    /// Identifier of the result
    pub result: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IfThenElse {
    /// Identifier of the condition
    pub condition: usize,
    /// Identifier of the then branch
    pub then_branch: Block,
    /// Identifier of the else branch
    pub else_branch: Block,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClosureCreation {
    /// parameters
    pub params: Box<[(usize, LirType)]>,
    /// return type
    pub return_type: LirType,
    /// body
    pub body: Block,
    /// Identifier of the result
    pub result: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lir {
    /// Closure application
    Apply {
        closure: usize,
        arg: usize,
        result: usize,
    },
    /// Binary operations
    BinaryOp(Box<BinaryOp>),
    /// Function call
    Call(Box<FunctionCall>),
    Clone {
        /// Identifier of the value to clone
        value: usize,
        /// Identifier of the result
        result: usize,
    },
    /// Closure creation
    Closure(Box<ClosureCreation>),

    /// Drop a value
    Drop {
        /// Identifier of the value to drop
        value: usize,
        /// Produce a reuse token
        token: Option<usize>,
    },

    /// Inductive type creation
    CtorCall(Box<CtorCall>),

    InductiveElimination {
        /// Identifier of the inductive type
        inductive: usize,
        /// Identifier of the eliminator
        eliminator: Box<[InductiveEliminator]>,
    },

    /// Return a value
    Return {
        /// Identifier of the value to return
        value: usize,
    },

    /// Tuple creation
    TupleIntro {
        /// Identifiers of the elements
        elements: Box<[usize]>,
        /// Identifier of the result
        result: usize,
    },

    TupleElim {
        /// Identifier of the tuple
        tuple: usize,
        /// Identifier of the eliminator
        eliminator: Box<[usize]>,
    },
    /// Unary operations
    UnaryOp(Box<UnaryOp>),

    /// If-then-else
    IfThenElse(Box<IfThenElse>),

    Constant {
        value: Box<Constant>,
        result: usize,
    },

    // Hole filling
    Fill {
        hole: usize,
        value: usize,
    },

    /// Convert a normal function to a closure
    Curry {
        function: QualifiedName,
        result: usize,
    },
}

fn variable(id: usize) -> proc_macro2::Ident {
    let id = format!("χ{}", id);
    proc_macro2::Ident::new(&id, proc_macro2::Span::call_site())
}

fn ident(id: &Ident) -> proc_macro2::Ident {
    let id = id.as_ref();
    proc_macro2::Ident::new(id, proc_macro2::Span::call_site())
}

fn qualified_name(name: &QualifiedName) -> TokenStream {
    let name = name.iter().map(ident);
    quote! {
        #(::#name)*
    }
}

macro_rules! box_iter {
    ($($x:expr),*) => {
        Box::new([$($x),*].into_iter())
    };
    (@ $x:expr) => {
        Box::new($x)
    };
}

impl ClosureCreation {
    pub fn free_operands(&self) -> Vec<usize> {
        todo!()
    }
}

impl Lir {
    pub fn defining_operand<'a>(&'a self) -> Box<dyn Iterator<Item = usize> + 'a> {
        match self {
            Lir::Apply { result, .. } => box_iter![*result],
            Lir::BinaryOp(inner) => box_iter![inner.result],
            Lir::Call(inner) => box_iter![inner.result],
            Lir::Clone { result, .. } => box_iter![*result],
            Lir::Closure(inner) => box_iter![inner.result],
            Lir::Drop { token, .. } => box_iter!(@ (*token).into_iter()),
            Lir::CtorCall(inner) => box_iter![inner.result],
            Lir::InductiveElimination { .. } => box_iter![],
            Lir::Return { .. } => box_iter![],
            Lir::TupleIntro { result, .. } => box_iter![*result],
            Lir::TupleElim { eliminator, .. } => box_iter!(@ eliminator.iter().copied()),
            Lir::UnaryOp(inner) => box_iter![inner.result],
            Lir::IfThenElse(..) => box_iter![],
            Lir::Constant { result, .. } => box_iter![*result],
            Lir::Fill { .. } => box_iter![],
            Lir::Curry { result, .. } => box_iter![*result],
        }
    }
    pub fn using_operands<'a>(&'a self) -> Box<dyn Iterator<Item = usize> + 'a> {
        match self {
            Lir::Apply { closure, arg, .. } => box_iter![*closure, *arg],
            Lir::BinaryOp(inner) => box_iter![inner.lhs, inner.rhs],
            Lir::Call(inner) => box_iter!(@ inner.args.iter().copied()),
            Lir::Clone { value, .. } => box_iter![*value],
            Lir::Closure(inner) => box_iter!(@ inner.free_operands().into_iter()),
            Lir::Drop { value, .. } => box_iter![*value],
            Lir::CtorCall(inner) => box_iter!(@ inner.args.iter().copied()),
            Lir::InductiveElimination { inductive, .. } => box_iter![*inductive],
            Lir::Return { value } => box_iter![*value],
            Lir::TupleIntro { elements, .. } => box_iter!(@ elements.iter().copied()),
            Lir::TupleElim { tuple, .. } => box_iter![*tuple],
            Lir::UnaryOp(inner) => box_iter![inner.operand],
            Lir::IfThenElse(inner) => box_iter![inner.condition],
            Lir::Constant { .. } => box_iter![],
            Lir::Fill { hole, value } => box_iter![*hole, *value],
            Lir::Curry { .. } => box_iter![],
        }
    }

    pub fn lower_to_rust(&self) -> TokenStream {
        match self {
            Lir::Drop { value, token } => {
                let value = variable(*value);
                let token = token.map(variable);
                match token {
                    Some(token) => quote! { let #token = #value.drop_for_reuse(); },
                    None => quote! { drop(#value); },
                }
            }
            Lir::Return { value } => {
                let value = variable(*value);
                quote! { return #value; }
            }
            Lir::TupleIntro { elements, result } => {
                let elements = elements.iter().copied().map(variable);
                let result = variable(*result);
                quote! {
                    let #result = (#( #elements ),*);
                }
            }
            Lir::InductiveElimination {
                inductive,
                eliminator,
            } => {
                let arms = eliminator.iter().map(|elim| elim.lower_to_rust(*inductive));
                let inductive = variable(*inductive);
                quote! {
                    match #inductive {
                        #(#arms)*
                    }
                }
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitExpr {
    pub name: QualifiedName,
    pub params: Box<[(Option<Ident>, LirType)]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Bound {
    pub target: TypeVar,
    pub bounds: Box<[TraitExpr]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionPrototype {
    pub name: QualifiedName,
    pub type_params: Box<[Ident]>,
    pub bounds: Box<[Bound]>,
    pub params: Box<[(usize, LirType)]>,
    pub return_type: LirType,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionDef {
    pub prototype: FunctionPrototype,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CtorDef {
    pub name: Ident,
    pub params: Box<[(Member, LirType)]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InductiveTypeDef {
    pub name: QualifiedName,
    pub type_params: Box<[Ident]>,
    pub bounds: Box<[Bound]>,
    pub ctors: Box<[CtorDef]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    pub name: QualifiedName,
    pub functions: Box<[FunctionDef]>,
    pub external_functions: Box<[FunctionPrototype]>,
    pub inductive_types: Box<[InductiveTypeDef]>,
}

#[cfg(test)]
mod tests {
    use syn::File;

    use super::*;

    fn well_formed_lower(input: Lir) {
        let input = input.lower_to_rust();
        let f: File = syn::parse_quote! {
            fn main() {
                #input
            }
        };
        let output = prettyplease::unparse(&f);
        println!("{}", output);
    }

    #[test]
    fn test_tuple_lower_to_rust() {
        well_formed_lower(Lir::TupleIntro {
            elements: vec![1, 2].into_boxed_slice(),
            result: 3,
        });
    }

    #[test]
    fn test_return_lower_to_rust() {
        well_formed_lower(Lir::Return { value: 1 });
    }

    #[test]
    fn test_drop_lower_to_rust() {
        well_formed_lower(Lir::Drop {
            value: 1,
            token: Some(2),
        });

        well_formed_lower(Lir::Drop {
            value: 1,
            token: None,
        });
    }

    #[test]
    fn test_inductive_elimination_fixpoint_lower_to_rust() {
        well_formed_lower(Lir::InductiveElimination {
            inductive: 1,
            eliminator: vec![InductiveEliminator {
                ctor: QualifiedName::new(Box::new([Ident::new("Some")])),
                style: EliminationStyle::Fixpoint(2),
                body: Block(vec![Lir::Return { value: 2 }]),
            }]
            .into_boxed_slice(),
        });
    }

    #[test]
    fn test_inductive_elimination_unwrap_lower_to_rust() {
        well_formed_lower(Lir::InductiveElimination {
            inductive: 1,
            eliminator: vec![InductiveEliminator {
                ctor: QualifiedName::new(Box::new([Ident::new("Some")])),
                style: EliminationStyle::Unwrap {
                    fields: vec![(Member::Named(Ident::new("x")), 2)].into_boxed_slice(),
                    token: 3,
                },
                body: Block(vec![]),
            }]
            .into_boxed_slice(),
        });
    }

    #[test]
    fn test_inductive_elimination_mutation_lower_to_rust() {
        well_formed_lower(Lir::InductiveElimination {
            inductive: 1,
            eliminator: vec![InductiveEliminator {
                ctor: QualifiedName::new(Box::new([Ident::new("Some")])),
                style: EliminationStyle::Mutation(
                    vec![MakeMutReceiver {
                        target: Member::Named(Ident::new("x")),
                        hole: 2,
                        value: 3,
                    }]
                    .into_boxed_slice(),
                ),
                body: Block(vec![Lir::Return { value: 4 }]),
            }]
            .into_boxed_slice(),
        });
    }
}
