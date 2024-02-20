use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::quote;
use rura_core::{types::LirType, Ident, Member, QualifiedName};
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
#[derive(Clone, Debug)]
pub enum ScalarConstant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    U128(u128),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
}

impl PartialEq for ScalarConstant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::I8(a), Self::I8(b)) => a == b,
            (Self::I16(a), Self::I16(b)) => a == b,
            (Self::I32(a), Self::I32(b)) => a == b,
            (Self::I64(a), Self::I64(b)) => a == b,
            (Self::ISize(a), Self::ISize(b)) => a == b,
            (Self::I128(a), Self::I128(b)) => a == b,
            (Self::U8(a), Self::U8(b)) => a == b,
            (Self::U16(a), Self::U16(b)) => a == b,
            (Self::U32(a), Self::U32(b)) => a == b,
            (Self::U64(a), Self::U64(b)) => a == b,
            (Self::USize(a), Self::USize(b)) => a == b,
            (Self::U128(a), Self::U128(b)) => a == b,
            (Self::F32(a), Self::F32(b)) => a.to_bits() == b.to_bits(),
            (Self::F64(a), Self::F64(b)) => a.to_bits() == b.to_bits(),
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            _ => false,
        }
    }
}
impl Eq for ScalarConstant {}

impl Hash for ScalarConstant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::I8(a) => a.hash(state),
            Self::I16(a) => a.hash(state),
            Self::I32(a) => a.hash(state),
            Self::I64(a) => a.hash(state),
            Self::ISize(a) => a.hash(state),
            Self::I128(a) => a.hash(state),
            Self::U8(a) => a.hash(state),
            Self::U16(a) => a.hash(state),
            Self::U32(a) => a.hash(state),
            Self::U64(a) => a.hash(state),
            Self::USize(a) => a.hash(state),
            Self::U128(a) => a.hash(state),
            Self::F32(a) => a.to_bits().hash(state),
            Self::F64(a) => a.to_bits().hash(state),
            Self::Bool(a) => a.hash(state),
            Self::Char(a) => a.hash(state),
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
    pub ctor_idx: usize,
    /// Identifiers of the arguments
    pub args: Vec<usize>,
    /// Reuse token
    pub token: Option<usize>,
    /// Identifier of the result
    pub result: usize,
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
    pub args: Vec<usize>,
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
    /// Closure creation (todo)
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

    ConstantScalar {
        value: Box<ScalarConstant>,
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

fn qualified_name(name: &QualifiedName) -> proc_macro2::TokenStream {
    let name = name.iter().map(ident);
    quote! {
        #(::#name)*
    }
}

impl Lir {
    pub fn lower_to_rust(&self) -> proc_macro2::TokenStream {
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
