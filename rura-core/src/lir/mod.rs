use proc_macro2::{Ident, TokenStream};
use quote::quote;
/**
 * LIR (Low-level Intermediate Representation) is a low-level intermediate representation designed for `rura`.
 */

#[repr(transparent)]
pub struct Block(pub Vec<Lir>);

pub enum ArithMode {
    Normal,
    Wrapping,
    Saturating,
}

pub struct InductiveEliminator {
    pub ctor: Ident,
    pub is_imcomplete: bool,
    pub args: Vec<(Ident, usize)>,
    pub body: Block,
}

pub struct TupleEliminator {
    pub elements: Vec<usize>,
}

pub struct CtorCall {
    /// Identifier of the inductive type
    pub type_path: syn::Path,
    /// Type parameters
    pub type_params: Vec<syn::Path>,
    /// Identifier of the constructor
    pub ctor: Ident,
    /// Identifiers of the arguments
    pub args: Vec<usize>,
    /// Reuse token
    pub token: Option<usize>,
    /// Identifier of the result
    pub result: usize,
}

pub struct BinaryOp {
    /// The binary operation to perform
    pub op: syn::BinOp,
    /// Arithmetic mode (only relevant for arithmetic operations)
    pub mode: ArithMode,
    /// Identifier of the left-hand side operand
    pub lhs: usize,
    /// Identifier of the right-hand side operand
    pub rhs: usize,
    /// Identifier of the result
    pub result: usize,
}

pub struct UnaryOp {
    /// The unary operation to perform
    pub op: syn::UnOp,
    /// Identifier of the operand
    pub operand: usize,
    /// Identifier of the result
    pub result: usize,
}

pub struct FunctionCall {
    /// Identifier of the function to call
    pub function: syn::Path,
    /// Identifiers of the arguments
    pub args: Vec<usize>,
    /// Identifier of the result
    pub result: usize,
}

pub struct IfThenElse {
    /// Identifier of the condition
    pub condition: usize,
    /// Identifier of the then branch
    pub then_branch: Block,
    /// Identifier of the else branch
    pub else_branch: Block,
    /// Identifier of the result
    pub result: usize,
}

pub struct ClosureCreation {
    /// parameters
    pub parameters: Vec<syn::Path>,
    /// values to capture
    pub capture: Vec<usize>,
    /// body
    pub body: Block,
    /// Identifier of the result
    pub result: usize,
}

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
        eliminator: Vec<InductiveEliminator>,
    },

    InplaceUpdate {
        /// Identifier of the value to update
        value: usize,
        /// Identifier of the index
        assignments: Vec<(Ident, usize)>,
        /// Identifier of the result
        result: usize,
    },

    Ref {
        /// Identifier of the value to reference
        value: usize,
        /// Identifier of the result
        result: usize,
    },

    /// Return a value
    Return {
        /// Identifier of the value to return
        value: usize,
    },

    /// Tuple creation
    Tuple {
        /// Identifiers of the elements
        elements: Vec<usize>,
        /// Identifier of the result
        result: usize,
    },

    TupleElimination {
        /// Identifier of the tuple
        tuple: usize,
        /// Identifier of the eliminator
        eliminator: TupleEliminator,
    },
    /// Unary operations
    UnaryOp(Box<UnaryOp>),

    /// Uniquefy a value
    Uniquefy {
        /// Identifier of the value to uniquefy
        value: usize,
        /// Identifier of the result
        result: usize,
    },

    /// If-then-else
    IfThenElse(Box<IfThenElse>),
}

fn variable(id: usize) -> proc_macro2::Ident {
    proc_macro2::Ident::new(&format!("χ{}", id), proc_macro2::Span::call_site())
}

impl Lir {
    pub fn lower_to_rust(&self) -> TokenStream {
        match self {
            Lir::Uniquefy { value, result } => {
                let value = variable(*value);
                let result = variable(*result);
                quote! {
                    let #result = #value.uniquefy();
                }
            }
            Lir::Ref { value, result } => {
                let value = variable(*value);
                let result = variable(*result);
                quote! {
                    let #result = &#value;
                }
            }
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
            Lir::Tuple { elements, result } => {
                let elements = elements.iter().copied().map(variable);
                let result = variable(*result);
                quote! {
                    let #result = (#( #elements ),*);
                }
            }
            Lir::InplaceUpdate {
                value,
                assignments,
                result,
            } => {
                let value = variable(*value);
                let assignments = assignments.iter().map(|(field, id)| {
                    let field = field.clone();
                    let id = variable(*id);
                    quote! { mutable_ref.#field = #id; }
                });
                let result = variable(*result);
                quote! {
                    let #result = {
                      let mut #value = #value;
                      let mutable_ref = #value.make_mut();
                      #( #assignments )*
                      #value
                    };
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
        well_formed_lower(Lir::Tuple {
            elements: vec![1, 2],
            result: 3,
        });
    }

    #[test]
    fn test_inplace_update_lower_to_rust() {
        well_formed_lower(Lir::InplaceUpdate {
            value: 1,
            assignments: vec![(Ident::new("foo", proc_macro2::Span::call_site()), 2)],
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
}
