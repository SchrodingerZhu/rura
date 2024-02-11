use proc_macro2::Ident;
use syn::{BinOp, TypePath};

/**
 * LIR (Low-level Intermediate Representation) is a low-level intermediate representation designed for `rura`.
 */

pub enum ArithMode {
    Normal,
    Wrapping,
    Saturating,
}

pub struct InductiveEliminator {
    pub ctor: Ident,
    pub is_imcomplete: bool,
    pub args: Vec<(Ident, usize)>,
    pub body: Vec<Lir>,
}

pub struct TupleEliminator {
    pub elements: Vec<usize>,
}

pub enum Lir {
    /// Closure application
    Apply {
        closure: usize,
        arg: usize,
        result: usize,
    },
    /// Binary operations
    BinaryOp {
        /// The binary operation to perform
        op: Box<BinOp>,
        /// Arithmetic mode (only relevant for arithmetic operations)
        mode: ArithMode,
        /// Identifier of the left-hand side operand
        lhs: usize,
        /// Identifier of the right-hand side operand
        rhs: usize,
        /// Identifier of the result
        result: usize,
    },
    /// Function call
    Call {
        /// Identifier of the function to call
        function: usize,
        /// Identifiers of the arguments
        args: Vec<usize>,
        /// Identifier of the result
        result: usize,
    },

    Clone {
        /// Identifier of the value to clone
        value: usize,
        /// Identifier of the result
        result: usize,
    },

    /// Closure creation (todo)
    Closure,

    /// Drop a value
    Drop {
        /// Identifier of the value to drop
        value: usize,
    },

    /// Inductive type creation
    Inductive {
        /// Identifier of the inductive type
        inductive: Box<TypePath>,
        /// Identifier of the constructor
        ctor: Ident,
        /// Identifiers of the arguments
        args: Vec<usize>,
        /// Identifier of the result
        result: usize,
    },

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
    UnaryOp {
        /// The unary operation to perform
        op: syn::UnOp,
        /// Identifier of the operand
        operand: usize,
        /// Identifier of the result
        result: usize,
    },
    Uniquefy {
        /// Identifier of the value to uniquefy
        value: usize,
        /// Identifier of the result
        result: usize,
    },
}
