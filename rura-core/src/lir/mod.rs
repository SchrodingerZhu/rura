/**
 * LIR (Low-level Intermediate Representation) is a low-level intermediate representation designed for `rura`.
 */
use std::rc::Rc;

pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
    BitNot,
    ArithShl,
    LogicNot,
}
