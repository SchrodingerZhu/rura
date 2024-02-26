use std::collections::HashMap;

use crate::pass::visitor::VisitorContext;
use crate::pprint::PrettyPrint;
use crate::QualifiedName;

use crate::types::LirType;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct TypeInference {
    data: HashMap<QualifiedName, HashMap<usize, LirType>>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error<'a> {
    #[error("binary operands %{0} : {1} and %{2} : {3} has inconsistent types")]
    InconsistentBinaryOperands(
        usize,
        PrettyPrint<'a, LirType>,
        usize,
        PrettyPrint<'a, LirType>,
    ),
    #[error("the type of operand %{0} is unknown when being used at this site")]
    UnknownOperandType(usize),
    #[error("argument %{0} has invalid type {1} for a rura function")]
    InvalidArgumentType(usize, PrettyPrint<'a, LirType>),
    #[error("closure %{0} captures an operand %{1} of an imcompatible type {2}")]
    ImcompatibleCapture(usize, usize, PrettyPrint<'a, LirType>),
}

pub struct TypeInferenceAgent<'a> {
    context: Vec<VisitorContext<'a>>,
    errors: Vec<(VisitorContext<'a>, Error<'a>)>,
}
