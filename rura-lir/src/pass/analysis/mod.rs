#![allow(unused)]
use std::{any::Any, fmt::Write};

use rura_parsing::QualifiedName;

pub mod free_variable;
pub mod type_inference;

pub trait AnalysisInfo {
    type ModuleInfo;
    type FunctionInfo;
    type OperandInfo;
    type TypeInfo;

    fn deserialize(data: &[u8]) -> Self;
    fn serialize(&self) -> Vec<u8>;
    fn module_info(&self) -> &Self::ModuleInfo;
    fn function_info(&self, name: &QualifiedName) -> &Self::FunctionInfo;
    fn operand_info(&self, func: &QualifiedName, operand: usize) -> &Self::OperandInfo;
    fn type_info(&self, name: &QualifiedName) -> &Self::TypeInfo;
}
