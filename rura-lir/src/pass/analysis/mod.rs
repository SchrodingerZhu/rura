#![allow(unused)]
use std::any::Any;

use rura_parsing::QualifiedName;

pub trait AnalysisInfo {
    fn from_json(json: &str) -> Self;
    fn to_json(&self) -> String;
    fn module_level_info(&self) -> Option<&dyn Any> {
        None
    }
    fn function_level_info(&self, name: &QualifiedName) -> Option<&dyn Any> {
        None
    }
    fn operand_level_info(&self, function: &QualifiedName, operand: usize) -> Option<&dyn Any> {
        None
    }
}
