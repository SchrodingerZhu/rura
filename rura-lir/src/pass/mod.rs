use rura_core::lir::ir::Module;

pub mod analysis;
pub mod diagnostic;
pub mod visitor;

pub trait Pass {
    fn get_identifier(&self) -> &str;
}

pub enum BoxedPass<'a> {
    Analysis(Box<dyn analysis::AnalysisPass<'a> + 'a>),
    Diagnostic(Box<dyn diagnostic::DiagnosticPass<'a> + 'a>),
}

pub type PassEntry = for<'a> fn(&'a Module, &'_ toml::Table) -> BoxedPass<'a>;
