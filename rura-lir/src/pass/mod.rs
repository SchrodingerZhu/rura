pub mod analysis;
pub mod diagnostic;

pub trait Pass {
    fn get_identifier(&self) -> &str;
}

pub enum BoxedPass {
    Diagnostic(Box<dyn diagnostic::DiagnosticPass>),
}

pub type PassPluginHook = Option<fn() -> BoxedPass>;
