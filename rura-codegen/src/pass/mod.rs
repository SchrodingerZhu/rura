pub mod diagnostic;

pub enum PassType {
    // Generate diagnotic information
    Diagnostic,
    // Attach new information to the LIR module
    Analysis,
    // Transform the LIR module
    Transform,
}

pub trait Pass {
    const IDENTIFER: &'static str;
    const TYPE: PassType;
}
