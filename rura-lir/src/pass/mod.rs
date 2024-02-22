pub mod diagnostic;

pub enum PassType {
    // Generate diagnostic information
    Diagnostic,
    // Attach new information to the LIR module
    Analysis,
    // Transform the LIR module
    Transform,
}

pub trait Pass {
    const IDENTIFIER: &'static str;
    const TYPE: PassType;
}
