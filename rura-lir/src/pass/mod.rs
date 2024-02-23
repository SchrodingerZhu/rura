pub mod diagnostic;

pub trait Pass {
    const IDENTIFIER: &'static str;
}
