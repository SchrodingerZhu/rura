pub mod analysis;
pub mod diagnostic;
pub mod visitor;

pub trait Pass {
    fn get_identifier(&self) -> &str;
}
