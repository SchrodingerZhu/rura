use std::{fmt::Display, fmt::Formatter};

pub mod lir;
pub mod shape;
pub mod types;

fn fmt_separated<T: Display, P: Display>(
    f: &mut Formatter<'_>,
    args: &[T],
    pat: P,
) -> std::fmt::Result {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            write!(f, "{pat}")?;
        }
        write!(f, "{}", arg)?;
    }
    Ok(())
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident(Box<str>);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QualifiedName(Box<[Box<str>]>);

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_separated(f, &self.0, "::")
    }
}
