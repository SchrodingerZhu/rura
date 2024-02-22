use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::{Display, Formatter},
    hash::Hash,
};

pub mod lir;
pub mod parser;
pub mod pass;
pub mod pprint;
pub mod shape;
pub mod types;

fn fmt_separated<T: Display, P: Display, I>(
    f: &mut Formatter<'_>,
    args: I,
    pat: P,
) -> std::fmt::Result
where
    I: ExactSizeIterator<Item = T>,
{
    let length = args.len();
    for (i, arg) in args.enumerate() {
        write!(f, "{}", arg)?;
        if i + 1 < length {
            write!(f, "{}", pat)?;
        }
    }
    Ok(())
}

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident(Box<str>);

impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        Self(s.into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Member {
    Named(Ident),
    Index(usize),
}

impl Member {
    pub fn is_named(&self) -> bool {
        matches!(self, Self::Named(_))
    }
}

impl Ident {
    pub fn new(s: impl Into<Box<str>>) -> Self {
        Self(s.into())
    }
}

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
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct QualifiedName(Box<[Ident]>);

impl QualifiedName {
    pub fn iter(&self) -> impl Iterator<Item = &Ident> {
        self.0.iter()
    }
    pub fn new(names: Box<[Ident]>) -> Self {
        Self(names)
    }
}

impl From<Box<[Ident]>> for QualifiedName {
    fn from(s: Box<[Ident]>) -> Self {
        Self::new(s)
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fmt_separated(f, self.0.iter(), "::")
    }
}

#[derive(Clone)]
pub enum StackedHashMap<'a, K, V> {
    Nil,
    Cons(HashMap<K, V>, &'a Self),
}

impl<'a, K: Hash + Eq, V> Default for StackedHashMap<'a, K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, K: Hash + Eq, V> StackedHashMap<'a, K, V> {
    pub fn new() -> Self {
        Self::Nil
    }
    pub fn stack<'b>(&'a self, map: HashMap<K, V>) -> StackedHashMap<'b, K, V>
    where
        'a: 'b,
    {
        Self::Cons(map, self)
    }
    pub fn get<Q: Borrow<K>>(&self, key: Q) -> Option<&V> {
        match self {
            Self::Nil => None,
            Self::Cons(map, rest) => map.get(key.borrow()).or_else(|| rest.get(key)),
        }
    }
}
