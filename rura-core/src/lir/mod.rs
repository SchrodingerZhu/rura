use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

use serde::{Deserialize, Serialize};

use crate::{fmt_separated, Input};

pub mod ir;
pub mod types;

#[repr(transparent)]
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Ident(pub Box<str>);

impl From<&str> for Ident {
    fn from(s: &str) -> Self {
        Self(s.into())
    }
}

impl<'a> From<Input<'a>> for Ident {
    fn from(s: Input<'a>) -> Self {
        Self(s.to_string().into())
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct QualifiedName(pub Box<[Ident]>);

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

struct Update<K: Hash + Eq + Clone, V> {
    key: K,
    old_value: Option<V>,
}

pub struct HashMapProxy<'a, K: Hash + Eq + Clone, V> {
    map: &'a mut HashMap<K, V>,
    changelog: Vec<Update<K, V>>,
}

impl<'a, K: Hash + Eq + Clone, V> HashMapProxy<'a, K, V> {
    pub fn new(map: &'a mut HashMap<K, V>) -> Self {
        Self {
            map,
            changelog: Vec::new(),
        }
    }
    pub fn insert(&mut self, key: K, value: V) {
        let old_value = self.map.insert(key.clone(), value);
        self.changelog.push(Update { key, old_value });
    }
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.map.get(key)
    }

    pub fn get_inner_mut(&mut self) -> &mut HashMap<K, V> {
        self.map
    }

    pub fn get_inner(&self) -> &HashMap<K, V> {
        self.map
    }
}

impl<'a, K: Hash + Eq + Clone, V> Drop for HashMapProxy<'a, K, V> {
    fn drop(&mut self) {
        for update in self.changelog.drain(..).rev() {
            if let Some(old_value) = update.old_value {
                self.map.insert(update.key.clone(), old_value);
            } else {
                self.map.remove(&update.key);
            }
        }
    }
}
