use std::fmt::Display;
use crate::parser::{Binding, Type};

#[derive(Debug, Clone)]
pub struct Scope<T: Display + Clone + PartialEq>(Vec<(String, T)>);

impl<T: Display + Clone + PartialEq> Scope<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn from_vars(vars: impl IntoIterator<Item = (String, T)>) -> Self {
        Self(vars.into_iter().collect())
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&T> {
        self.0.iter().find(|(n, _)| n.as_str() == name.as_ref()).map(|(_, field)| field)
    }

    pub fn get_mut(&mut self, name: impl AsRef<str>) -> Option<&mut T> {
        self.0.iter_mut().find(|(n, _)| n.as_str() == name.as_ref()).map(|(_, field)| field)
    }

    pub fn push(&mut self, name: String, field: T) {
        self.0.push((name, field));
    }

    pub fn size(&self) -> usize {
       self.0.len()
    }

    pub fn resize(&mut self, size: usize) {
        self.0.splice(size.., []);
    }
}

impl<T: Display + Clone + PartialEq> Display for Scope<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;

        for (name, field) in self.0.iter() {
            write!(f, "{}: {}, ", name, field)?;
        }

        write!(f, " }}")
    }
}

impl<T: Display + Clone + PartialEq> Scope<T> {
    pub fn iter(&self) -> ScopeIterator<T> {
        ScopeIterator {
            scope: self,
            index: 0
        }
    }
}

impl From<&Vec<Binding>> for Scope<Type> {
    fn from(value: &Vec<Binding>) -> Self {
        Self(value.iter().map(|Binding(name, ty)| (name.clone(), ty.clone())).collect())
    }
}

pub struct ScopeIterator<'a, T: Display + Clone + PartialEq> {
    scope: &'a Scope<T>,
    index: usize
}

impl<'a, T: Display + Clone + PartialEq> Iterator for ScopeIterator<'a, T> {
    type Item = (&'a String, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.scope.0.get(self.index).map(|(name, field)| (name, field));
        self.index += 1;
        result
    }
}