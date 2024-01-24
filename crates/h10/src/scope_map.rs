use crate::collections::Map;

use std::borrow::Borrow;
use std::hash::Hash;

#[derive(Debug)]
pub struct ScopeMap<K, V>(Vec<Map<K, V>>);

impl<K, V> Default for ScopeMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> ScopeMap<K, V> {
    pub fn new() -> Self {
        ScopeMap(vec![Default::default()])
    }

    pub fn len_scopes(&self) -> usize {
        self.0.len()
    }

    /// Exit the current scope. Panics if we're not in a scope.
    pub fn exit(&mut self) {
        self.0.pop().unwrap();
    }

    /// Enter a new scope.
    pub fn enter(&mut self) {
        self.0.push(Default::default());
    }
}

impl<K: Hash + Eq, V> ScopeMap<K, V> {
    /// Bind at the current scope. If the mapped thing is already mapped in the *current scope*
    /// (not in a parent scope!), returns the old value for the thing. The return value can be used
    /// to check duplicate definitions.
    pub fn bind(&mut self, k: K, v: V) -> Option<V> {
        self.0.last_mut().unwrap().insert(k, v)
    }

    /// Get the value of the key, from the outer-most scope that has the key.
    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        for map in self.0.iter().rev() {
            if let Some(val) = map.get(k) {
                return Some(val);
            }
        }
        None
    }

    /// Get the value of the key in the outer-most scope. Unlike `get`, this does not look at
    /// parent scopes when the key is not in the outer-most scope.
    pub fn get_current_scope<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.last().unwrap().get(k)
    }
}
