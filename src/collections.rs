#![allow(unused)]

use rpds::HashTrieMap;
use rustc_hash::{FxHashMap, FxHashSet};

pub type Set<T> = FxHashSet<T>;

pub type Map<K, V> = FxHashMap<K, V>;

pub type TrieMap<K, V> = HashTrieMap<K, V>;

#[allow(unused)]
pub fn set_singleton<T: std::hash::Hash + Eq>(a: T) -> Set<T> {
    let mut set: Set<T> = Default::default();
    set.insert(a);
    set
}

pub fn set_add_new<T>(set: &mut Set<T>, t: T)
where
    T: Eq + std::hash::Hash,
{
    let new = set.insert(t);
    debug_assert!(new);
}

#[allow(unused)]
pub fn map_add_new<K, V>(map: &mut Map<K, V>, k: K, v: V)
where
    K: Eq + std::hash::Hash,
{
    let old = map.insert(k, v);
    debug_assert!(old.is_none());
}
