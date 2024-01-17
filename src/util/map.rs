use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

pub type FxHashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<FxHasher>>;
