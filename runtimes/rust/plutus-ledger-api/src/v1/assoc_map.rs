use std::hash::Hash;

#[cfg(feature = "lbf")]
use lbr_prelude::json::{json_array, Json};
use linked_hash_map::LinkedHashMap;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::plutus_data::{IsPlutusData, PlutusData, PlutusDataError, PlutusType};

//////////////
// AssocMap //
//////////////

#[derive(Debug, PartialEq, Eq, Clone, Default, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct AssocMap<K, V>(pub Vec<(K, V)>);

impl<K, V> AssocMap<K, V> {
    pub fn new() -> Self {
        AssocMap(Vec::new())
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, None is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old value is returned. The key is not updated, though; this matters for types that can be == without being identical. See the module-level documentation for more.
    pub fn insert(&mut self, key: K, mut value: V) -> Option<V>
    where
        K: PartialEq,
    {
        let old_value = self.0.iter_mut().find(|(k, _v)| k == &key);
        match old_value {
            None => {
                self.0.push((key, value));
                None
            }
            Some((_, v)) => {
                std::mem::swap(v, &mut value);
                Some(value)
            }
        }
    }

    /// Removes a key from the map, returning the value at the key if the key was previously in the map.
    ///
    /// The key may be any borrowed form of the map’s key type, but the ordering on the borrowed form must match the ordering on the key type.
    pub fn remove(&mut self, key: &K) -> Option<V>
    where
        K: PartialEq,
        V: Clone,
    {
        let vec = &mut self.0;

        let old_value = vec
            .iter()
            .enumerate()
            .find_map(|(i, (k, _))| if k == key { Some(i) } else { None });
        match old_value {
            None => None,
            Some(i) => {
                let (_, v) = vec.remove(i);
                Some(v)
            }
        }
    }
}

impl<K: IsPlutusData, V: IsPlutusData> IsPlutusData for AssocMap<K, V> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Map(
            self.0
                .iter()
                .map(|(k, v)| (k.to_plutus_data(), v.to_plutus_data()))
                .collect(),
        )
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Map(pairs) => pairs
                .iter()
                .map(|(k, v)| Ok((K::from_plutus_data(k)?, V::from_plutus_data(v)?)))
                .collect::<Result<Vec<(K, V)>, PlutusDataError>>()
                .map(Self),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                got: From::from(plutus_data),
                wanted: PlutusType::Map,
            }),
        }
    }
}

impl<K, V> From<Vec<(K, V)>> for AssocMap<K, V> {
    fn from(vec: Vec<(K, V)>) -> Self {
        AssocMap(vec)
    }
}

impl<K: Clone, V: Clone, const N: usize> From<[(K, V); N]> for AssocMap<K, V> {
    fn from(vec: [(K, V); N]) -> Self {
        AssocMap(vec.to_vec())
    }
}

impl<K, V> From<AssocMap<K, V>> for Vec<(K, V)> {
    fn from(m: AssocMap<K, V>) -> Self {
        m.0
    }
}

impl<K: Hash + Eq, V> From<AssocMap<K, V>> for LinkedHashMap<K, V> {
    fn from(m: AssocMap<K, V>) -> Self {
        m.0.into_iter().collect()
    }
}

impl<K: Hash + Eq, V> From<LinkedHashMap<K, V>> for AssocMap<K, V> {
    fn from(value: LinkedHashMap<K, V>) -> Self {
        AssocMap(value.into_iter().collect())
    }
}

#[cfg(feature = "lbf")]
impl<K: Json, V: Json> Json for AssocMap<K, V> {
    fn to_json(&self) -> serde_json::Value {
        json_array(
            self.0
                .iter()
                .map(|(k, v)| json_array(vec![k.to_json(), v.to_json()]))
                .collect(),
        )
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, lbr_prelude::json::Error> {
        let vec_of_vectors: Vec<Vec<serde_json::Value>> = Json::from_json(value)?;
        let vec_of_pairs = vec_of_vectors
            .into_iter()
            .map(|vec| {
                let [k, v]: [serde_json::Value; 2] =
                    TryFrom::try_from(vec).map_err(|vec: Vec<_>| {
                        lbr_prelude::json::Error::UnexpectedArrayLength {
                            got: vec.len(),
                            wanted: 2,
                            parser: "v1::assoc_map::AssocMap".into(),
                        }
                    })?;

                let k = K::from_json(&k)?;
                let v = V::from_json(&v)?;

                Ok((k, v))
            })
            .collect::<Result<Vec<(K, V)>, _>>()?;

        Ok(Self(vec_of_pairs))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assoc_map_insert() {
        let mut assoc_map = AssocMap::new();

        assoc_map.insert(1, "one");
        assoc_map.insert(2, "to");
        assoc_map.insert(3, "three");
        assoc_map.insert(2, "two");

        let expected = AssocMap::from([(1, "one"), (2, "two"), (3, "three")]);

        assert_eq!(assoc_map, expected);
    }

    #[test]
    fn assoc_map_remove() {
        let mut assoc_map = AssocMap::from([(1, "one"), (2, "two"), (3, "three")]);

        let removed = assoc_map.remove(&1);

        let expected = AssocMap::from([(2, "two"), (3, "three")]);

        assert_eq!(assoc_map, expected);
        assert_eq!(removed, Some("one"))
    }
}
