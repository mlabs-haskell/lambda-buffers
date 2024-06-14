use crate::error::Error;
use crate::json::{json_array, Json};
use plutus_ledger_api::v1::assoc_map::AssocMap;

impl<K: Json, V: Json> Json for AssocMap<K, V> {
    fn to_json(&self) -> serde_json::Value {
        json_array(
            (&self.0)
                .into_iter()
                .map(|(k, v)| json_array(vec![k.to_json(), v.to_json()]))
                .collect(),
        )
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        let vec_of_vectors: Vec<Vec<serde_json::Value>> = Json::from_json(value)?;
        let vec_of_pairs = vec_of_vectors
            .into_iter()
            .map(|vec| {
                let [k, v]: [serde_json::Value; 2] =
                    TryFrom::try_from(vec).map_err(|vec: Vec<_>| Error::UnexpectedArrayLength {
                        got: vec.len(),
                        wanted: 2,
                        parser: "v1::assoc_map::AssocMap".into(),
                    })?;

                let k = K::from_json(&k)?;
                let v = V::from_json(&v)?;

                Ok((k, v))
            })
            .collect::<Result<Vec<(K, V)>, _>>()?;

        Ok(Self(vec_of_pairs))
    }
}
