pub use crate::error::{Error, JsonType};
use core::str::FromStr;
use data_encoding::BASE64;
#[cfg(feature = "derive")]
pub use lbr_prelude_derive::Json;
use num_bigint::BigInt;
use serde_json;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};

/// Trait that lbf-prelude::json class maps to
pub trait Json {
    fn to_json(&self) -> Result<Value, Error>;

    fn from_json(value: Value) -> Result<Self, Error>
    where
        Self: Sized;
}

//  lbf-prelude::json instance rule implementations

impl Json for BigInt {
    fn to_json(&self) -> Result<Value, Error> {
        let num = serde_json::Number::from_str(&self.to_string())
            .map_err(|_| Error::InternalError("Failed to convert BigInt to String".to_owned()))?;

        Ok(Value::Number(num))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Number(number) => {
                BigInt::from_str(number.as_str()).map_err(|_| Error::UnexpectedJsonInvariant {
                    wanted: "bigint".to_owned(),
                    got: "unexpected string".to_owned(),
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Number,
                got: JsonType::from(&value),
            }),
        }
    }
}

impl Json for bool {
    fn to_json(&self) -> Result<Value, Error> {
        Ok(Value::Bool(*self))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Bool(bool) => Ok(bool),
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Bool,
                got: JsonType::from(&value),
            }),
        }
    }
}

impl Json for char {
    fn to_json(&self) -> Result<Value, Error> {
        String::from(*self).to_json()
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        String::from_json(value).and_then(|str| {
            let mut chars = str.chars();
            let ch = chars.next();
            let rest = chars.next();
            match (ch, rest) {
                (Some(ch), None) => Ok(ch),
                _ => Err(Error::UnexpectedJsonInvariant {
                    got: "string".to_owned(),
                    wanted: "char".to_owned(),
                }),
            }
        })
    }
}

impl Json for Vec<u8> {
    fn to_json(&self) -> Result<Value, Error> {
        BASE64.encode(self).to_json()
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        String::from_json(value).and_then(|str| {
            BASE64
                .decode(&str.into_bytes())
                .map_err(|_| Error::UnexpectedJsonInvariant {
                    got: "string".to_owned(),
                    wanted: "base64 string".to_owned(),
                })
        })
    }
}

impl Json for String {
    fn to_json(&self) -> Result<Value, Error> {
        Ok(Value::String(self.to_owned()))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::String(str) => Ok(str),
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::String,
                got: JsonType::from(&value),
            }),
        }
    }
}

impl<T> Json for Option<T>
where
    T: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        match self {
            Some(val) => Ok(sum_constructor("Just", vec![val.to_json()?])),
            None => Ok(sum_constructor("Nothing", Vec::with_capacity(0))),
        }
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        sum_parser(&value).and_then(|obj| match obj {
            ("Nothing", ctor_fields) => match &ctor_fields[..] {
                [] => Ok(None),
                _ => Err(Error::UnexpectedArrayLength {
                    wanted: 0,
                    got: ctor_fields.len(),
                }),
            },
            ("Just", ctor_fields) => match &ctor_fields[..] {
                [val] => Ok(Some(T::from_json(val.clone())?)),
                _ => Err(Error::UnexpectedArrayLength {
                    wanted: 1,
                    got: ctor_fields.len(),
                }),
            },
            _ => Err(Error::UnexpectedJsonInvariant {
                wanted: "constructor names (Nothing, Just)".to_owned(),
                got: "unknown constructor name".to_owned(),
            }),
        })
    }
}

impl<T, E> Json for Result<T, E>
where
    T: Json,
    E: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        match self {
            Ok(val) => Ok(sum_constructor("Right", vec![val.to_json()?])),
            Err(val) => Ok(sum_constructor("Left", vec![val.to_json()?])),
        }
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        sum_parser(&value).and_then(|map| match map {
            ("Right", ctor_fields) => match &ctor_fields[..] {
                [val] => Ok(Ok(T::from_json(val.clone())?)),
                _ => Err(Error::UnexpectedArrayLength {
                    wanted: 1,
                    got: ctor_fields.len(),
                }),
            },
            ("Left", ctor_fields) => match &ctor_fields[..] {
                [val] => Ok(Err(E::from_json(val.clone())?)),
                _ => Err(Error::UnexpectedArrayLength {
                    wanted: 1,
                    got: ctor_fields.len(),
                }),
            },
            _ => Err(Error::UnexpectedJsonInvariant {
                wanted: "constructor names (Left, Right)".to_owned(),
                got: "unknown constructor name".to_owned(),
            }),
        })
    }
}

impl<T> Json for Vec<T>
where
    T: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        let values = self
            .iter()
            .map(|val| val.to_json())
            .collect::<Result<Vec<Value>, Error>>()?;

        Ok(Value::Array(values))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Array(vec) => vec
                .iter()
                .map(|val| T::from_json(val.clone()))
                .collect::<Result<Vec<T>, Error>>(),
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Array,
                got: JsonType::from(&value),
            }),
        }
    }
}

impl<T> Json for BTreeSet<T>
where
    T: Json + Eq + Ord,
{
    fn to_json(&self) -> Result<Value, Error> {
        let values = self
            .iter()
            .map(|val| val.to_json())
            .collect::<Result<Vec<Value>, Error>>()?;

        Ok(Value::Array(values))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        Vec::from_json(value).and_then(|vec: Vec<Value>| {
            let set = vec
                .iter()
                .map(|val| T::from_json(val.clone()))
                .collect::<Result<BTreeSet<T>, Error>>()?;

            if set.len() == vec.len() {
                Ok(set)
            } else {
                Err(Error::UnexpectedJsonInvariant {
                    wanted: "array with all unique elements".to_owned(),
                    got: "invalid set".to_owned(),
                })
            }
        })
    }
}

impl<K, V> Json for BTreeMap<K, V>
where
    K: Json + Eq + Ord,
    V: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        let values = self
            .iter()
            .map(|(key, val)| Ok(Value::Array(vec![key.to_json()?, val.to_json()?])))
            .collect::<Result<Vec<Value>, Error>>()?;

        Ok(Value::Array(values))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        Vec::from_json(value).and_then(|vec: Vec<Value>| {
            let set = vec
                .iter()
                .map(|kv_tuple| <(K, V)>::from_json(kv_tuple.clone()))
                .collect::<Result<BTreeMap<K, V>, Error>>()?;

            if set.len() == vec.len() {
                Ok(set)
            } else {
                Err(Error::UnexpectedJsonInvariant {
                    wanted: "array with all unique elements".to_owned(),
                    got: "invalid set".to_owned(),
                })
            }
        })
    }
}

impl Json for () {
    fn to_json(&self) -> Result<Value, Error> {
        Ok(Value::Null)
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Null => Ok(()),
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Null,
                got: JsonType::from(&value),
            }),
        }
    }
}

impl Json for Value {
    fn to_json(&self) -> Result<Value, Error> {
        Ok(self.clone())
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        Ok(value)
    }
}

impl<A, B> Json for (A, B)
where
    A: Json,
    B: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        Ok(Value::Array(vec![self.0.to_json()?, self.1.to_json()?]))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        Vec::from_json(value).and_then(|vec: Vec<Value>| match &vec[..] {
            [a, b] => Ok((A::from_json(a.clone())?, B::from_json(b.clone())?)),
            _ => Err(Error::UnexpectedArrayLength {
                wanted: 2,
                got: vec.len(),
            }),
        })
    }
}

/// Construct a JSON Value from a sum type
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON
pub fn sum_constructor(ctor_name: &str, ctor_product: Vec<Value>) -> Value {
    let mut obj = serde_json::Map::new();
    obj.insert("name".to_owned(), Value::String(ctor_name.to_owned()));
    obj.insert("fields".to_owned(), Value::Array(ctor_product));

    Value::Object(obj)
}

/// Parse a JSON value into an intermediary representation of a sum type
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON
pub fn sum_parser(value: &Value) -> Result<(&str, &Vec<Value>), Error> {
    match value {
        Value::Object(obj) => {
            let name = obj
                .get("name")
                .ok_or(Error::UnexpectedFieldName {
                    wanted: "name".to_owned(),
                    got: obj.keys().cloned().collect(),
                })
                .and_then(|name| match name {
                    Value::String(str) => Ok(str),
                    _ => Err(Error::UnexpectedJsonType {
                        wanted: JsonType::String,
                        got: JsonType::from(value),
                    }),
                })?;
            let fields = obj
                .get("fields")
                .ok_or(Error::UnexpectedFieldName {
                    wanted: "fields".to_owned(),
                    got: obj.keys().cloned().collect(),
                })
                .and_then(|fields| match fields {
                    Value::Array(str) => Ok(str),
                    _ => Err(Error::UnexpectedJsonType {
                        wanted: JsonType::Array,
                        got: JsonType::from(value),
                    }),
                })?;

            Ok((name, fields))
        }
        _ => Err(Error::UnexpectedJsonType {
            wanted: JsonType::Null,
            got: JsonType::from(value),
        }),
    }
}
