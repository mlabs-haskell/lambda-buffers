//! Json serialization of Lambda Buffers types
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
                    parser: "BigInt".to_owned(),
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Number,
                got: JsonType::from(&value),
                parser: "Prelude.Integer".to_owned(),
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
                parser: "Prelude.Bool".to_owned(),
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
                    parser: "Prelude.Char".to_owned(),
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
                    parser: "Prelude.Bytes".to_owned(),
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
                parser: "Prelude.Text".to_owned(),
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
            Some(val) => Ok(json_constructor("Just", vec![val.to_json()?])),
            None => Ok(json_constructor("Nothing", Vec::with_capacity(0))),
        }
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        case_json_constructor(
            "Prelude.Maybe",
            vec![
                (
                    "Nothing",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(None),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: "Prelude.Maybe".to_owned(),
                        }),
                    }),
                ),
                (
                    "Just",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(Some(T::from_json(val.clone())?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Prelude.Maybe".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl<T, E> Json for Result<T, E>
where
    T: Json,
    E: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        match self {
            Ok(val) => Ok(json_constructor("Right", vec![val.to_json()?])),
            Err(val) => Ok(json_constructor("Left", vec![val.to_json()?])),
        }
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        case_json_constructor(
            "Prelude.Either",
            vec![
                (
                    "Right",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(Ok(T::from_json(val.clone())?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Prelude.Either".to_owned(),
                        }),
                    }),
                ),
                (
                    "Left",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(Err(E::from_json(val.clone())?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Prelude.Either".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
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
                parser: "Prelude.List".to_owned(),
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
                    parser: "Prelude.Set".to_owned(),
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
                    parser: "Prelude.Map".to_owned(),
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
                parser: "Prelude.Unit".to_owned(),
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
                parser: "Prelude.Tuple".to_owned(),
            }),
        })
    }
}

/// Construct a JSON Array
///
/// LamVal Json builtin
pub fn json_array(array: Vec<Value>) -> Value {
    Value::Array(array)
}

/// Parse a JSON Array and its elements
pub fn case_json_array<A>(
    parser_name: &str,
    parse_arr: impl Fn(Vec<Value>) -> Result<A, Error>,
    value: Value,
) -> Result<A, Error> {
    match value {
        Value::Array(array) => parse_arr(array),
        _ => Err(Error::UnexpectedJsonType {
            wanted: JsonType::Array,
            got: JsonType::from(&value),
            parser: parser_name.to_owned(),
        }),
    }
}

/// Construct a dictionary as a nested JSON Array
/// k1: v1, k2: v2 -> `[[k1, v1], [k2, v2]]`
///
/// LamVal Json builtin
pub fn json_map(map: Vec<(Value, Value)>) -> Value {
    Value::Array(
        map.into_iter()
            .map(|(k, v)| Value::Array(vec![k, v]))
            .collect(),
    )
}

/// Parse a JSON Array as a dictionary
///
/// LamVal Json builtin
pub fn case_json_map<K, V>(
    parser_name: &str,
    parse_elem: impl Fn((Value, Value)) -> Result<(K, V), Error>,
    value: Value,
) -> Result<BTreeMap<K, V>, Error>
where
    K: Ord,
{
    match value {
        Value::Array(vec) => vec
            .into_iter()
            .map(|kv_tuple| parse_elem(<(Value, Value)>::from_json(kv_tuple)?))
            .collect::<Result<BTreeMap<K, V>, Error>>(),
        _ => Err(Error::UnexpectedJsonType {
            wanted: JsonType::Array,
            got: JsonType::from(&value),
            parser: parser_name.to_owned(),
        }),
    }
}

/// Construct a JSON Object for a list of key-value pairs
///
/// LamVal Json builtin
pub fn json_object(kvs: Vec<(String, Value)>) -> Value {
    Value::Object(
        kvs.into_iter()
            .map(|(k, v)| (k, v))
            .collect::<serde_json::Map<String, Value>>(),
    )
}

/// Parse a JSON Object and its fields
///
/// LamVal Json builtin
pub fn case_json_object<A>(
    parser_name: &str,
    parse_obj: impl Fn(serde_json::Map<String, Value>) -> Result<A, Error>,
    value: Value,
) -> Result<A, Error> {
    match value {
        Value::Object(obj) => parse_obj(obj),
        _ => Err(Error::UnexpectedJsonType {
            wanted: JsonType::Object,
            got: JsonType::from(&value),
            parser: parser_name.to_owned(),
        }),
    }
}

/// Extract a field from a JSON Object
///
/// LamVal Json builtin
pub fn json_field(name: &str, obj: &serde_json::Map<String, Value>) -> Result<Value, Error> {
    obj.get(name)
        .cloned()
        .ok_or_else(|| Error::UnexpectedFieldName {
            wanted: name.to_owned(),
            got: obj.keys().cloned().collect(),
            parser: name.to_owned(),
        })
}

/// Construct a JSON Value from a sum type.
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON.
///
/// LamVal Json builtin
pub fn json_constructor(ctor_name: &str, ctor_product: Vec<Value>) -> Value {
    let mut obj = serde_json::Map::new();
    obj.insert("name".to_owned(), Value::String(ctor_name.to_owned()));
    obj.insert("fields".to_owned(), Value::Array(ctor_product));

    Value::Object(obj)
}

/// Construct a closure that can parse a JSON object into a sum type.
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON.
///
/// LamVal Json builtin
pub fn case_json_constructor<'a, T: 'a>(
    parser_name: &'a str,
    ctor_parsers: Vec<(&'a str, Box<dyn Fn(&Vec<Value>) -> Result<T, Error>>)>,
    value: Value,
) -> Result<T, Error> {
    let ctor_parsers: BTreeMap<&str, Box<dyn Fn(&Vec<Value>) -> Result<T, Error>>> =
        BTreeMap::from_iter(ctor_parsers);

    match value {
        Value::Object(ref obj) => {
            let name = obj
                .get("name")
                .ok_or(Error::UnexpectedFieldName {
                    wanted: "name".to_owned(),
                    got: obj.keys().cloned().collect(),
                    parser: parser_name.to_owned(),
                })
                .and_then(|name| match name {
                    Value::String(str) => Ok(str),
                    _ => Err(Error::UnexpectedJsonType {
                        wanted: JsonType::String,
                        got: JsonType::from(&value),
                        parser: parser_name.to_owned(),
                    }),
                })?;
            let fields = obj
                .get("fields")
                .ok_or(Error::UnexpectedFieldName {
                    wanted: "fields".to_owned(),
                    got: obj.keys().cloned().collect(),
                    parser: parser_name.to_owned(),
                })
                .and_then(|fields| match fields {
                    Value::Array(str) => Ok(str),
                    _ => Err(Error::UnexpectedJsonType {
                        wanted: JsonType::Array,
                        got: JsonType::from(&value),
                        parser: parser_name.to_owned(),
                    }),
                })?;

            let names = ctor_parsers
                .keys()
                .map(std::ops::Deref::deref)
                .collect::<Vec<_>>()
                .join(", ");

            let ctor_parser =
                ctor_parsers
                    .get(name.as_str())
                    .ok_or(Error::UnexpectedJsonInvariant {
                        wanted: format!("constructor names ({})", names),
                        got: format!("unknown constructor name: {}", name),
                        parser: parser_name.to_owned(),
                    })?;

            ctor_parser(fields)
        }
        _ => Err(Error::UnexpectedJsonType {
            wanted: JsonType::Null,
            got: JsonType::from(&value),
            parser: parser_name.to_owned(),
        }),
    }
}
