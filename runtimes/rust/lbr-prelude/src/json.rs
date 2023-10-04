use data_encoding::BASE64;
use num_bigint::BigInt;
use serde::ser;
use serde_json::{self, Error, Value};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Trait that lbf-prelude::json class maps to
trait Json {
    fn to_json(&self) -> Result<Value, Error>;

    fn from_json(value: Value) -> Result<Self, Error>
    where
        Self: Sized;
}

//  lbf-prelude::json instance rule implementations

impl Json for BigInt {
    fn to_json(&self) -> Result<Value, Error> {
        serde_json::to_value(self)
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        serde_json::from_value(value)
    }
}

impl Json for bool {
    fn to_json(&self) -> Result<Value, Error> {
        serde_json::to_value(self)
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        serde_json::from_value(value)
    }
}

impl Json for char {
    fn to_json(&self) -> Result<Value, Error> {
        serde_json::to_value(self)
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        serde_json::from_value(value)
    }
}

impl Json for Vec<u8> {
    fn to_json(&self) -> Result<Value, Error> {
        Ok(Value::String(BASE64.encode(self)))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::String(str) => BASE64
                .decode(&str.into_bytes())
                .map_err(|_| serde::ser::Error::custom("Couldn't decode base64 bytes.")),
            _ => error("Expected JSON String"),
        }
    }
}

impl Json for String {
    fn to_json(&self) -> Result<Value, Error> {
        serde_json::to_value(self)
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        serde_json::from_value(value)
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
        let map = sum_parser(&value)?;

        match map {
            ("Nothing", ctor_fields) => 
                match &ctor_fields[..] {
                    [] => Ok(None),
                    _ => Err(serde::ser::Error::custom(format!(
                        "Expected a JSON Array with 0 fields but got {:?}",
                        ctor_fields
                    ))),
                },
            ("Just", ctor_fields) => 
                match &ctor_fields[..] {
                    [val] => Ok(Some(T::from_json(val.clone())?)),
                    _ => Err(serde::ser::Error::custom(format!(
                        "Expected a JSON Array with 1 fields but got {:?}",
                        ctor_fields
                    ))),
                },
            _ => error(
                    "Expected a JSON String to contain one of constructor names (Just, Nothing) but got an unknown constructor name.")
        }
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
        let map = sum_parser(&value)?;

        match map {
            ("Right", ctor_fields) => 
                match &ctor_fields[..] {
                    [val] => Ok(Ok(T::from_json(val.clone())?)),
                    _ => error(&format!("Expected a JSON Array with 1 fields but got {:?}", ctor_fields)),
                },
                ("Left", ctor_fields) => 
                    match &ctor_fields[..] {
                        [val] => Ok(Err(E::from_json(val.clone())?)),
                        _ =>  error(&format!(
                                        "Expected a JSON Array with 1 fields but got {:?}",
                                        ctor_fields
                                        )),
                    },
                _ => error(
                        "Expected a JSON String to contain one of constructor names (Just, Nothing) but got an unknown constructor name.")
        }
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
            .into_iter()
            .collect::<Result<Vec<Value>, Error>>()?;

        Ok(Value::Array(values))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Array(vec) => vec
                .iter()
                .map(|val| T::from_json(val.clone()))
                .into_iter()
                .collect::<Result<Vec<T>, Error>>(),
            _ => error(&format!("Expected a JSON Array but got {:?}", value)),
        }
    }
}

impl<T> Json for HashSet<T>
where
    T: Json + Eq + Hash,
{
    fn to_json(&self) -> Result<Value, Error> {
        let values = self
            .iter()
            .map(|val| val.to_json())
            .into_iter()
            .collect::<Result<Vec<Value>, Error>>()?;

        Ok(Value::Array(values))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Array(vec) => {
                let set = vec
                    .iter()
                    .map(|val| T::from_json(val.clone()))
                    .into_iter()
                    .collect::<Result<HashSet<T>, Error>>()?;

                if set.len() == vec.len() {
                    Ok(set)
                } else {
                    error(&format!(
                        "Expected the JSON Array to have all unique elements: {:?}",
                        vec
                    ))
                }
            }
            _ => error(&format!("Expected a JSON Array but got {:?}", value)),
        }
    }
}

impl<K, V> Json for HashMap<K, V>
where
    K: Json + Eq + Hash,
    V: Json,
{
    fn to_json(&self) -> Result<Value, Error> {
        let values = self
            .iter()
            .map(|(key, val)| Ok(Value::Array(vec![key.to_json()?, val.to_json()?])))
            .into_iter()
            .collect::<Result<Vec<Value>, Error>>()?;

        Ok(Value::Array(values))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Array(vec) => {
                let set = vec
                    .iter()
                    .map(|kv_tuple| <(K, V)>::from_json(kv_tuple.clone()))
                    .into_iter()
                    .collect::<Result<HashMap<K, V>, Error>>()?;

                if set.len() == vec.len() {
                    Ok(set)
                } else {
                    error(&format!(
                        "Expected the JSON Array to have all unique elements: {:?}",
                        vec
                    ))
                }
            }
            _ => error(&format!("Expected a JSON Array but got {:?}", value)),
        }
    }
}

impl Json for () {
    fn to_json(&self) -> Result<Value, Error> {
        Ok(Value::Null)
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        match value {
            Value::Null => Ok(()),
            _ => error(&format!("Expected a JSON Null but got {:?}", value)),
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
        match value {
            Value::Array(vec) => match &vec[..] {
                [a, b] => Ok((A::from_json(a.clone())?, B::from_json(b.clone())?)),
                _ => error("Expected a JSON Array with 2 fields"),
            },
            _ => error("Expected a JSON Array with 2 fields"),
        }
    }
}

fn error<T>(msg: &str) -> Result<T, Error> {
    Err(ser::Error::custom(msg))
}

pub fn json_constructor(ctor_name: &str, ctor_product: Vec<Value>) -> Value {
    let mut obj = serde_json::Map::new();
    obj.insert("name".to_owned(), Value::String(ctor_name.to_owned()));
    obj.insert("field".to_owned(), Value::Array(ctor_product));

    Value::Object(obj)
}

pub fn sum_parser<'a>(value: &'a Value) -> Result<(&'a str, &'a Vec<Value>), Error> {
    match value {
        Value::Object(obj) => {
            let name = obj
                .get("name")
                .ok_or(ser::Error::custom("Expected a 'name' field"))?;
            let fields = obj
                .get("fields")
                .ok_or(ser::Error::custom("Expected a 'fields' field"))?;

            match (name, fields) {
                (Value::String(name), Value::Array(values)) => Ok((&name, values)),
                _ => Err(ser::Error::custom(format!(
                    "Expected a JSON Object with name and fields but got {:?}",
                    obj
                ))),
            }
        }
        _ => error(&format!("Expected a JSON Object but got {:?}", value)),
    }
}
