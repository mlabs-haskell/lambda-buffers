//! This module provides curried versions of the LamVal builtin functions.

pub use super::{json_array, json_map, json_object};
use crate::error::Error;
use serde_json::Value;
use std::collections::BTreeMap;

/// Parse a JSON Array and its elements
///
/// LamVal Json builtin
pub fn case_json_array<'a, T: 'a>(
    x0: &'a str,
) -> Box<
    dyn 'a
        + Fn(
            Box<dyn 'a + Fn(&Vec<Value>) -> Result<T, Error>>,
        ) -> Box<dyn 'a + FnOnce(&Value) -> Result<T, Error>>,
> {
    Box::new(move |x1| Box::new(move |x2| super::case_json_array(x0, x1, x2)))
}

///// Parse a JSON Array as a dictionary
/////
///// LamVal Json builtin
pub fn case_json_map<'a, K: 'a, V: 'a>(
    x0: &'a str,
) -> Box<
    dyn 'a
        + Fn(
            Box<dyn 'a + Fn(&(Value, Value)) -> Result<(K, V), Error>>,
        ) -> Box<dyn 'a + FnOnce(&Value) -> Result<BTreeMap<K, V>, Error>>,
>
where
    K: Ord,
{
    Box::new(move |x1| Box::new(move |x2| super::case_json_map(x0, x1, x2)))
}

/// Parse a JSON Object and its fields
//
/// LamVal Json builtin
pub fn case_json_object<'a, T: 'a>(
    x0: Box<dyn 'a + Fn(&serde_json::Map<String, Value>) -> Result<T, Error>>,
) -> Box<dyn FnOnce(&Value) -> Result<T, Error> + 'a> {
    Box::new(move |x1| super::case_json_object(x0, x1))
}

/// Extract a field from a JSON Object
///
/// LamVal Json builtin
pub fn json_field<'a, T: 'a>(
    x0: &'a str,
) -> Box<
    dyn 'a
        + FnOnce(
            &'a serde_json::Map<String, Value>,
        ) -> Box<
            dyn 'a + FnOnce(Box<dyn 'a + Fn(&Value) -> Result<T, Error>>) -> Result<T, Error>,
        >,
> {
    Box::new(move |x1| Box::new(move |x2| super::json_field(x0, x1, x2)))
}

/// Construct a JSON Value from a sum type.
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON.
///
/// LamVal Json builtin
pub fn json_constructor<'a>(x0: &'a str) -> Box<dyn 'a + FnOnce(Vec<Value>) -> Value> {
    Box::new(move |x1| super::json_constructor(x0, x1))
}

/// Construct a closure that can parse a JSON object into a sum type.
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON.
///
/// LamVal Json builtin
pub fn case_json_constructor<'a, T: 'a>(
    x0: &'a str,
) -> Box<
    dyn 'a
        + Fn(
            Vec<(&'a str, Box<dyn Fn(&Vec<Value>) -> Result<T, Error>>)>,
        ) -> Box<dyn 'a + FnOnce(&Value) -> Result<T, Error>>,
> {
    Box::new(move |x1| Box::new(move |x2| super::case_json_constructor(x0, x1, x2)))
}

/// Fail JSON parsing with an internal error
pub fn fail_parse<T>(err: &str) -> Result<T, Error> {
    Err(Error::InternalError(err.to_owned()))
}

/// Curried Result::and_then function
pub fn bind_parse<'a, A: 'a, B: 'a>(
    x: Result<A, Error>,
) -> Box<dyn 'a + FnOnce(Box<dyn Fn(&A) -> Result<B, Error>>) -> Result<B, Error>> {
    Box::new(move |f| x.and_then(|x1| f(&x1)))
}
