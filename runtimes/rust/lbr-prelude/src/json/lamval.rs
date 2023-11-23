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
    dyn Fn(
            Box<dyn Fn(Vec<Value>) -> Result<T, Error> + 'a>,
        ) -> Box<dyn FnOnce(Value) -> Result<T, Error> + 'a>
        + 'a,
> {
    Box::new(move |x1| Box::new(move |x2| super::case_json_array(x0, x1, x2)))
}

///// Parse a JSON Array as a dictionary
/////
///// LamVal Json builtin
pub fn case_json_map<'a, K: 'a, V: 'a>(
    x0: &'a str,
) -> Box<
    dyn Fn(
            Box<dyn Fn((Value, Value)) -> Result<(K, V), Error> + 'a>,
        ) -> Box<dyn FnOnce(Value) -> Result<BTreeMap<K, V>, Error> + 'a>
        + 'a,
>
where
    K: Ord,
{
    Box::new(move |x1| Box::new(move |x2| super::case_json_map(x0, x1, x2)))
}

/// Parse a JSON Object and its fields
///
/// LamVal Json builtin
pub fn case_json_object<'a, T: 'a>(
    x0: &'a str,
) -> Box<
    dyn Fn(
            Box<dyn Fn(serde_json::Map<String, Value>) -> Result<T, Error> + 'a>,
        ) -> Box<dyn FnOnce(Value) -> Result<T, Error> + 'a>
        + 'a,
> {
    Box::new(move |x1| Box::new(move |x2| super::case_json_object(x0, x1, x2)))
}

/// Extract a field from a JSON Object
///
/// LamVal Json builtin
pub fn json_field<'a>(
    x0: &'a str,
) -> Box<dyn FnOnce(&serde_json::Map<String, Value>) -> Result<Value, Error> + 'a> {
    Box::new(move |x1| super::json_field(x0, x1))
}

/// Construct a JSON Value from a sum type.
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON.
///
/// LamVal Json builtin
pub fn json_constructor<'a, T: 'a>(x0: &'a str) -> Box<dyn FnOnce(Vec<Value>) -> Value + 'a> {
    Box::new(move |x1| super::json_constructor(x0, x1))
}

/// Construct a closure that can parse a JSON object into a sum type.
/// We always encode sum types into a `{"name": string, "fields": any[]}` format in JSON.
///
/// LamVal Json builtin
pub fn case_json_constructor<'a, T: 'a>(
    x0: &'a str,
) -> Box<
    dyn Fn(
            Vec<(&'a str, Box<dyn Fn(&Vec<Value>) -> Result<T, Error>>)>,
        ) -> Box<dyn FnOnce(Value) -> Result<T, Error> + 'a>
        + 'a,
> {
    Box::new(move |x1| Box::new(move |x2| super::case_json_constructor(x0, x1, x2)))
}

/// Curried eq function
pub fn eq<'a, T>(x: T) -> Box<dyn FnOnce(T) -> bool + 'a>
where
    T: PartialEq + 'a,
{
    Box::new(move |y| x == y)
}

/// Curried boolean and function
pub fn and<'a>(x: bool) -> Box<dyn FnOnce(bool) -> bool + 'a> {
    Box::new(move |y| x && y)
}

/// Fail JSON parsing with an internal error
pub fn fail_parse<T>(err: &str) -> Result<T, Error> {
    Err(Error::InternalError(err.to_owned()))
}

/// Curried Result::and_then function
pub fn bind_parse<'a, A: 'a, B: 'a>(
    x: Result<A, Error>,
) -> Box<dyn FnOnce(Box<dyn Fn(A) -> Result<B, Error>>) -> Result<B, Error> + 'a> {
    Box::new(move |f| x.and_then(f))
}
