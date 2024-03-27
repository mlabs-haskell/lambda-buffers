//! Common error type
use serde_json::Value;
use thiserror;

/// Json types corresponding to `serde_json::Value`
#[derive(Debug)]
pub enum JsonType {
    Null,
    Bool,
    Number,
    String,
    Array,
    Object,
}

impl From<&Value> for JsonType {
    fn from(value: &Value) -> JsonType {
        match value {
            Value::Null => JsonType::Null,
            Value::Bool(_) => JsonType::Bool,
            Value::Number(_) => JsonType::Number,
            Value::String(_) => JsonType::String,
            Value::Array(_) => JsonType::Array,
            Value::Object(_) => JsonType::Object,
        }
    }
}

/// Error type representing Json conversion errors
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Malformed JSON string, unable to parse")]
    MalformedJson { source: serde_json::Error },
    #[error("{parser:?} > Expected a JSON type: {wanted:?}, but got a JSON type: {got:?}")]
    UnexpectedJsonType {
        got: JsonType,
        wanted: JsonType,
        parser: String,
    },
    #[error("{parser:?} > Expected a JSON type as {wanted:?}, but got {got:?}")]
    UnexpectedJsonInvariant {
        got: String,
        wanted: String,
        parser: String,
    },
    #[error("{parser:?} > Expected a field name in a JSON Object: {wanted:?}, but got fields named {got:?}")]
    UnexpectedFieldName {
        got: Vec<String>,
        wanted: String,
        parser: String,
    },
    #[error(
        "{parser:?} > Expected a JSON Array with {wanted:?} elements, but got {got:?} elements"
    )]
    UnexpectedArrayLength {
        got: usize,
        wanted: usize,
        parser: String,
    },
    #[error("Some internal error happened: {0}")]
    InternalError(String),
}
