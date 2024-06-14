use crate::{
    error::{Error, JsonType},
    json::{case_json_constructor, json_constructor, Json},
};
use plutus_ledger_api::v1::interval::{Extended, LowerBound, PlutusInterval, UpperBound};

impl<T> Json for PlutusInterval<T>
where
    T: Json,
{
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("from".to_owned(), self.from.to_json());
        dict.insert("to".to_owned(), self.to.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let from = dict
                    .get("from")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "from".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "PlutusInterval".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let to = dict
                    .get("to")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "to".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "PlutusInterval".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self { from, to })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "PlutusInterval".to_owned(),
            }),
        }
    }
}
impl<T> Json for UpperBound<T>
where
    T: Json,
{
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("bound".to_owned(), self.bound.to_json());
        dict.insert("closed".to_owned(), self.closed.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let bound = dict
                    .get("bound")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "bound".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "UpperBound".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let closed = dict
                    .get("closed")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "closed".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "UpperBound".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self { bound, closed })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "UpperBound".to_owned(),
            }),
        }
    }
}

impl<T> Json for LowerBound<T>
where
    T: Json,
{
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("bound".to_owned(), self.bound.to_json());
        dict.insert("closed".to_owned(), self.closed.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let bound = dict
                    .get("bound")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "bound".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "LowerBound".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let closed = dict
                    .get("closed")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "closed".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "LowerBound".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self { bound, closed })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "LowerBound".to_owned(),
            }),
        }
    }
}

impl<T> Json for Extended<T>
where
    T: Json,
{
    fn to_json(&self) -> serde_json::Value {
        match self {
            Extended::NegInf => json_constructor("NegInf", &Vec::with_capacity(0)),
            Extended::Finite(f0) => json_constructor("Finite", vec![f0.to_json()]),
            Extended::PosInf => json_constructor("PosInf", &Vec::with_capacity(0)),
        }
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        case_json_constructor(
            "Extended",
            vec![
                (
                    "NegInf",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(Extended::NegInf),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: "Extended".to_owned(),
                        }),
                    }),
                ),
                (
                    "Finite",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(Extended::Finite(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "Extended".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "PosInf",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(Extended::PosInf),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: "Extended".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}
