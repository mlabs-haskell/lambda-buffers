use crate::{
    error::Error,
    json::{case_json_constructor, case_json_object, json_constructor, json_object, Json},
};
use data_encoding::HEXLOWER;
use num_bigint::BigInt;
use plutus_ledger_api::plutus_data::PlutusData;

impl Json for PlutusData {
    fn to_json(&self) -> serde_json::Value {
        match self {
            PlutusData::Constr(index, fields) => json_constructor(
                "Constr",
                vec![json_object(vec![
                    ("index".to_string(), index.to_json()),
                    ("fields".to_string(), fields.to_json()),
                ])],
            ),
            PlutusData::Map(map) => json_constructor("Map", vec![map.to_json()]),
            PlutusData::List(list) => json_constructor("List", vec![list.to_json()]),
            PlutusData::Integer(int) => json_constructor("Integer", vec![int.to_json()]),
            PlutusData::Bytes(bytes) => {
                json_constructor("Bytes", vec![String::to_json(&HEXLOWER.encode(bytes))])
            }
        }
    }
    fn from_json(value: &serde_json::Value) -> Result<PlutusData, Error> {
        case_json_constructor(
            "PlutusV1.PlutusData",
            vec![
                (
                    "Constr",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => case_json_object(
                            |obj| {
                                let index = obj.get("index").ok_or(Error::UnexpectedFieldName {
                                    wanted: "index".to_owned(),
                                    got: obj.keys().cloned().collect(),
                                    parser: "PlutusV1.PlutusData".to_owned(),
                                })?;
                                let fields =
                                    obj.get("fields").ok_or(Error::UnexpectedFieldName {
                                        wanted: "fields".to_owned(),
                                        got: obj.keys().cloned().collect(),
                                        parser: "PlutusV1.PlutusData".to_owned(),
                                    })?;
                                Ok(PlutusData::Constr(
                                    BigInt::from_json(index)?,
                                    <Vec<PlutusData>>::from_json(fields)?,
                                ))
                            },
                            val,
                        ),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "Map",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(PlutusData::Map(Json::from_json(val)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "List",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(PlutusData::List(Json::from_json(val)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "Integer",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(PlutusData::Integer(Json::from_json(val)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "Bytes",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => {
                            let bytes = String::from_json(val).and_then(|str| {
                                HEXLOWER.decode(&str.into_bytes()).map_err(|_| {
                                    Error::UnexpectedJsonInvariant {
                                        wanted: "base16 string".to_owned(),
                                        got: "unexpected string".to_owned(),
                                        parser: "Plutus.V1.Bytes".to_owned(),
                                    }
                                })
                            })?;
                            Ok(PlutusData::Bytes(bytes))
                        }
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}
