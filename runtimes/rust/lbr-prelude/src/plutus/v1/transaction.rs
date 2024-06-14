use crate::{
    error::{Error, JsonType},
    json::{case_json_constructor, json_constructor, Json},
};
use plutus_ledger_api::v1::transaction::{
    DCert, POSIXTime, ScriptContext, ScriptPurpose, TransactionHash, TransactionInfo,
    TransactionInput, TransactionOutput, TxInInfo,
};

impl Json for TransactionInput {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("transaction_id".to_owned(), self.transaction_id.to_json());
        dict.insert("index".to_owned(), self.index.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let transaction_id = dict
                    .get("transaction_id")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "transaction_id".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let index = dict
                    .get("index")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "index".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    transaction_id,
                    index,
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "TransactionInput".to_owned(),
            }),
        }
    }
}

impl Json for TransactionHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for TransactionOutput {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("address".to_owned(), self.address.to_json());
        dict.insert("value".to_owned(), self.value.to_json());
        dict.insert("datum_hash".to_owned(), self.datum_hash.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let address = dict
                    .get("address")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "address".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionOutput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let value = dict
                    .get("value")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "value".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionOutput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let datum_hash = dict
                    .get("datum_hash")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "datum_hash".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionOutput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    address,
                    value,
                    datum_hash,
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "TransactionOutput".to_owned(),
            }),
        }
    }
}

impl Json for POSIXTime {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for TxInInfo {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("reference".to_owned(), self.reference.to_json());
        dict.insert("output".to_owned(), self.output.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let reference = dict
                    .get("reference")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "reference".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TxInInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let output = dict
                    .get("output")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "output".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TxInInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self { reference, output })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "TxInInfo".to_owned(),
            }),
        }
    }
}

impl Json for DCert {
    fn to_json(&self) -> serde_json::Value {
        match self {
            DCert::DelegRegKey(f0) => json_constructor("DelegRegKey", vec![f0.to_json()]),
            DCert::DelegDeRegKey(f0) => json_constructor("DelegDeRegKey", vec![f0.to_json()]),
            DCert::DelegDelegate(f0, f1) => {
                json_constructor("DelegDelegate", vec![f0.to_json(), f1.to_json()])
            }
            DCert::PoolRegister(f0, f1) => {
                json_constructor("PoolRegister", vec![f0.to_json(), f1.to_json()])
            }
            DCert::PoolRetire(f0, f1) => {
                json_constructor("PoolRetire", vec![f0.to_json(), f1.to_json()])
            }
            DCert::Genesis => json_constructor("Genesis", &Vec::with_capacity(0)),
            DCert::Mir => json_constructor("Mir", &Vec::with_capacity(0)),
        }
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        case_json_constructor(
            "DCert",
            vec![
                (
                    "DelegRegKey",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(DCert::DelegRegKey(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "DCert".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "DelegDeRegKey",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(DCert::DelegDeRegKey(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "DCert".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "DelegDelegate",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 2usize {
                            Ok(DCert::DelegDelegate(
                                Json::from_json(&ctor_fields[0])?,
                                Json::from_json(&ctor_fields[1])?,
                            ))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 2usize,
                                got: ctor_fields.len(),
                                parser: "DCert".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "PoolRegister",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 2usize {
                            Ok(DCert::PoolRegister(
                                Json::from_json(&ctor_fields[0])?,
                                Json::from_json(&ctor_fields[1])?,
                            ))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 2usize,
                                got: ctor_fields.len(),
                                parser: "DCert".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "PoolRetire",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 2usize {
                            Ok(DCert::PoolRetire(
                                Json::from_json(&ctor_fields[0])?,
                                Json::from_json(&ctor_fields[1])?,
                            ))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 2usize,
                                got: ctor_fields.len(),
                                parser: "DCert".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "Genesis",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(DCert::Genesis),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: "DCert".to_owned(),
                        }),
                    }),
                ),
                (
                    "Mir",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(DCert::Mir),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: "DCert".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl Json for ScriptPurpose {
    fn to_json(&self) -> serde_json::Value {
        match self {
            ScriptPurpose::Minting(f0) => json_constructor("Minting", vec![f0.to_json()]),
            ScriptPurpose::Spending(f0) => json_constructor("Spending", vec![f0.to_json()]),
            ScriptPurpose::Rewarding(f0) => json_constructor("Rewarding", vec![f0.to_json()]),
            ScriptPurpose::Certifying(f0) => json_constructor("Certifying", vec![f0.to_json()]),
        }
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        case_json_constructor(
            "ScriptPurpose",
            vec![
                (
                    "Minting",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(ScriptPurpose::Minting(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "ScriptPurpose".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "Spending",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(ScriptPurpose::Spending(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "ScriptPurpose".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "Rewarding",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(ScriptPurpose::Rewarding(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "ScriptPurpose".to_owned(),
                            })
                        }
                    }),
                ),
                (
                    "Certifying",
                    Box::new(|ctor_fields| {
                        if ctor_fields.len() == 1usize {
                            Ok(ScriptPurpose::Certifying(Json::from_json(&ctor_fields[0])?))
                        } else {
                            Err(Error::UnexpectedArrayLength {
                                wanted: 1usize,
                                got: ctor_fields.len(),
                                parser: "ScriptPurpose".to_owned(),
                            })
                        }
                    }),
                ),
            ],
            value,
        )
    }
}

impl Json for TransactionInfo {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("inputs".to_owned(), self.inputs.to_json());
        dict.insert("outputs".to_owned(), self.outputs.to_json());
        dict.insert("fee".to_owned(), self.fee.to_json());
        dict.insert("mint".to_owned(), self.mint.to_json());
        dict.insert("d_cert".to_owned(), self.d_cert.to_json());
        dict.insert("wdrl".to_owned(), self.wdrl.to_json());
        dict.insert("valid_range".to_owned(), self.valid_range.to_json());
        dict.insert("signatories".to_owned(), self.signatories.to_json());
        dict.insert("datums".to_owned(), self.datums.to_json());
        dict.insert("id".to_owned(), self.id.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let inputs = dict
                    .get("inputs")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "inputs".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let outputs = dict
                    .get("outputs")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "outputs".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let fee = dict
                    .get("fee")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "fee".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let mint = dict
                    .get("mint")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "mint".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let d_cert = dict
                    .get("d_cert")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "d_cert".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let wdrl = dict
                    .get("wdrl")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "wdrl".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let valid_range = dict
                    .get("valid_range")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "valid_range".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let signatories = dict
                    .get("signatories")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "signatories".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let datums = dict
                    .get("datums")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "datums".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let id = dict
                    .get("id")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "id".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionInfo".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    inputs,
                    outputs,
                    fee,
                    mint,
                    d_cert,
                    wdrl,
                    valid_range,
                    signatories,
                    datums,
                    id,
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "TransactionInfo".to_owned(),
            }),
        }
    }
}

impl Json for ScriptContext {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("tx_info".to_owned(), self.tx_info.to_json());
        dict.insert("purpose".to_owned(), self.purpose.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let tx_info = dict
                    .get("tx_info")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "tx_info".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "ScriptContext".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let purpose = dict
                    .get("purpose")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "purpose".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "ScriptContext".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self { tx_info, purpose })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "ScriptContext".to_owned(),
            }),
        }
    }
}
