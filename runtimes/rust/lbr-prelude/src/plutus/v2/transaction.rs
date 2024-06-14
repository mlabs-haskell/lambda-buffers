use crate::{
    error::{Error, JsonType},
    json::Json,
};
use plutus_ledger_api::v2::transaction::{
    ScriptContext, TransactionInfo, TransactionOutput, TxInInfo,
};

impl Json for TransactionOutput {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("address".to_owned(), self.address.to_json());
        dict.insert("value".to_owned(), self.value.to_json());
        dict.insert("datum".to_owned(), self.datum.to_json());
        dict.insert(
            "reference_script".to_owned(),
            self.reference_script.to_json(),
        );
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
                let datum = dict
                    .get("datum")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "datum".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionOutput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let reference_script = dict
                    .get("reference_script")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "reference_script".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "TransactionOutput".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    address,
                    value,
                    datum,
                    reference_script,
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
impl Json for TransactionInfo {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("inputs".to_owned(), self.inputs.to_json());
        dict.insert(
            "reference_inputs".to_owned(),
            self.reference_inputs.to_json(),
        );
        dict.insert("outputs".to_owned(), self.outputs.to_json());
        dict.insert("fee".to_owned(), self.fee.to_json());
        dict.insert("mint".to_owned(), self.mint.to_json());
        dict.insert("d_cert".to_owned(), self.d_cert.to_json());
        dict.insert("wdrl".to_owned(), self.wdrl.to_json());
        dict.insert("valid_range".to_owned(), self.valid_range.to_json());
        dict.insert("signatories".to_owned(), self.signatories.to_json());
        dict.insert("redeemers".to_owned(), self.redeemers.to_json());
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
                let reference_inputs = dict
                    .get("reference_inputs")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "reference_inputs".to_owned(),
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
                let redeemers = dict
                    .get("redeemers")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "redeemers".to_owned(),
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
                    reference_inputs,
                    outputs,
                    fee,
                    mint,
                    d_cert,
                    wdrl,
                    valid_range,
                    signatories,
                    redeemers,
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
