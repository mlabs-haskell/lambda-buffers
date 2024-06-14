use crate::error::Error;
use crate::json::{self, case_json_constructor, json_constructor, Json, JsonType};
use plutus_ledger_api::v1::address::{
    Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential, TransactionIndex,
};

impl Json for Credential {
    fn to_json(&self) -> serde_json::Value {
        match self {
            Credential::PubKey(pkh) => json_constructor("PubKeyCredential", vec![pkh.to_json()]),
            Credential::Script(val_hash) => {
                json_constructor("ScriptCredential", vec![val_hash.to_json()])
            }
        }
    }
    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        case_json_constructor(
            "Plutus.V1.Credential",
            vec![
                (
                    "PubKeyCredential",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [pkh] => Ok(Credential::PubKey(Json::from_json(pkh)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.Credential".to_owned(),
                        }),
                    }),
                ),
                (
                    "ScriptCredential",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val_hash] => Ok(Credential::Script(Json::from_json(val_hash)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.Credential".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl Json for StakingCredential {
    fn to_json(&self) -> serde_json::Value {
        match self {
            StakingCredential::Hash(pkh) => {
                json::json_constructor("StakingHash", vec![pkh.to_json()])
            }
            StakingCredential::Pointer(val_hash) => {
                json::json_constructor("StakingPtr", vec![val_hash.to_json()])
            }
        }
    }
    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        case_json_constructor(
            "Plutus.V1.StakingCredential",
            vec![
                (
                    "StakingHash",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [pkh] => Ok(StakingCredential::Hash(Json::from_json(pkh)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.StakingCredential".to_owned(),
                        }),
                    }),
                ),
                (
                    "StakingPtr",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val_hash] => Ok(StakingCredential::Pointer(Json::from_json(&val_hash)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.StakingCredential".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl Json for Address {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("credential".to_owned(), self.credential.to_json());
        dict.insert(
            "staking_credential".to_owned(),
            self.staking_credential.to_json(),
        );
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let credential = dict
                    .get("credential")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "credential".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "Address".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let staking_credential = dict
                    .get("staking_credential")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "staking_credential".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "Address".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    credential,
                    staking_credential,
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "Address".to_owned(),
            }),
        }
    }
}

impl Json for ChainPointer {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("slot_number".to_owned(), self.slot_number.to_json());
        dict.insert(
            "transaction_index".to_owned(),
            self.transaction_index.to_json(),
        );
        dict.insert(
            "certificate_index".to_owned(),
            self.certificate_index.to_json(),
        );
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let slot_number = dict
                    .get("slot_number")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "slot_number".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "ChainPointer".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let transaction_index = dict
                    .get("transaction_index")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "transaction_index".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "ChainPointer".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let certificate_index = dict
                    .get("certificate_index")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "certificate_index".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "ChainPointer".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    slot_number,
                    transaction_index,
                    certificate_index,
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "ChainPointer".to_owned(),
            }),
        }
    }
}
impl Json for Slot {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
impl Json for CertificateIndex {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
impl Json for TransactionIndex {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
