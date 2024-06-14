use crate::{error::Error, json::Json};
use plutus_ledger_api::v2::script::{MintingPolicyHash, ScriptHash, ValidatorHash};

impl Json for ValidatorHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for MintingPolicyHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for ScriptHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }

    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
