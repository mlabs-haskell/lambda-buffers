use crate::{error::Error, json::Json};
use plutus_ledger_api::v1::redeemer::{Redeemer, RedeemerHash};

impl Json for Redeemer {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for RedeemerHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
