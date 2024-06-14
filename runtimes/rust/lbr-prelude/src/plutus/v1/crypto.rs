use crate::{error::Error, json::Json};
use data_encoding::HEXLOWER;
use plutus_ledger_api::v1::crypto::{
    Ed25519PubKeyHash, LedgerBytes, PaymentPubKeyHash, StakePubKeyHash,
};

impl Json for LedgerBytes {
    fn to_json(&self) -> serde_json::Value {
        String::to_json(&HEXLOWER.encode(&self.0))
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        let bytes = String::from_json(value).and_then(|str| {
            HEXLOWER
                .decode(&str.into_bytes())
                .map_err(|_| Error::UnexpectedJsonInvariant {
                    wanted: "base16 string".to_owned(),
                    got: "unexpected string".to_owned(),
                    parser: "Plutus.V1.Bytes".to_owned(),
                })
        })?;

        Ok(Self(bytes))
    }
}

impl Json for Ed25519PubKeyHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for PaymentPubKeyHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
impl Json for StakePubKeyHash {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}
