use cardano_serialization_lib::crypto::ScriptHash;
use cardano_serialization_lib::{AssetName, PolicyID};
use lbr_prelude::error::Error;
use lbr_prelude::json::Json;
use serde_json::Value;

#[derive(Clone)]
struct AssetClass(PolicyID, AssetName);

pub struct AsJson<T>(pub T);

pub fn to_json<T>(data: T) -> Result<Value, Error>
where
    AsJson<T>: Json,
{
    AsJson(data).to_json()
}

pub fn from_json<T>(value: Value) -> Result<T, Error>
where
    AsJson<T>: Json,
{
    Ok(AsJson::from_json(value)?.0)
}

impl Json for AsJson<AssetClass> {
    fn to_json(&self) -> Result<Value, Error> {
        let AssetClass(policy_id, asset_name) = self.0.clone();

        let policy_id = to_json(policy_id)?;
        let asset_name = to_json(asset_name)?;

        Ok(Value::Array(vec![policy_id, asset_name]))
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        let (AsJson(policy_id), AsJson(asset_name)) =
            <(AsJson<PolicyID>, AsJson<AssetName>)>::from_json(value)?;

        let asset_class = AssetClass(policy_id, asset_name);
        Ok(AsJson(asset_class))
    }
}

impl Json for AsJson<AssetName> {
    fn to_json(&self) -> Result<Value, Error> {
        self.0.to_bytes().to_json()
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        <Vec<u8>>::from_json(value)
            .and_then(|bytes| {
                AssetName::from_bytes(bytes).map_err(|_| Error::UnexpectedJsonInvariant {
                    wanted: "28 bytes hex".to_owned(),
                    got: "unexpected string".to_owned(),
                })
            })
            .map(|script_hash| AsJson(script_hash))
    }
}

// impl Json Value;

// impl Json MultiAsset;

// impl Coin;

// impl Json PubKeyHash;

impl Json for AsJson<ScriptHash> {
    fn to_json(&self) -> Result<Value, Error> {
        self.0.to_bytes().to_json()
    }

    fn from_json(value: Value) -> Result<Self, Error> {
        <Vec<u8>>::from_json(value)
            .and_then(|bytes| {
                ScriptHash::from_bytes(bytes).map_err(|_| Error::UnexpectedJsonInvariant {
                    wanted: "28 bytes hex".to_owned(),
                    got: "unexpected string".to_owned(),
                })
            })
            .map(|script_hash| AsJson(script_hash))
    }
}

// impl Json Datum;

// impl Json Redeemer;

// impl Json Data;

// impl Json Extended;

// impl Json Address;

// impl Json TransactionInput;

// impl Json TransactionOutput;

// impl Json OutputDatum;

// impl Json TxInInfo;
