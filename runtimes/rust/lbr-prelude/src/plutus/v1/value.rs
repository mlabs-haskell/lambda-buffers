use crate::{
    error::{Error, JsonType},
    json::Json,
};
use plutus_ledger_api::v1::value::{AssetClass, CurrencySymbol, TokenName, Value};

impl Json for Value {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for CurrencySymbol {
    fn to_json(&self) -> serde_json::Value {
        match self {
            CurrencySymbol::Ada => serde_json::Value::String(String::new()),
            CurrencySymbol::NativeToken(policy_hash) => policy_hash.to_json(),
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        match value.clone() {
            serde_json::Value::String(str) => {
                if str.is_empty() {
                    Ok(CurrencySymbol::Ada)
                } else {
                    Ok(CurrencySymbol::NativeToken(Json::from_json(value)?))
                }
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::String,
                got: JsonType::from(value),
                parser: "Plutus.V1.CurrencySymbol".to_owned(),
            }),
        }
    }
}

impl Json for TokenName {
    fn to_json(&self) -> serde_json::Value {
        self.0.to_json()
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        Ok(Self(Json::from_json(value)?))
    }
}

impl Json for AssetClass {
    fn to_json(&self) -> serde_json::Value {
        let mut dict = serde_json::Map::new();
        dict.insert("currency_symbol".to_owned(), self.currency_symbol.to_json());
        dict.insert("token_name".to_owned(), self.token_name.to_json());
        serde_json::Value::Object(dict)
    }
    fn from_json(value: &serde_json::Value) -> std::result::Result<Self, Error> {
        match value {
            serde_json::Value::Object(dict) => {
                let currency_symbol = dict
                    .get("currency_symbol")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "currency_symbol".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "AssetClass".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                let token_name = dict
                    .get("token_name")
                    .ok_or(Error::UnexpectedFieldName {
                        wanted: "token_name".to_owned(),
                        got: dict.keys().cloned().collect(),
                        parser: "AssetClass".to_owned(),
                    })
                    .and_then(Json::from_json)?;
                Ok(Self {
                    currency_symbol,
                    token_name,
                })
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::Object,
                got: JsonType::from(value),
                parser: "AssetClass".to_owned(),
            }),
        }
    }
}
