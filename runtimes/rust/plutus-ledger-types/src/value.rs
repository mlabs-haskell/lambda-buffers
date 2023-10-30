use crate::crypto::LedgerBytes;
use crate::script::MintingPolicyHash;
#[cfg(feature = "lbf")]
use lbr_prelude::json::{Error, Json, JsonType};
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
#[cfg(feature = "lbf")]
use serde_json;
use std::collections::BTreeMap;

/// Identifier of a currency, which could be either Ada (or tAda), or a native token represented by
/// it's minting policy hash. A currency may be associated with multiple `AssetClass`es.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CurrencySymbol {
    Ada,
    NativeToken(MintingPolicyHash),
}

#[cfg(feature = "lbf")]
impl Json for CurrencySymbol {
    fn to_json(&self) -> Result<serde_json::Value, Error> {
        match self {
            CurrencySymbol::Ada => Ok(serde_json::Value::String(String::new())),
            CurrencySymbol::NativeToken(policy_hash) => policy_hash.to_json(),
        }
    }

    fn from_json(value: serde_json::Value) -> Result<Self, Error> {
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
                got: JsonType::from(&value),
            }),
        }
    }
}

/// A value that can contain multiple asset classes
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Value(pub BTreeMap<CurrencySymbol, BTreeMap<TokenName, BigInt>>);

/// Name of a token. This can be any arbitrary bytearray
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TokenName(pub LedgerBytes);

pub fn ada_token_name() -> TokenName {
    TokenName(LedgerBytes(Vec::with_capacity(0)))
}

/// AssetClass is uniquely identifying a specific asset
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct AssetClass {
    pub currency_symbol: CurrencySymbol,
    pub token_name: TokenName,
}
