#[cfg(feature = "lbf")]
use data_encoding::HEXLOWER;
#[cfg(feature = "lbf")]
use lbr_prelude::json::{Error, Json};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Identifier of a validator script
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ValidatorHash(pub ScriptHash);

/// Hash of a minting policy script
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct MintingPolicyHash(pub ScriptHash);

/// Hash of a Plutus script
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ScriptHash(pub Vec<u8>);

#[cfg(feature = "lbf")]
impl Json for ScriptHash {
    fn to_json(&self) -> Result<serde_json::Value, Error> {
        String::to_json(&HEXLOWER.encode(&self.0))
    }

    fn from_json(value: serde_json::Value) -> Result<Self, Error> {
        let bytes = String::from_json(value).and_then(|str| {
            HEXLOWER
                .decode(&str.into_bytes())
                .map_err(|_| Error::UnexpectedJsonInvariant {
                    wanted: "base16 string".to_owned(),
                    got: "unexpected string".to_owned(),
                })
        })?;

        Ok(Self(bytes))
    }
}
