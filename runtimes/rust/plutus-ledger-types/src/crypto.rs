#[cfg(feature = "lbf")]
use data_encoding::HEXLOWER;
#[cfg(feature = "lbf")]
use lbr_prelude::json::{Error, Json};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// ED25519 public key hash
/// This is the standard cryptography in Cardano, commonly referred to as `PubKeyHash` in Plutus
/// and other libraries
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Ed25519PubKeyHash(pub LedgerBytes);

/// Standard public key hash used to verify a transaction witness
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct PaymentPubKeyHash(pub Ed25519PubKeyHash);

/// Standard public key hash used to verify a staking
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct StakePubKeyHash(pub Ed25519PubKeyHash);

/// A bytestring in the Cardano ledger context
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct LedgerBytes(pub Vec<u8>);

#[cfg(feature = "lbf")]
impl Json for LedgerBytes {
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
