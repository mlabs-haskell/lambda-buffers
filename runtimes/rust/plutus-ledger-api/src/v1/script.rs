//! Types related to Plutus scripts

use cardano_serialization_lib as csl;

#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate as plutus_ledger_api;
use crate::aux::guard_bytes;
use crate::csl::csl_to_pla::FromCSL;
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};
use crate::error::ConversionError;
use crate::plutus_data::IsPlutusData;
use crate::v1::crypto::LedgerBytes;

///////////////////
// ValidatorHash //
///////////////////

/// Identifier of a validator script
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ValidatorHash(pub ScriptHash);

impl FromCSL<csl::ScriptHash> for ValidatorHash {
    fn from_csl(value: &csl::ScriptHash) -> Self {
        ValidatorHash(ScriptHash::from_csl(value))
    }
}

impl ValidatorHash {
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        Ok(ValidatorHash(ScriptHash::from_bytes(bytes)?))
    }
}

///////////////////////
// MintingPolicyHash //
///////////////////////

/// Hash of a minting policy script
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct MintingPolicyHash(pub ScriptHash);

impl MintingPolicyHash {
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        Ok(MintingPolicyHash(ScriptHash::from_bytes(bytes)?))
    }
}

impl FromCSL<csl::PolicyID> for MintingPolicyHash {
    fn from_csl(value: &csl::PolicyID) -> Self {
        MintingPolicyHash(ScriptHash(LedgerBytes(value.to_bytes())))
    }
}

impl TryFromPLA<MintingPolicyHash> for csl::PolicyID {
    fn try_from_pla(val: &MintingPolicyHash) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

////////////////
// ScriptHash //
////////////////

/// Hash of a Plutus script
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ScriptHash(pub LedgerBytes);

impl ScriptHash {
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        Ok(ScriptHash(LedgerBytes(guard_bytes(
            "ScriptHash",
            bytes,
            28,
        )?)))
    }
}

impl FromCSL<csl::ScriptHash> for ScriptHash {
    fn from_csl(value: &csl::ScriptHash) -> Self {
        ScriptHash(LedgerBytes(value.to_bytes()))
    }
}

impl TryFromPLA<ScriptHash> for csl::ScriptHash {
    fn try_from_pla(val: &ScriptHash) -> Result<Self, TryFromPLAError> {
        csl::ScriptHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}
