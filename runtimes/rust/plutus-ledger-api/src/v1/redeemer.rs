//! Types related to Plutus Redeemers

use cardano_serialization_lib as csl;

#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate as plutus_ledger_api;
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};
use crate::plutus_data::{IsPlutusData, PlutusData};
use crate::v1::crypto::LedgerBytes;

//////////////
// Redeemer //
//////////////

/// Piece of information attached to a transaction when redeeming a value from a validator or a
/// minting policy
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Redeemer(pub PlutusData);

#[derive(Clone, Debug)]
pub struct RedeemerWithExtraInfo<'a> {
    pub redeemer: &'a Redeemer,
    pub tag: &'a csl::RedeemerTag,
    pub index: u64,
}

impl TryFromPLA<RedeemerWithExtraInfo<'_>> for csl::Redeemer {
    fn try_from_pla<'a>(val: &RedeemerWithExtraInfo<'_>) -> Result<csl::Redeemer, TryFromPLAError> {
        let Redeemer(plutus_data) = val.redeemer;
        Ok(csl::Redeemer::new(
            val.tag,
            &val.index.try_to_csl()?,
            &plutus_data.try_to_csl()?,
            &csl::ExUnits::new(&csl::BigNum::from(0u64), &csl::BigNum::from(0u64)),
        ))
    }
}

//////////////////
// RedeemerHash //
//////////////////

/// blake2b-256 hash of a datum
#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct RedeemerHash(pub LedgerBytes);
