//! Types related to Plutus Datums

use cardano_serialization_lib as csl;

use crate as plutus_ledger_api;
use crate::csl::csl_to_pla::FromCSL;
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};
use crate::plutus_data::{IsPlutusData, PlutusData};
use crate::v1::crypto::LedgerBytes;
#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

///////////////
// DatumHash //
///////////////

/// blake2b-256 hash of a datum
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DatumHash(pub LedgerBytes);

impl FromCSL<csl::DataHash> for DatumHash {
    fn from_csl(value: &csl::DataHash) -> Self {
        DatumHash(LedgerBytes(value.to_bytes()))
    }
}

impl TryFromPLA<DatumHash> for csl::DataHash {
    fn try_from_pla(val: &DatumHash) -> Result<Self, TryFromPLAError> {
        csl::DataHash::from_bytes(val.0 .0.to_owned()).map_err(TryFromPLAError::CSLDeserializeError)
    }
}

///////////
// Datum //
///////////

/// Piece of information associated with a UTxO encoded into a PlutusData type.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Datum(pub PlutusData);

impl TryFromPLA<Datum> for csl::PlutusData {
    fn try_from_pla(val: &Datum) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}
