use crate::plutus_data::PlutusData;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Optional datum of a transaction
///
/// In case an inline datum is used, the data is embedded inside the transaction body, so it can be
/// directly retrieved. In case of a datum hash, an off-chain indexer is required to find the
/// associated datum by its hash.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum OutputDatum {
    None,
    DatumHash(DataHash),
    InlineDatum(Datum),
}

/// blake2b-256 hash of a datum
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DataHash(pub Vec<u8>);

/// Piece of information associated with a UTxO encoded into a PlutusData type.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Datum(pub PlutusData);
