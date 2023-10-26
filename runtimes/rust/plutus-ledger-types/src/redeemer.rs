use crate::plutus_data::PlutusData;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Piece of information attached to a transaction when redeeming a value from a validator or a
/// minting policy
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Redeemer(pub PlutusData);
