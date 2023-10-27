#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use num_bigint::BigInt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Data representation of on-chain data such as Datums and Redeemers
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum PlutusData {
    Constr(BigInt, Vec<PlutusData>),
    Map(Vec<(PlutusData, PlutusData)>),
    List(Vec<PlutusData>),
    Integer(BigInt),
    Bytes(Vec<u8>),
}
