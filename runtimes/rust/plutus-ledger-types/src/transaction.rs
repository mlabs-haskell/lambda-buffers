use crate::address::Address;
use crate::crypto::LedgerBytes;
use crate::datum::OutputDatum;
use crate::interval::PlutusInterval;
use crate::script::ScriptHash;
use crate::value::Value;
#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use num_bigint::BigInt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An input of a transaction
///
/// Also know as `TxOutRef` from Plutus, this identifies a UTxO by its transacton hash and index
/// inside the transaction
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionInput {
    pub transaction_id: TransactionHash,
    pub index: BigInt,
}

/// 32-bytes blake2b256 hash of a transaction body.
///
/// Also known as Transaction ID or `TxID`.
/// Note: Plutus docs might incorrectly state that it uses SHA256.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionHash(pub LedgerBytes);

/// An output of a transaction
///
/// This must include a target address, an amount, an optional datum and an optional reference
/// script
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionOutput {
    pub address: Address,
    pub datum: OutputDatum,
    pub reference_script: Option<ScriptHash>,
    pub value: Value,
}

/// POSIX time is measured as the number of milliseconds since 1970-01-01T00:00:00Z
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct POSIXTime(pub BigInt);

pub type POSIXTimeRange = PlutusInterval<POSIXTime>;

/// An input of a pending transaction.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TxInInfo {
    pub transaction_input: TransactionInput,
    pub resolved: TransactionOutput,
}
