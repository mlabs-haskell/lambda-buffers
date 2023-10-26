use crate::address::Address;
use crate::datum::OutputDatum;
use crate::script::ScriptHash;
use crate::value::Value;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An input of a transaction
///
/// Also know as `TxOutRef` from Plutus, this identifies a UTxO by its transacton hash and index
/// inside the transaction
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct TransactionInput {
    pub transaction_id: TransactionHash,
    pub index: u32,
}

/// 32-bytes blake2b256 hash of a transaction body.
///
/// Also known as Transaction ID or `TxID`.
/// Note: Plutus docs might incorrectly state that it uses SHA256.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct TransactionHash(pub Vec<u8>);

/// An output of a transaction
///
/// This must include a target address, an amount, an optional datum and an optional reference
/// script
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct TransactionOutput {
    pub address: Address,
    pub amount: Value,
    pub datum: OutputDatum,
    pub reference_script: Option<ScriptHash>,
}
