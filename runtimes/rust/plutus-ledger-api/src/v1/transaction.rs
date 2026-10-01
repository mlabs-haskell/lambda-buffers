//! Types related to Cardano transactions.
use std::{fmt, str::FromStr};

use anyhow::anyhow;
use cardano_serialization_lib as csl;
#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use nom::{
    character::complete::char,
    combinator::{all_consuming, map, map_res},
    error::{context, VerboseError},
    sequence::{preceded, tuple},
    Finish, IResult,
};
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use super::{
    address::{Address, StakingCredential},
    crypto::{ledger_bytes, LedgerBytes, PaymentPubKeyHash},
    datum::{Datum, DatumHash},
    interval::PlutusInterval,
    value::{CurrencySymbol, Value},
};

use crate::{
    self as plutus_ledger_api,
    aux::{big_int, guard_bytes},
};
use crate::{
    csl::pla_to_csl::{TryFromPLAError, TryToCSL},
    plutus_data::IsPlutusData,
};
use crate::{
    csl::{csl_to_pla::FromCSL, pla_to_csl::TryFromPLA},
    error::ConversionError,
};

//////////////////////
// TransactionInput //
//////////////////////

/// An input of a transaction
///
/// Also know as `TxOutRef` from Plutus, this identifies a UTxO by its transacton hash and index
/// inside the transaction
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionInput {
    pub transaction_id: TransactionHash,
    pub index: BigInt,
}

/// Serializing into a hexadecimal tx hash, followed by an tx id after a # (e.g. aabbcc#1)
impl fmt::Display for TransactionInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.transaction_id.0, self.index)
    }
}

impl FromCSL<csl::TransactionInput> for TransactionInput {
    fn from_csl(value: &csl::TransactionInput) -> Self {
        TransactionInput {
            transaction_id: TransactionHash::from_csl(&value.transaction_id()),
            index: BigInt::from_csl(&value.index()),
        }
    }
}

impl TryFromPLA<TransactionInput> for csl::TransactionInput {
    fn try_from_pla(val: &TransactionInput) -> Result<Self, TryFromPLAError> {
        Ok(csl::TransactionInput::new(
            &val.transaction_id.try_to_csl()?,
            val.index.try_to_csl()?,
        ))
    }
}

impl FromCSL<csl::TransactionInputs> for Vec<TransactionInput> {
    fn from_csl(value: &csl::TransactionInputs) -> Self {
        (0..value.len())
            .map(|idx| TransactionInput::from_csl(&value.get(idx)))
            .collect()
    }
}

impl TryFromPLA<Vec<TransactionInput>> for csl::TransactionInputs {
    fn try_from_pla(val: &Vec<TransactionInput>) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::TransactionInputs::new(), |mut acc, input| {
                acc.add(&input.try_to_csl()?);
                Ok(acc)
            })
    }
}

/// Nom parser for TransactionInput
/// Expects a transaction hash of 32 bytes in hexadecimal followed by a # and an integer index
/// E.g.: 1122334455667788990011223344556677889900112233445566778899001122#1
pub(crate) fn transaction_input(
    input: &str,
) -> IResult<&str, TransactionInput, VerboseError<&str>> {
    map(
        tuple((transaction_hash, preceded(char('#'), big_int))),
        |(transaction_id, index)| TransactionInput {
            transaction_id,
            index,
        },
    )(input)
}

impl FromStr for TransactionInput {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(transaction_input)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing TransactionInput '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

/////////////////////
// TransactionHash //
/////////////////////

/// 32-bytes blake2b256 hash of a transaction body.
///
/// Also known as Transaction ID or `TxID`.
/// Note: Plutus docs might incorrectly state that it uses SHA256.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionHash(pub LedgerBytes);

impl fmt::Display for TransactionHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TransactionHash {
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        Ok(TransactionHash(LedgerBytes(guard_bytes(
            "ScriptHash",
            bytes,
            32,
        )?)))
    }
}

impl FromCSL<csl::TransactionHash> for TransactionHash {
    fn from_csl(value: &csl::TransactionHash) -> Self {
        TransactionHash(LedgerBytes(value.to_bytes()))
    }
}

impl TryFromPLA<TransactionHash> for csl::TransactionHash {
    fn try_from_pla(val: &TransactionHash) -> Result<Self, TryFromPLAError> {
        csl::TransactionHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}

/// Nom parser for TransactionHash
/// Expects a hexadecimal string representation of 32 bytes
/// E.g.: 1122334455667788990011223344556677889900112233445566778899001122
pub(crate) fn transaction_hash(input: &str) -> IResult<&str, TransactionHash, VerboseError<&str>> {
    context(
        "transaction_hash",
        map_res(ledger_bytes, |LedgerBytes(bytes)| {
            TransactionHash::from_bytes(bytes)
        }),
    )(input)
}

impl FromStr for TransactionHash {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(transaction_hash)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing TransactionHash '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

///////////////////////
// TransactionOutput //
///////////////////////

/// An output of a transaction
///
/// This must include the target address, the hash of the datum attached, and the amount of output
/// tokens
#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionOutput {
    pub address: Address,
    pub value: Value,
    pub datum_hash: Option<DatumHash>,
}

///////////////
// POSIXTime //
///////////////

/// POSIX time is measured as the number of milliseconds since 1970-01-01T00:00:00Z
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct POSIXTime(pub BigInt);

#[cfg(feature = "chrono")]
#[derive(thiserror::Error, Debug)]
pub enum POSIXTimeConversionError {
    #[error(transparent)]
    TryFromBigIntError(#[from] num_bigint::TryFromBigIntError<BigInt>),
    #[error("POSIXTime is out of bounds.")]
    OutOfBoundsError,
}

#[cfg(feature = "chrono")]
impl<Tz: chrono::TimeZone> From<chrono::DateTime<Tz>> for POSIXTime {
    fn from(datetime: chrono::DateTime<Tz>) -> POSIXTime {
        POSIXTime(BigInt::from(datetime.timestamp_millis()))
    }
}

#[cfg(feature = "chrono")]
impl TryFrom<POSIXTime> for chrono::DateTime<chrono::Utc> {
    type Error = POSIXTimeConversionError;

    fn try_from(posix_time: POSIXTime) -> Result<chrono::DateTime<chrono::Utc>, Self::Error> {
        let POSIXTime(millis) = posix_time;
        chrono::DateTime::from_timestamp_millis(
            <i64>::try_from(millis).map_err(POSIXTimeConversionError::TryFromBigIntError)?,
        )
        .ok_or(POSIXTimeConversionError::OutOfBoundsError)
    }
}

////////////////////
// POSIXTimeRange //
////////////////////

pub type POSIXTimeRange = PlutusInterval<POSIXTime>;

//////////////
// TxInInfo //
//////////////

/// An input of a pending transaction.
#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TxInInfo {
    pub reference: TransactionInput,
    pub output: TransactionOutput,
}

impl From<(TransactionInput, TransactionOutput)> for TxInInfo {
    fn from((reference, output): (TransactionInput, TransactionOutput)) -> TxInInfo {
        TxInInfo { reference, output }
    }
}

///////////
// DCert //
///////////

/// Partial representation of digests of certificates on the ledger.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum DCert {
    DelegRegKey(StakingCredential),
    DelegDeRegKey(StakingCredential),
    DelegDelegate(
        /// Delegator
        StakingCredential,
        /// Delegatee
        PaymentPubKeyHash,
    ),
    /// A digest of the PoolParam
    PoolRegister(
        /// Pool id
        PaymentPubKeyHash,
        /// Pool VFR
        PaymentPubKeyHash,
    ),
    PoolRetire(
        PaymentPubKeyHash,
        /// Epoch
        BigInt,
    ),
    Genesis,
    Mir,
}

///////////////////
// ScriptPurpose //
///////////////////

/// The purpose of the script that's currently running.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum ScriptPurpose {
    Minting(CurrencySymbol),
    Spending(TransactionInput),
    Rewarding(StakingCredential),
    Certifying(DCert),
}

/////////////////////
// TransactionInfo //
/////////////////////

/// A pending transaction as seen by validator scripts, also known as TxInfo in Plutus
#[derive(Debug, PartialEq, Eq, Clone, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionInfo {
    pub inputs: Vec<TxInInfo>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Value,
    pub mint: Value,
    pub d_cert: Vec<DCert>,
    pub wdrl: Vec<(StakingCredential, BigInt)>,
    pub valid_range: POSIXTimeRange,
    pub signatories: Vec<PaymentPubKeyHash>,
    pub datums: Vec<(DatumHash, Datum)>,
    pub id: TransactionHash,
}

///////////////////
// ScriptContext //
///////////////////

/// The context that is presented to the currently-executing script.
#[derive(Debug, PartialEq, Eq, Clone, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ScriptContext {
    pub tx_info: TransactionInfo,
    pub purpose: ScriptPurpose,
}
