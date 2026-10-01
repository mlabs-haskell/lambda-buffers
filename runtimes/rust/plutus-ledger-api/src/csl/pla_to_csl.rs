use cardano_serialization_lib as csl;
use num_bigint::{BigInt, TryFromBigIntError};
use num_traits::sign::Signed;

#[derive(Debug, thiserror::Error)]
pub enum TryFromPLAError {
    #[error("{0}")]
    CSLDeserializeError(csl::DeserializeError),

    #[error("{0}")]
    CSLJsError(csl::JsError),

    #[error("Unable to cast BigInt {0} into type {1}: value is out of bound")]
    BigIntOutOfRange(BigInt, String),

    #[error("Unable to represent PLA value in CSL: ${0}")]
    ImpossibleConversion(String),

    #[error("Invalid valid transaction time range: ${0:?}")]
    InvalidTimeRange(crate::v2::transaction::POSIXTimeRange),

    #[error("Script is missing from context: {0:?}")]
    MissingScript(crate::v2::script::ScriptHash),
}

/// Convert a plutus-ledger-api type to its cardano-serialization-lib counterpart
/// `try_to_csl_with` accepts extra data where the PLA data itself is not enough
pub trait TryFromPLA<T> {
    fn try_from_pla(val: &T) -> Result<Self, TryFromPLAError>
    where
        Self: Sized;
}

/// Convert a plutus-ledger-api type to its cardano-serialization-lib counterpart
/// `try_to_csl_with` accepts extra data where the PLA data itself is not enough
///
/// DO NOT IMPLEMENT THIS DIRECTLY. Implement `TryFromPLA` instead.
pub trait TryToCSL<T> {
    fn try_to_csl(&self) -> Result<T, TryFromPLAError>;
}

impl<T, U> TryToCSL<U> for T
where
    U: TryFromPLA<T>,
{
    fn try_to_csl(&self) -> Result<U, TryFromPLAError> {
        TryFromPLA::try_from_pla(self)
    }
}

impl TryFromPLA<u64> for csl::BigNum {
    fn try_from_pla(val: &u64) -> Result<Self, TryFromPLAError> {
        // BigNum(s) are u64 under the hood.

        Ok(csl::BigNum::from(*val))
    }
}

impl TryFromPLA<BigInt> for csl::BigNum {
    fn try_from_pla(val: &BigInt) -> Result<Self, TryFromPLAError> {
        // BigNum(s) are u64 under the hood.
        let x: u64 = val
            .to_owned()
            .try_into()
            .map_err(|err: TryFromBigIntError<BigInt>| {
                TryFromPLAError::BigIntOutOfRange(err.into_original(), "u64".into())
            })?;

        x.try_to_csl()
    }
}

impl TryFromPLA<BigInt> for csl::BigInt {
    fn try_from_pla(val: &BigInt) -> Result<Self, TryFromPLAError> {
        Ok(val.to_owned().into())
    }
}

impl TryFromPLA<BigInt> for csl::Int {
    fn try_from_pla(val: &BigInt) -> Result<Self, TryFromPLAError> {
        if val.is_negative() {
            Ok(csl::Int::new_negative(&(val.abs()).try_to_csl()?))
        } else {
            Ok(csl::Int::new(&val.try_to_csl()?))
        }
    }
}

impl TryFromPLA<i64> for csl::Int {
    fn try_from_pla(val: &i64) -> Result<Self, TryFromPLAError> {
        if val.is_negative() {
            Ok(csl::Int::new_negative(&csl::BigNum::from(
                val.unsigned_abs(),
            )))
        } else {
            Ok(csl::Int::new(&csl::BigNum::from(*val as u64)))
        }
    }
}

impl TryFromPLA<BigInt> for u32 /* TransactionIndex */ {
    fn try_from_pla(val: &BigInt) -> Result<Self, TryFromPLAError> {
        val.to_owned()
            .try_into()
            .map_err(|err: TryFromBigIntError<BigInt>| {
                TryFromPLAError::BigIntOutOfRange(err.into_original(), "u32".into())
            })
    }
}
