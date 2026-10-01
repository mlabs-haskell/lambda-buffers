use std::{ops::Neg, str::FromStr};

use cardano_serialization_lib as csl;
use num_bigint::{BigInt, ParseBigIntError};

#[derive(Debug, Clone, thiserror::Error)]
pub enum TryFromCSLError {
    #[error("Unable to parse BigInt: {0}")]
    InvalidBigInt(ParseBigIntError),

    #[error("Unable to represent CSL value in PLA: {0}")]
    ImpossibleConversion(String),
}

/// Convert a cardano-serialization-lib type to its plutus-ledger-api counterpart
pub trait FromCSL<T> {
    fn from_csl(value: &T) -> Self
    where
        Self: Sized;
}

pub trait ToPLA<T> {
    fn to_pla(&self) -> T;
}

impl<T, U> ToPLA<U> for T
where
    U: FromCSL<T>,
{
    fn to_pla(&self) -> U {
        FromCSL::from_csl(self)
    }
}

/// Convert a cardano-serialization-lib type to its plutus-ledger-api counterpart
pub trait TryFromCSL<T> {
    fn try_from_csl(value: &T) -> Result<Self, TryFromCSLError>
    where
        Self: Sized;
}

pub trait TryToPLA<T> {
    fn try_to_pla(&self) -> Result<T, TryFromCSLError>;
}

impl<T, U> TryToPLA<U> for T
where
    U: TryFromCSL<T>,
{
    fn try_to_pla(&self) -> Result<U, TryFromCSLError> {
        TryFromCSL::try_from_csl(self)
    }
}

impl FromCSL<csl::BigNum> for BigInt {
    fn from_csl(value: &csl::BigNum) -> Self {
        let x: u64 = From::from(*value);
        BigInt::from(x)
    }
}

impl FromCSL<u32> for BigInt {
    fn from_csl(value: &u32) -> Self {
        BigInt::from(*value)
    }
}

impl TryFromCSL<csl::BigInt> for BigInt {
    fn try_from_csl(value: &csl::BigInt) -> Result<Self, TryFromCSLError> {
        BigInt::from_str(&value.to_str()).map_err(TryFromCSLError::InvalidBigInt)
    }
}

impl FromCSL<csl::Int> for BigInt {
    fn from_csl(value: &csl::Int) -> Self {
        if value.is_positive() {
            BigInt::from_csl(&value.as_positive().unwrap())
        } else {
            BigInt::from_csl(&value.as_negative().unwrap()).neg()
        }
    }
}
