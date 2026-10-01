//! Types related to rational values

#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::plutus_data::{IsPlutusData, PlutusData, PlutusDataError};

// TODO(chfanghr): maintain the invariants mentioned here: https://github.com/IntersectMBO/plutus/blob/master/plutus-tx/src/PlutusTx/Ratio.hs#L65-L68
/// Represents an arbitrary-precision ratio.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Rational(
    /// numerator
    pub BigInt,
    /// denominator
    pub BigInt,
);

impl IsPlutusData for Rational {
    fn to_plutus_data(&self) -> PlutusData {
        (self.0.clone(), self.1.clone()).to_plutus_data()
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError>
    where
        Self: Sized,
    {
        let (n, d) = IsPlutusData::from_plutus_data(plutus_data)?;

        Ok(Self(n, d))
    }
}
