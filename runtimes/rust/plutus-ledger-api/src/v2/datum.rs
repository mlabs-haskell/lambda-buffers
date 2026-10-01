//! Types related to Plutus Datums

use cardano_serialization_lib as csl;
#[cfg(feature = "lbf")]
use lbr_prelude::json::{self, Error, Json};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate as plutus_ledger_api;
pub use crate::v1::datum::{Datum, DatumHash};
use crate::{
    csl::{
        csl_to_pla::{FromCSL, TryFromCSL, TryFromCSLError, TryToPLA},
        pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL},
    },
    plutus_data::IsPlutusData,
};

// use crate as plutus_data/
/////////////////
// OutputDatum //
/////////////////

/// Optional datum of a transaction
///
/// In case an inline datum is used, the data is embedded inside the transaction body, so it can be
/// directly retrieved. In case of a datum hash, an off-chain indexer is required to find the
/// associated datum by its hash.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum OutputDatum {
    None,
    DatumHash(DatumHash),
    InlineDatum(Datum),
}

#[cfg(feature = "lbf")]
impl Json for OutputDatum {
    fn to_json(&self) -> serde_json::Value {
        match self {
            OutputDatum::None => json::json_constructor("NoOutputDatum", Vec::new()),
            OutputDatum::DatumHash(dat_hash) => {
                json::json_constructor("OutputDatumHash", vec![dat_hash.to_json()])
            }
            OutputDatum::InlineDatum(datum) => {
                json::json_constructor("OutputDatum", vec![datum.to_json()])
            }
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        json::case_json_constructor(
            "Plutus.V2.OutputDatum",
            vec![
                (
                    "NoOutputDatum",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(OutputDatum::None),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: "Plutus.V2.OutputDatum".to_owned(),
                        }),
                    }),
                ),
                (
                    "OutputDatumHash",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [dat_hash] => Ok(OutputDatum::DatumHash(Json::from_json(dat_hash)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V2.OutputDatum".to_owned(),
                        }),
                    }),
                ),
                (
                    "OutputDatum",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [datum] => Ok(OutputDatum::InlineDatum(Json::from_json(datum)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V2.OutputDatum".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl TryFromCSL<csl::OutputDatum> for OutputDatum {
    fn try_from_csl(value: &csl::OutputDatum) -> Result<Self, TryFromCSLError> {
        Ok(if let Some(d) = value.data() {
            OutputDatum::InlineDatum(Datum(d.try_to_pla()?))
        } else if let Some(h) = value.data_hash() {
            OutputDatum::DatumHash(DatumHash::from_csl(&h))
        } else {
            OutputDatum::None
        })
    }
}

impl TryFromPLA<OutputDatum> for Option<csl::OutputDatum> {
    fn try_from_pla(
        pla_output_datum: &OutputDatum,
    ) -> Result<Option<csl::OutputDatum>, TryFromPLAError> {
        Ok(match pla_output_datum {
            OutputDatum::None => None,
            OutputDatum::InlineDatum(Datum(d)) => {
                Some(csl::OutputDatum::new_data(&d.try_to_csl()?))
            }
            OutputDatum::DatumHash(dh) => Some(csl::OutputDatum::new_data_hash(&dh.try_to_csl()?)),
        })
    }
}
