use crate::{
    error::Error,
    json::{case_json_constructor, json_constructor, Json},
};
use plutus_ledger_api::v2::datum::OutputDatum;

impl Json for OutputDatum {
    fn to_json(&self) -> serde_json::Value {
        match self {
            OutputDatum::None => json_constructor("NoOutputDatum", &Vec::with_capacity(0)),
            OutputDatum::DatumHash(dat_hash) => {
                json_constructor("OutputDatumHash", &vec![dat_hash.to_json()])
            }
            OutputDatum::InlineDatum(datum) => {
                json_constructor("OutputDatum", &vec![datum.to_json()])
            }
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        case_json_constructor(
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
                        [dat_hash] => Ok(OutputDatum::DatumHash(Json::from_json(&dat_hash)?)),
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
