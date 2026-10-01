use crate::plutus_data::PlutusDataError;
use crate::plutus_data::{self, PlutusData};

use num_bigint::BigInt;

pub fn case_plutus_data<'a, T: 'a>(
    x0: Box<dyn 'a + FnOnce(&'a BigInt) -> Box<dyn 'a + FnOnce(&'a Vec<PlutusData>) -> T>>,
) -> Box<
    dyn 'a
        + FnOnce(
            Box<dyn FnOnce(&'a Vec<PlutusData>) -> T>,
        ) -> Box<
            dyn 'a
                + FnOnce(
                    Box<dyn FnOnce(&'a BigInt) -> T>,
                ) -> Box<
                    dyn 'a
                        + FnOnce(
                            Box<dyn FnOnce(&'a PlutusData) -> T>,
                        ) -> Box<dyn 'a + FnOnce(&'a PlutusData) -> T>,
                >,
        >,
> {
    Box::new(move |x1| {
        Box::new(move |x2| {
            Box::new(move |x3| {
                Box::new(move |x4| plutus_data::case_plutus_data(x0, x1, x2, x3, x4))
            })
        })
    })
}

pub fn constr(tag: u32) -> Box<dyn Fn(Vec<PlutusData>) -> PlutusData> {
    Box::new(move |fields| PlutusData::Constr(BigInt::from(tag), fields.clone()))
}

/// Fail PlutusData parsing with an internal error
pub fn fail_parse<T>() -> Result<T, PlutusDataError> {
    Err(PlutusDataError::InternalError(
        "Failed to parse PlutusData".to_owned(),
    ))
}

/// Curried Result::and_then function
pub fn bind_parse<'a, A: 'a, B: 'a>(
    x: Result<A, PlutusDataError>,
) -> Box<
    dyn FnOnce(Box<dyn Fn(&A) -> Result<B, PlutusDataError> + 'a>) -> Result<B, PlutusDataError>
        + 'a,
> {
    Box::new(move |f| x.and_then(|x1| f(&x1)))
}
