#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use num_bigint::BigInt;
use std::collections::{BTreeMap, BTreeSet};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Data representation of on-chain data such as Datums and Redeemers
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "lbf", derive(Json))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum PlutusData {
    Constr(BigInt, Vec<PlutusData>),
    Map(Vec<(PlutusData, PlutusData)>),
    List(Vec<PlutusData>),
    Integer(BigInt),
    Bytes(Vec<u8>),
}

pub trait ToPlutusData {
    fn to_plutus_data(&self) -> PlutusData;
}

pub trait FromPlutusData {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError>
    where
        Self: Sized;
}

#[derive(Clone, Debug)]
pub enum PlutusType {
    Constr,
    Map,
    List,
    Integer,
    Bytes,
}

impl From<&PlutusData> for PlutusType {
    fn from(plutus_data: &PlutusData) -> Self {
        match plutus_data {
            PlutusData::Constr(_, _) => PlutusType::Constr,
            PlutusData::Map(_) => PlutusType::Map,
            PlutusData::List(_) => PlutusType::List,
            PlutusData::Integer(_) => PlutusType::Integer,
            PlutusData::Bytes(_) => PlutusType::Bytes,
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum PlutusDataError {
    #[error("Expected a PlutusData type {wanted:?}, but got {got:?}")]
    UnexpectedPlutusType { got: PlutusType, wanted: PlutusType },
    #[error("Expected a PlutusData type as {wanted:?}, but got {got:?}")]
    UnexpectedPlutusInvariant { got: String, wanted: String },
    #[error("Expected a Plutus List with {wanted:?} elements, but got {got:?} elements")]
    UnexpectedListLength { got: usize, wanted: usize },
    #[error("Some internal error happened: {0}")]
    InternalError(String),
}

impl ToPlutusData for BigInt {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Integer(self.clone())
    }
}

impl FromPlutusData for BigInt {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Integer(int) => Ok(int),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Integer,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl ToPlutusData for bool {
    fn to_plutus_data(&self) -> PlutusData {
        if *self {
            PlutusData::Constr(BigInt::from(1), Vec::with_capacity(0))
        } else {
            PlutusData::Constr(BigInt::from(0), Vec::with_capacity(0))
        }
    }
}

impl FromPlutusData for bool {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Constr(flag, fields) => match u32::try_from(&flag) {
                Ok(0) => {
                    verify_fields_len(&fields, 0)?;
                    Ok(false)
                }
                Ok(1) => {
                    verify_fields_len(&fields, 0)?;
                    Ok(true)
                }
                _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                    wanted: "Constr field between 0 and 1".to_owned(),
                    got: flag.to_string(),
                }),
            },

            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Constr,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl ToPlutusData for char {
    fn to_plutus_data(&self) -> PlutusData {
        String::from(*self).to_plutus_data()
    }
}

impl FromPlutusData for char {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        String::from_plutus_data(plutus_data).and_then(|str| {
            let mut chars = str.chars();
            let ch = chars.next();
            let rest = chars.next();
            match (ch, rest) {
                (Some(ch), None) => Ok(ch),
                _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                    got: "string".to_owned(),
                    wanted: "char".to_owned(),
                }),
            }
        })
    }
}

impl ToPlutusData for Vec<u8> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Bytes(self.clone())
    }
}

impl FromPlutusData for Vec<u8> {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Bytes(bytes) => Ok(bytes),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Bytes,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl ToPlutusData for String {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Bytes(self.as_bytes().into())
    }
}

impl FromPlutusData for String {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Bytes(bytes) => String::from_utf8(bytes).map_err(|err| {
                PlutusDataError::InternalError(format!(
                    "Couldn't convert Plutus bytes to String: {:?}",
                    err
                ))
            }),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Integer,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl<T> ToPlutusData for Option<T>
where
    T: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Some(val) => PlutusData::Constr(BigInt::from(0), vec![val.to_plutus_data()]),
            None => PlutusData::Constr(BigInt::from(1), Vec::with_capacity(0)),
        }
    }
}

impl<T> FromPlutusData for Option<T>
where
    T: FromPlutusData,
{
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Constr(flag, fields) => match u32::try_from(&flag) {
                Ok(0) => {
                    verify_fields_len(&fields, 1)?;
                    Ok(Some(T::from_plutus_data(fields[0].clone())?))
                }
                Ok(1) => {
                    verify_fields_len(&fields, 0)?;
                    Ok(None)
                }
                _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                    wanted: "Constr field between 0 and 1".to_owned(),
                    got: flag.to_string(),
                }),
            },

            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Constr,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl<T, E> ToPlutusData for Result<T, E>
where
    T: ToPlutusData,
    E: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Err(val) => PlutusData::Constr(BigInt::from(0), vec![val.to_plutus_data()]),
            Ok(val) => PlutusData::Constr(BigInt::from(1), vec![val.to_plutus_data()]),
        }
    }
}

impl<T, E> FromPlutusData for Result<T, E>
where
    T: FromPlutusData,
    E: FromPlutusData,
{
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Constr(flag, fields) => match u32::try_from(&flag) {
                Ok(0) => {
                    verify_fields_len(&fields, 1)?;
                    Ok(Err(E::from_plutus_data(fields[0].clone())?))
                }
                Ok(1) => {
                    verify_fields_len(&fields, 1)?;
                    Ok(Ok(T::from_plutus_data(fields[0].clone())?))
                }
                _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                    wanted: "Constr field between 0 and 1".to_owned(),
                    got: flag.to_string(),
                }),
            },

            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Constr,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl<T> ToPlutusData for Vec<T>
where
    T: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        let values = self
            .iter()
            .map(|val| val.to_plutus_data())
            .collect::<Vec<PlutusData>>();

        PlutusData::List(values)
    }
}

impl<T> FromPlutusData for Vec<T>
where
    T: FromPlutusData,
{
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::List(vec) => vec
                .iter()
                .map(|val| T::from_plutus_data(val.clone()))
                .collect::<Result<Vec<T>, PlutusDataError>>(),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::List,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl<T> ToPlutusData for BTreeSet<T>
where
    T: ToPlutusData + Eq + Ord,
{
    fn to_plutus_data(&self) -> PlutusData {
        let set = self
            .iter()
            .map(|val| val.to_plutus_data())
            .collect::<Vec<PlutusData>>();

        PlutusData::List(set)
    }
}

impl<T> FromPlutusData for BTreeSet<T>
where
    T: FromPlutusData + Eq + Ord,
{
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::List(vec) => vec
                .into_iter()
                .map(|val| T::from_plutus_data(val))
                .collect::<Result<Self, PlutusDataError>>(),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Map,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl<K, V> ToPlutusData for BTreeMap<K, V>
where
    K: ToPlutusData + Eq + Ord,
    V: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        let assoc_map = self
            .iter()
            .map(|(key, val)| (key.to_plutus_data(), val.to_plutus_data()))
            .collect::<Vec<(PlutusData, PlutusData)>>();

        PlutusData::Map(assoc_map)
    }
}

impl<K, V> FromPlutusData for BTreeMap<K, V>
where
    K: FromPlutusData + Eq + Ord,
    V: FromPlutusData,
{
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Map(dict) => dict
                .into_iter()
                .map(|(key, val)| Ok((K::from_plutus_data(key)?, V::from_plutus_data(val)?)))
                .collect::<Result<Self, PlutusDataError>>(),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Map,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl ToPlutusData for () {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(BigInt::from(0), Vec::with_capacity(0))
    }
}

impl FromPlutusData for () {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Constr(flag, fields) => match u32::try_from(&flag) {
                Ok(0) => {
                    verify_fields_len(&fields, 0)?;
                    Ok(())
                }
                _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                    wanted: "Constr field to be 0".to_owned(),
                    got: flag.to_string(),
                }),
            },

            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Constr,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

impl ToPlutusData for PlutusData {
    fn to_plutus_data(&self) -> PlutusData {
        self.clone()
    }
}

impl FromPlutusData for PlutusData {
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        Ok(plutus_data)
    }
}

impl<A, B> ToPlutusData for (A, B)
where
    A: ToPlutusData,
    B: ToPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(
            BigInt::from(0),
            vec![self.0.to_plutus_data(), self.1.to_plutus_data()],
        )
    }
}

impl<A, B> FromPlutusData for (A, B)
where
    A: FromPlutusData,
    B: FromPlutusData,
{
    fn from_plutus_data(plutus_data: PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Constr(flag, fields) => match u32::try_from(&flag) {
                Ok(0) => {
                    verify_fields_len(&fields, 2)?;
                    Ok((
                        A::from_plutus_data(fields[0].clone())?,
                        B::from_plutus_data(fields[1].clone())?,
                    ))
                }
                _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                    wanted: "Constr field to be 0".to_owned(),
                    got: flag.to_string(),
                }),
            },

            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Constr,
                got: PlutusType::from(&plutus_data),
            }),
        }
    }
}

fn verify_fields_len(fields: &Vec<PlutusData>, expected: usize) -> Result<(), PlutusDataError> {
    if fields.len() != expected {
        Err(PlutusDataError::UnexpectedPlutusInvariant {
            wanted: format!("Constr with {} fields", expected),
            got: format!("{:?}", fields),
        })
    } else {
        Ok(())
    }
}
