//! Plutus Data related types and traits

use std::collections::{BTreeMap, BTreeSet};

use cardano_serialization_lib as csl;
use num_bigint::BigInt;

use crate::csl::csl_to_pla::{FromCSL, TryFromCSL, TryFromCSLError, TryToPLA};
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};

pub use is_plutus_data_derive::IsPlutusData;

#[cfg(feature = "lbf")]
use data_encoding::HEXLOWER;
#[cfg(feature = "lbf")]
use lbr_prelude::error::Error;
#[cfg(feature = "lbf")]
use lbr_prelude::json::{
    case_json_constructor, case_json_object, json_constructor, json_object, Json,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Data representation of on-chain data such as Datums and Redeemers
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum PlutusData {
    Constr(BigInt, Vec<PlutusData>),
    Map(Vec<(PlutusData, PlutusData)>),
    List(Vec<PlutusData>),
    Integer(BigInt),
    Bytes(Vec<u8>),
}

#[derive(Clone, Debug)]
pub enum PlutusType {
    Constr,
    Map,
    List,
    Integer,
    Bytes,
}

pub trait IsPlutusData {
    fn to_plutus_data(&self) -> PlutusData;

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError>
    where
        Self: Sized;
}

// TODO(chfanghr): improve error reporting
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

impl PlutusData {
    pub fn constr(tag: u32, fields: Vec<PlutusData>) -> Self {
        PlutusData::Constr(BigInt::from(tag), fields)
    }

    pub fn map(fields: Vec<(PlutusData, PlutusData)>) -> Self {
        PlutusData::Map(fields)
    }

    pub fn list(fields: Vec<PlutusData>) -> Self {
        PlutusData::List(fields)
    }

    pub fn integer(value: u32) -> Self {
        PlutusData::Integer(BigInt::from(value))
    }

    pub fn bytes(value: Vec<u8>) -> Self {
        PlutusData::Bytes(value)
    }
}

impl IsPlutusData for PlutusData {
    fn to_plutus_data(&self) -> PlutusData {
        self.clone()
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        Ok(plutus_data.clone())
    }
}

impl TryFromCSL<csl::PlutusData> for PlutusData {
    fn try_from_csl(value: &csl::PlutusData) -> Result<Self, TryFromCSLError> {
        Ok(match value.kind() {
            csl::PlutusDataKind::ConstrPlutusData => {
                let constr_data = value.as_constr_plutus_data().unwrap();
                let tag = BigInt::from_csl(&constr_data.alternative());
                let args = constr_data.data().try_to_pla()?;
                PlutusData::Constr(tag, args)
            }
            csl::PlutusDataKind::Map => PlutusData::Map(value.as_map().unwrap().try_to_pla()?),
            csl::PlutusDataKind::List => PlutusData::List(value.as_list().unwrap().try_to_pla()?),
            csl::PlutusDataKind::Integer => {
                PlutusData::Integer(value.as_integer().unwrap().try_to_pla()?)
            }
            csl::PlutusDataKind::Bytes => PlutusData::Bytes(value.as_bytes().unwrap()),
        })
    }
}

#[cfg(feature = "lbf")]
impl Json for PlutusData {
    fn to_json(&self) -> serde_json::Value {
        match self {
            PlutusData::Constr(index, fields) => json_constructor(
                "Constr",
                vec![json_object(vec![
                    ("index".to_string(), index.to_json()),
                    ("fields".to_string(), fields.to_json()),
                ])],
            ),
            PlutusData::Map(map) => json_constructor("Map", vec![map.to_json()]),
            PlutusData::List(list) => json_constructor("List", vec![list.to_json()]),
            PlutusData::Integer(int) => json_constructor("Integer", vec![int.to_json()]),
            PlutusData::Bytes(bytes) => {
                json_constructor("Bytes", vec![String::to_json(&HEXLOWER.encode(bytes))])
            }
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<PlutusData, Error> {
        case_json_constructor(
            "PlutusV1.PlutusData",
            vec![
                (
                    "Constr",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => case_json_object(
                            |obj| {
                                let index = obj.get("index").ok_or(Error::UnexpectedFieldName {
                                    wanted: "index".to_owned(),
                                    got: obj.keys().cloned().collect(),
                                    parser: "PlutusV1.PlutusData".to_owned(),
                                })?;

                                let fields =
                                    obj.get("fields").ok_or(Error::UnexpectedFieldName {
                                        wanted: "fields".to_owned(),
                                        got: obj.keys().cloned().collect(),
                                        parser: "PlutusV1.PlutusData".to_owned(),
                                    })?;
                                Ok(PlutusData::Constr(
                                    BigInt::from_json(index)?,
                                    <Vec<PlutusData>>::from_json(fields)?,
                                ))
                            },
                            val,
                        ),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "Map",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(PlutusData::Map(Json::from_json(val)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "List",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(PlutusData::List(Json::from_json(val)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "Integer",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => Ok(PlutusData::Integer(Json::from_json(val)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
                (
                    "Bytes",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val] => {
                            let bytes = String::from_json(val).and_then(|str| {
                                HEXLOWER.decode(&str.into_bytes()).map_err(|_| {
                                    Error::UnexpectedJsonInvariant {
                                        wanted: "base16 string".to_owned(),
                                        got: "unexpected string".to_owned(),
                                        parser: "Plutus.V1.Bytes".to_owned(),
                                    }
                                })
                            })?;
                            Ok(PlutusData::Bytes(bytes))
                        }
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "PlutusV1.PlutusData".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

// MARK: Orphan IsPlutusData Instances

impl IsPlutusData for BigInt {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Integer(self.clone())
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Integer(int) => Ok(int.clone()),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Integer,
                got: PlutusType::from(plutus_data),
            }),
        }
    }
}

impl IsPlutusData for Vec<u8> {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Bytes(self.clone())
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Bytes(bytes) => Ok(bytes.clone()),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Bytes,
                got: PlutusType::from(plutus_data),
            }),
        }
    }
}

const BOOL_FALSE_TAG: u32 = 0;
const BOOL_TRUE_TAG: u32 = 1;

impl IsPlutusData for bool {
    fn to_plutus_data(&self) -> PlutusData {
        if *self {
            PlutusData::Constr(BOOL_TRUE_TAG.into(), vec![])
        } else {
            PlutusData::Constr(BOOL_FALSE_TAG.into(), vec![])
        }
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        let (tag, fields) = parse_constr(plutus_data)?;
        let [] = parse_fixed_len_constr_fields::<0>(fields)?;
        match tag {
            BOOL_TRUE_TAG => Ok(true),
            BOOL_FALSE_TAG => Ok(false),
            _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                wanted: format!("Constr with tag {BOOL_TRUE_TAG} or {BOOL_FALSE_TAG}"),
                got: tag.to_string(),
            }),
        }
    }
}

impl IsPlutusData for String {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Bytes(self.as_bytes().into())
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Bytes(bytes) => String::from_utf8(bytes.clone()).map_err(|err| {
                PlutusDataError::InternalError(format!(
                    "Couldn't convert Plutus bytes to String: {:?}",
                    err
                ))
            }),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Bytes,
                got: PlutusType::from(plutus_data),
            }),
        }
    }
}

impl IsPlutusData for char {
    fn to_plutus_data(&self) -> PlutusData {
        String::from(*self).to_plutus_data()
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
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

const OPTION_SOME_TAG: u32 = 0;
const OPTION_NONE_TAG: u32 = 1;

impl<T> IsPlutusData for Option<T>
where
    T: IsPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Some(val) => PlutusData::Constr(OPTION_SOME_TAG.into(), vec![val.to_plutus_data()]),
            None => PlutusData::Constr(OPTION_NONE_TAG.into(), vec![]),
        }
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        let (tag, fields) = parse_constr(plutus_data)?;

        match tag {
            OPTION_SOME_TAG => {
                let [data] = parse_fixed_len_constr_fields::<1>(fields)?;
                Ok(Some(T::from_plutus_data(data)?))
            }
            OPTION_NONE_TAG => {
                let [] = parse_fixed_len_constr_fields::<0>(fields)?;
                Ok(None)
            }
            _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                wanted: format!("Constr with tag {OPTION_SOME_TAG} or {OPTION_NONE_TAG}"),
                got: tag.to_string(),
            }),
        }
    }
}

const RESULT_ERR_TAG: u32 = 0;
const RESULT_OK_TAG: u32 = 1;

impl<T, E> IsPlutusData for Result<T, E>
where
    T: IsPlutusData,
    E: IsPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            Err(err) => PlutusData::Constr(RESULT_ERR_TAG.into(), vec![err.to_plutus_data()]),
            Ok(val) => PlutusData::Constr(RESULT_OK_TAG.into(), vec![val.to_plutus_data()]),
        }
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        let (tag, fields) = parse_constr(plutus_data)?;
        let [field] = parse_fixed_len_constr_fields::<1>(fields)?;

        match tag {
            RESULT_ERR_TAG => Ok(Err(E::from_plutus_data(field)?)),
            RESULT_OK_TAG => Ok(Ok(T::from_plutus_data(field)?)),
            _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                wanted: format!("Constr with tag {RESULT_ERR_TAG} or {RESULT_OK_TAG}"),
                got: tag.to_string(),
            }),
        }
    }
}

impl<T> IsPlutusData for Vec<T>
where
    T: IsPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        let values = self
            .iter()
            .map(|val| val.to_plutus_data())
            .collect::<Vec<PlutusData>>();

        PlutusData::List(values)
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        let list = parse_list(plutus_data)?;
        list.iter().map(T::from_plutus_data).collect()
    }
}

impl<T> IsPlutusData for BTreeSet<T>
where
    T: IsPlutusData + Eq + Ord,
{
    fn to_plutus_data(&self) -> PlutusData {
        let set = self
            .iter()
            .map(|val| val.to_plutus_data())
            .collect::<Vec<PlutusData>>();

        PlutusData::List(set)
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::List(vec) => vec
                .iter()
                .map(|val| T::from_plutus_data(val))
                .collect::<Result<Self, PlutusDataError>>(),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::List,
                got: PlutusType::from(plutus_data),
            }),
        }
    }
}

impl<K, V> IsPlutusData for BTreeMap<K, V>
where
    K: IsPlutusData + Eq + Ord,
    V: IsPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        let assoc_map = self
            .iter()
            .map(|(key, val)| (key.to_plutus_data(), val.to_plutus_data()))
            .collect::<Vec<(PlutusData, PlutusData)>>();

        PlutusData::Map(assoc_map)
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        match plutus_data {
            PlutusData::Map(dict) => dict
                .iter()
                .map(|(key, val)| Ok((K::from_plutus_data(key)?, V::from_plutus_data(val)?)))
                .collect::<Result<Self, PlutusDataError>>(),
            _ => Err(PlutusDataError::UnexpectedPlutusType {
                wanted: PlutusType::Map,
                got: PlutusType::from(plutus_data),
            }),
        }
    }
}

const UNIT_TAG: u32 = 0;

impl IsPlutusData for () {
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(UNIT_TAG.into(), vec![])
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        let fields = parse_constr_with_tag(plutus_data, UNIT_TAG)?;
        let [] = parse_fixed_len_constr_fields::<0>(fields)?;
        Ok(())
    }
}

const PAIR_TAG: u32 = 0;

impl<A, B> IsPlutusData for (A, B)
where
    A: IsPlutusData,
    B: IsPlutusData,
{
    fn to_plutus_data(&self) -> PlutusData {
        PlutusData::Constr(
            BigInt::from(PAIR_TAG),
            vec![self.0.to_plutus_data(), self.1.to_plutus_data()],
        )
    }

    fn from_plutus_data(plutus_data: &PlutusData) -> Result<Self, PlutusDataError> {
        let fields = parse_constr_with_tag(plutus_data, PAIR_TAG)?;
        let [a, b] = parse_fixed_len_constr_fields::<2>(fields)?;
        Ok((A::from_plutus_data(a)?, B::from_plutus_data(b)?))
    }
}

// MARK: Orphan TryFromCSL instances

impl TryFromCSL<csl::PlutusList> for Vec<PlutusData> {
    fn try_from_csl(value: &csl::PlutusList) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| value.get(idx).try_to_pla())
            .collect()
    }
}

impl TryFromCSL<csl::PlutusMap> for Vec<(PlutusData, PlutusData)> {
    fn try_from_csl(c_map: &csl::PlutusMap) -> Result<Self, TryFromCSLError> {
        let keys = c_map.keys();
        (0..keys.len()).try_fold(Vec::new(), |mut vector, idx| {
            let key = keys.get(idx);
            let values = c_map.get(&key).unwrap();

            for value_idx in 0..values.len() {
                vector.push((
                    key.clone().try_to_pla()?,
                    values.get(value_idx).unwrap().try_to_pla()?,
                ))
            }

            Ok(vector)
        })
    }
}

impl TryFromPLA<PlutusData> for csl::PlutusData {
    fn try_from_pla(val: &PlutusData) -> Result<Self, TryFromPLAError> {
        match val {
            PlutusData::Constr(tag, args) => Ok(csl::PlutusData::new_constr_plutus_data(
                &csl::ConstrPlutusData::new(&tag.try_to_csl()?, &args.try_to_csl()?),
            )),
            PlutusData::Map(l) => Ok(csl::PlutusData::new_map(&l.try_to_csl()?)),
            PlutusData::List(l) => Ok(csl::PlutusData::new_list(&l.try_to_csl()?)),
            PlutusData::Integer(i) => Ok(csl::PlutusData::new_integer(&i.try_to_csl()?)),
            PlutusData::Bytes(b) => Ok(csl::PlutusData::new_bytes(b.to_owned())),
        }
    }
}

impl TryFromPLA<Vec<PlutusData>> for csl::PlutusList {
    fn try_from_pla(val: &Vec<PlutusData>) -> Result<Self, TryFromPLAError> {
        val.iter()
            // traverse
            .map(|x| x.try_to_csl())
            .collect::<Result<Vec<csl::PlutusData>, TryFromPLAError>>()
            .map(|x| x.into())
    }
}

impl TryFromPLA<Vec<(PlutusData, PlutusData)>> for csl::PlutusMap {
    fn try_from_pla(val: &Vec<(PlutusData, PlutusData)>) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::PlutusMap::new(), |mut acc, (k, v)| {
                let mut values = match acc.get(&k.try_to_csl()?) {
                    Some(existing_values) => existing_values,
                    None => csl::PlutusMapValues::new(),
                };
                values.add(&v.try_to_csl()?);
                acc.insert(&k.try_to_csl()?, &values);
                Ok(acc)
            })
    }
}

// MARK: Aux functions

/// Deserialise a Plutus data using parsers for each variant
pub fn case_plutus_data<'a, T>(
    ctor_case: impl FnOnce(&'a BigInt) -> Box<dyn 'a + FnOnce(&'a Vec<PlutusData>) -> T>,
    list_case: impl FnOnce(&'a Vec<PlutusData>) -> T,
    int_case: impl FnOnce(&'a BigInt) -> T,
    other_case: impl FnOnce(&'a PlutusData) -> T,
    pd: &'a PlutusData,
) -> T {
    match pd {
        PlutusData::Constr(tag, args) => ctor_case(&tag)(&args),
        PlutusData::List(args) => list_case(&args),
        PlutusData::Integer(i) => int_case(&i),
        other => other_case(&other),
    }
}

/// Given a vector of PlutusData, parse it as an array whose length is known at
/// compile time.
///
/// This function is used by the derive macro.
pub fn parse_fixed_len_constr_fields<const LEN: usize>(
    v: &[PlutusData],
) -> Result<&[PlutusData; LEN], PlutusDataError> {
    v.try_into()
        .map_err(|_| PlutusDataError::UnexpectedListLength {
            got: v.len(),
            wanted: LEN,
        })
}

/// Given a PlutusData, parse it as PlutusData::Constr and its tag as u32. Return
/// the u32 tag and fields.
///
/// This function is used by the derive macro.
pub fn parse_constr(data: &PlutusData) -> Result<(u32, &Vec<PlutusData>), PlutusDataError> {
    match data {
        PlutusData::Constr(tag, fields) => u32::try_from(tag)
            .map_err(|err| PlutusDataError::UnexpectedPlutusInvariant {
                got: err.to_string(),
                wanted: "Constr bigint tag within u32 range".into(),
            })
            .map(|tag| (tag, fields)),
        _ => Err(PlutusDataError::UnexpectedPlutusType {
            wanted: PlutusType::Constr,
            got: PlutusType::from(data),
        }),
    }
}

/// Given a PlutusData, parse it as PlutusData::Constr and verify its tag.
///
/// This function is used by the derive macro.
pub fn parse_constr_with_tag(
    data: &PlutusData,
    expected_tag: u32,
) -> Result<&Vec<PlutusData>, PlutusDataError> {
    let (tag, fields) = parse_constr(data)?;

    if tag != expected_tag {
        Err(PlutusDataError::UnexpectedPlutusInvariant {
            got: tag.to_string(),
            wanted: format!("Constr with tag {}", expected_tag),
        })
    } else {
        Ok(fields)
    }
}

/// Given a PlutusData, parse it as PlutusData::List. Return the plutus data list.
///
/// This function is used by the derive macro.
pub fn parse_list(data: &PlutusData) -> Result<&Vec<PlutusData>, PlutusDataError> {
    match data {
        PlutusData::List(list_of_plutus_data) => Ok(list_of_plutus_data),
        _ => Err(PlutusDataError::UnexpectedPlutusType {
            got: PlutusType::from(data),
            wanted: PlutusType::List,
        }),
    }
}
