//! Types related to Cardano values, such as Ada and native tokens.

use std::str::FromStr;
use std::string::String;
use std::{
    collections::BTreeMap,
    iter::Sum,
    ops::{Add, Mul, Neg, Not, Sub},
};
use std::{fmt, ops};

use anyhow::anyhow;
use cardano_serialization_lib as csl;
#[cfg(feature = "lbf")]
use lbr_prelude::json::{Error, Json, JsonType};
use nom::combinator::{map, opt};
use nom::{
    branch::alt,
    character::complete::{char, space0},
    combinator::{all_consuming, eof, map_res, success},
    error::{context, VerboseError},
    multi::separated_list0,
    sequence::preceded,
    sequence::tuple,
    Finish, IResult,
};
use num_bigint::BigInt;
use num_traits::Zero;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
#[cfg(feature = "lbf")]
use serde_json;

use crate as plutus_ledger_api;
use crate::aux::{big_int, singleton, union_b_tree_maps_with, union_btree_maps_with};
use crate::csl::csl_to_pla::FromCSL;
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};
use crate::error::ConversionError;
use crate::plutus_data::{IsPlutusData, PlutusData, PlutusDataError};
use crate::v1::crypto::LedgerBytes;
use crate::v1::script::{MintingPolicyHash, ScriptHash};

use super::crypto::ledger_bytes;

////////////////////
// CurrencySymbol //
////////////////////

/// Identifier of a currency, which could be either Ada (or tAda), or a native token represented by
/// it's minting policy hash. A currency may be associated with multiple `AssetClass`es.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CurrencySymbol {
    Ada,
    NativeToken(MintingPolicyHash),
}

impl CurrencySymbol {
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        if bytes.is_empty() {
            Ok(CurrencySymbol::Ada)
        } else {
            Ok(CurrencySymbol::NativeToken(MintingPolicyHash::from_bytes(
                bytes,
            )?))
        }
    }

    pub fn is_ada(&self) -> bool {
        match self {
            CurrencySymbol::Ada => true,
            CurrencySymbol::NativeToken(_) => false,
        }
    }
}

/// Serialize into hexadecimal string, or empty string if Ada
/// It returns `lovelace` instead of the empty string when the alternate flag is used (e.g.: format!("{:#}", cs))
impl fmt::Display for CurrencySymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CurrencySymbol::Ada => {
                if f.alternate() {
                    write!(f, "lovelace")
                } else {
                    write!(f, "")
                }
            }
            CurrencySymbol::NativeToken(symbol) => write!(f, "{}", symbol.0 .0),
        }
    }
}

impl FromStr for CurrencySymbol {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(currency_symbol)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing CurrencySymbol '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

impl IsPlutusData for CurrencySymbol {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            CurrencySymbol::NativeToken(policy_hash) => policy_hash.to_plutus_data(),
            CurrencySymbol::Ada => PlutusData::Bytes(Vec::new()),
        }
    }

    fn from_plutus_data(data: &PlutusData) -> Result<Self, PlutusDataError> {
        IsPlutusData::from_plutus_data(data).map(|bytes: LedgerBytes| {
            if bytes.0.is_empty() {
                CurrencySymbol::Ada
            } else {
                CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(bytes)))
            }
        })
    }
}

#[cfg(feature = "lbf")]
impl Json for CurrencySymbol {
    fn to_json(&self) -> serde_json::Value {
        match self {
            CurrencySymbol::Ada => serde_json::Value::String(String::new()),
            CurrencySymbol::NativeToken(policy_hash) => policy_hash.to_json(),
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        match value.clone() {
            serde_json::Value::String(str) => {
                if str.is_empty() {
                    Ok(CurrencySymbol::Ada)
                } else {
                    Ok(CurrencySymbol::NativeToken(Json::from_json(value)?))
                }
            }
            _ => Err(Error::UnexpectedJsonType {
                wanted: JsonType::String,
                got: JsonType::from(value),
                parser: "Plutus.V1.CurrencySymbol".to_owned(),
            }),
        }
    }
}

/// Nom parser for CurrencySymbol
/// Expects a hexadecimal string representation of 0 (Ada) or 28 bytes (NativeToken)
pub(crate) fn currency_symbol(input: &str) -> IResult<&str, CurrencySymbol, VerboseError<&str>> {
    context(
        "currency symbol",
        map_res(ledger_bytes, |LedgerBytes(bytes)| {
            CurrencySymbol::from_bytes(bytes)
        }),
    )(input)
}

///////////
// Value //
///////////

/// A value that can contain multiple asset classes
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Value(pub BTreeMap<CurrencySymbol, BTreeMap<TokenName, BigInt>>);

#[cfg(feature = "serde")]
mod value_serde {
    use std::collections::BTreeMap;

    use num_bigint::BigInt;
    use serde::{de::Deserializer, ser::Serializer, Deserialize, Serialize};

    use super::{CurrencySymbol, TokenName, Value};

    struct Assets(BTreeMap<TokenName, BigInt>);

    impl Serialize for Value {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.collect_seq(
                self.0
                    .iter()
                    .map(|(cur_sym, assets)| (cur_sym, Assets(assets.to_owned()))),
            )
        }
    }

    impl<'de> Deserialize<'de> for Value {
        fn deserialize<D>(deserializer: D) -> Result<Value, D::Error>
        where
            D: Deserializer<'de>,
        {
            let vec: Vec<(CurrencySymbol, Assets)> = Vec::deserialize(deserializer)?;

            Ok(Value(
                vec.into_iter().map(|(cs, assets)| (cs, assets.0)).collect(),
            ))
        }
    }

    impl Serialize for Assets {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.collect_seq(self.0.iter())
        }
    }

    impl<'de> Deserialize<'de> for Assets {
        fn deserialize<D>(deserializer: D) -> Result<Assets, D::Error>
        where
            D: Deserializer<'de>,
        {
            let vec: Vec<(TokenName, BigInt)> = Vec::deserialize(deserializer)?;

            Ok(Assets(vec.into_iter().collect()))
        }
    }
}

impl Value {
    pub fn new() -> Self {
        Value(BTreeMap::new())
    }
    /// Create a Value containing only ada tokens, given the quantity in lovelace.
    pub fn ada_value(amount: &BigInt) -> Self {
        Self::token_value(&CurrencySymbol::Ada, &TokenName::ada(), amount)
    }

    /// Create a Value containing only the given quantity of the given token.
    pub fn token_value(cs: &CurrencySymbol, tn: &TokenName, amount: &BigInt) -> Self {
        Value(singleton((
            cs.clone(),
            singleton((tn.clone(), amount.clone())),
        )))
    }

    /// Lookup the quantity of the given token.
    pub fn get_token_amount(&self, cs: &CurrencySymbol, tn: &TokenName) -> BigInt {
        self.0
            .get(cs)
            .and_then(|tn_map| tn_map.get(tn))
            .map_or(BigInt::zero(), Clone::clone)
    }

    /// Lookup the quantity of ada(unit: lovelace).
    pub fn get_ada_amount(&self) -> BigInt {
        self.get_token_amount(&CurrencySymbol::Ada, &TokenName::ada())
    }

    /// Insert ada into a value by inserting or replacing old value
    pub fn insert_ada_mut(&mut self, amount: BigInt) {
        self.0.insert(
            CurrencySymbol::Ada,
            BTreeMap::from([(TokenName::ada(), amount)]),
        );
    }

    /// Create a new value by inserting a new token or replacing the existing quantity.
    pub fn insert_token(
        &self,
        currency_symbol: &CurrencySymbol,
        token_name: &TokenName,
        amount: &BigInt,
    ) -> Self {
        let mut result_map = self.clone();

        result_map.insert_token_mut(currency_symbol.clone(), token_name.clone(), amount.clone());

        result_map
    }
    /// Insert a new token into the value, or replace the existing quantity.
    pub fn insert_token_mut(
        &mut self,
        currency_symbol: CurrencySymbol,
        token_name: TokenName,
        amount: BigInt,
    ) {
        let tn_map = self.0.get_mut(&currency_symbol);

        match tn_map {
            None => {
                self.0
                    .insert(currency_symbol, singleton((token_name, amount)));
            }
            Some(tn_map) => {
                let val = tn_map.get_mut(&token_name);
                match val {
                    None => {
                        tn_map.insert(token_name, amount);
                    }
                    Some(old_amount) => *old_amount = amount,
                }
            }
        }
    }

    /// Return true if the value don't have any entries.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Remove all tokens whose quantity is zero.
    pub fn normalize(self) -> Self {
        self.filter(|_, _, a| a.is_zero().not())
    }

    pub fn is_subset(&self, b: &Value) -> bool {
        (b - self)
            .normalize()
            // Has negative entries?
            .filter(|_, _, amount| amount < &BigInt::from(0u32))
            .is_empty()
    }

    pub fn is_pure_ada(&self) -> bool {
        self.0.iter().all(|(cs, _)| cs == &CurrencySymbol::Ada)
    }

    /// Apply a function to each token of the value, and use its result as the new amount.
    pub fn map_amount<F>(self, mut f: F) -> Self
    where
        F: FnMut(&CurrencySymbol, &TokenName, &BigInt) -> BigInt,
    {
        self.filter_map_amount(|cs, tn, a| Some(f(cs, tn, a)))
    }

    /// Apply a predicate to tokens.
    pub fn filter<F>(self, mut f: F) -> Self
    where
        F: FnMut(&CurrencySymbol, &TokenName, &BigInt) -> bool,
    {
        self.filter_map_amount(|cs, tn, a| f(cs, tn, a).then(|| a.clone()))
    }

    /// Apply a function to each token of the value. If the result is None, the token entry will be
    /// removed.
    ///
    /// Note that if the name-quantity map of any given currency symbols is empty, the currency entry
    /// will be removed from the top-level map entirely.
    pub fn filter_map_amount<F>(self, mut f: F) -> Self
    where
        F: FnMut(&CurrencySymbol, &TokenName, &BigInt) -> Option<BigInt>,
    {
        Value(
            (self.0)
                .into_iter()
                .filter_map(|(cs, tn_map)| {
                    let filtered_tn_map = tn_map
                        .into_iter()
                        .filter_map(|(tn, a)| f(&cs, &tn, &a).map(|a| (tn, a)))
                        .collect::<BTreeMap<TokenName, BigInt>>();

                    if filtered_tn_map.is_empty() {
                        None
                    } else {
                        Some((cs.clone(), filtered_tn_map))
                    }
                })
                .collect(),
        )
    }

    /// Create a vector with each distinct value
    /// Warning: is the value is not normalized, the same asset class can appear twice
    pub fn flatten(&self) -> Vec<(&CurrencySymbol, &TokenName, &BigInt)> {
        self.0
            .iter()
            .flat_map(|(currency_symbol, assets)| {
                assets
                    .iter()
                    .map(move |(token_name, amount)| (currency_symbol, token_name, amount))
            })
            .collect()
    }

    pub fn unflatten(list: &[(CurrencySymbol, TokenName, BigInt)]) -> Self {
        list.iter()
            .fold(Value::new(), |v, (cs, tn, am)| v.insert_token(cs, tn, am))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self
            .0
            .iter()
            .flat_map(|(currency_symbol, assets)| {
                assets
                    .iter()
                    .map(move |(token_name, amount)| (currency_symbol, token_name, amount))
            })
            .peekable();
        while let Some((cur_sym, tn, amount)) = it.next() {
            if cur_sym.is_ada() {
                amount.fmt(f)?;
            } else if tn.is_empty() {
                amount.fmt(f)?;
                " ".fmt(f)?;
                cur_sym.fmt(f)?;
            } else {
                amount.fmt(f)?;
                " ".fmt(f)?;
                cur_sym.fmt(f)?;
                ".".fmt(f)?;
                tn.fmt(f)?;
            }
            if it.peek().is_some() {
                "+".fmt(f)?;
            }
        }

        Ok(())
    }
}

/// Nom parser for a single entry in a Value
/// Expects an integer quantity, followed by an asset class after a space character
/// (space is not required for Ada)
/// E.g.: 12 11223344556677889900112233445566778899001122334455667788.001122aabbcc
pub(crate) fn flat_value(
    input: &str,
) -> IResult<&str, (CurrencySymbol, TokenName, BigInt), VerboseError<&str>> {
    map(
        tuple((big_int, opt(preceded(char(' '), asset_class)))),
        |(amount, asset_class)| match asset_class {
            None => (CurrencySymbol::Ada, TokenName::ada(), amount),
            Some(AssetClass {
                currency_symbol,
                token_name,
            }) => (currency_symbol, token_name, amount),
        },
    )(input)
}

/// Nom parser for Value
/// Expects flat Value entries divided by a `+` sign
/// E.g.: 123+12 11223344556677889900112233445566778899001122334455667788.001122aabbcc
pub(crate) fn value(input: &str) -> IResult<&str, Value, VerboseError<&str>> {
    map(
        separated_list0(tuple((space0, char('+'))), flat_value),
        |flat_values| Value::unflatten(&flat_values),
    )(input)
}

impl FromStr for Value {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(value)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!("Error while parsing Value '{}': {}", s, err))
            })
            .map(|(_, cs)| cs)
    }
}

impl Zero for Value {
    fn zero() -> Self {
        Default::default()
    }

    fn is_zero(&self) -> bool {
        self.is_empty()
    }
}

impl_op!(+ |a: &Value, b: &Value| -> Value { a.clone() + b.clone() });
impl_op!(+ |a: &Value, b: Value| -> Value { a.clone() + b });
impl_op!(+ |a: Value, b: &Value| -> Value { a + b.clone() });

impl Add<Value> for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> Self::Output {
        Value(union_btree_maps_with(
            |lhs, rhs| union_btree_maps_with(|lhs, rhs| lhs + rhs, lhs, rhs),
            self.0,
            rhs.0,
        ))
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        self.map_amount(|_, _, a| a.neg())
    }
}

impl Neg for &Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        self.clone().neg()
    }
}

impl_op!(-|a: &Value, b: &Value| -> Value { a.clone() - b.clone() });
impl_op!(-|a: &Value, b: Value| -> Value { a.clone() - b });
impl_op!(-|a: Value, b: &Value| -> Value { a - b.clone() });

impl Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> Self::Output {
        self.add(rhs.neg())
    }
}

impl_op!(*|a: &BigInt, b: &Value| -> Value { b * a });
impl_op_commutative!(*|a: Value, b: BigInt| -> Value { &a * &b });
impl_op_commutative!(*|a: &Value, b: BigInt| -> Value { a * &b });
impl_op_commutative!(*|a: Value, b: &BigInt| -> Value { &a * b });

impl_op_commutative!(*|a: &Value, b: i8| -> Value { a * BigInt::from(b) });
impl_op_commutative!(*|a: &Value, b: i16| -> Value { a * BigInt::from(b) });
impl_op_commutative!(*|a: &Value, b: i32| -> Value { a * BigInt::from(b) });
impl_op_commutative!(*|a: &Value, b: i64| -> Value { a * BigInt::from(b) });

impl_op_commutative!(*|a: &Value, b: u8| -> Value { a * BigInt::from(b) });
impl_op_commutative!(*|a: &Value, b: u16| -> Value { a * BigInt::from(b) });
impl_op_commutative!(*|a: &Value, b: u32| -> Value { a * BigInt::from(b) });
impl_op_commutative!(*|a: &Value, b: u64| -> Value { a * BigInt::from(b) });

impl Mul<&BigInt> for &Value {
    type Output = Value;

    fn mul(self, rhs: &BigInt) -> Self::Output {
        Value(
            self.0
                .iter()
                .map(|(cs, tn_map)| {
                    (
                        cs.clone(),
                        tn_map.iter().map(|(tn, q)| (tn.clone(), q * rhs)).collect(),
                    )
                })
                .collect(),
        )
    }
}

impl Sum<Value> for Value {
    fn sum<I: Iterator<Item = Value>>(iter: I) -> Self {
        iter.fold(Zero::zero(), Add::add)
    }
}

impl<'a> Sum<&'a Value> for Value {
    fn sum<I: Iterator<Item = &'a Value>>(iter: I) -> Self {
        iter.fold(Zero::zero(), Add::add)
    }
}

impl IsPlutusData for Value {
    fn to_plutus_data(&self) -> PlutusData {
        self.0.to_plutus_data()
    }

    fn from_plutus_data(data: &PlutusData) -> Result<Self, PlutusDataError> {
        IsPlutusData::from_plutus_data(data).map(Self)
    }
}

impl FromCSL<csl::Assets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(value: &csl::Assets) -> Self {
        let keys = value.keys();
        (0..keys.len()).fold(BTreeMap::new(), |mut acc, idx| {
            let asset_name = keys.get(idx);
            if let Some(quantity) = value.get(&asset_name) {
                acc.insert(
                    TokenName::from_csl(&asset_name),
                    BigInt::from_csl(&quantity),
                );
            }
            acc
        })
    }
}

impl TryFromPLA<BTreeMap<TokenName, BigInt>> for csl::Assets {
    fn try_from_pla(val: &BTreeMap<TokenName, BigInt>) -> Result<Self, TryFromPLAError> {
        val.iter().try_fold(csl::Assets::new(), |mut acc, (k, v)| {
            acc.insert(&k.try_to_csl()?, &v.try_to_csl()?);
            Ok(acc)
        })
    }
}

impl FromCSL<csl::MultiAsset> for Value {
    fn from_csl(value: &csl::MultiAsset) -> Self {
        let keys = value.keys();
        Value((0..keys.len()).fold(BTreeMap::new(), |mut acc, idx| {
            let script_hash = keys.get(idx);
            if let Some(assets) = value.get(&script_hash) {
                let assets = BTreeMap::from_csl(&assets);
                acc.insert(
                    CurrencySymbol::NativeToken(MintingPolicyHash::from_csl(&script_hash)),
                    assets,
                );
            }
            acc
        }))
    }
}

impl FromCSL<csl::Value> for Value {
    fn from_csl(value: &csl::Value) -> Self {
        let lovelaces = BigInt::from_csl(&value.coin());
        let mut pla_value = Value::ada_value(&lovelaces);
        if let Some(multi_asset) = value.multiasset() {
            pla_value = &pla_value + &Value::from_csl(&multi_asset)
        }
        pla_value
    }
}

impl TryFromPLA<Value> for csl::Value {
    fn try_from_pla(val: &Value) -> Result<Self, TryFromPLAError> {
        let coin: csl::Coin = val
            .0
            .get(&CurrencySymbol::Ada)
            .and_then(|m| m.get(&TokenName::ada()))
            .map_or(Ok(csl::BigNum::zero()), TryToCSL::try_to_csl)?;

        let m_ass = val
            .0
            .iter()
            .filter_map(|(cs, tn_map)| match &cs {
                CurrencySymbol::Ada => None,
                CurrencySymbol::NativeToken(h) => Some((h, tn_map)),
            })
            .try_fold(csl::MultiAsset::new(), |mut acc, (cs, ass)| {
                acc.insert(&cs.try_to_csl()?, &ass.try_to_csl()?);
                Ok(acc)
            })?;

        let mut v = csl::Value::new(&coin);

        v.set_multiasset(&m_ass);

        Ok(v)
    }
}

impl FromCSL<csl::MintAssets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(m_ass: &csl::MintAssets) -> Self {
        let keys = m_ass.keys();
        (0..keys.len())
            .map(|idx| {
                let key = keys.get(idx);
                let value = m_ass.get(&key).unwrap();
                (TokenName::from_csl(&key), BigInt::from_csl(&value))
            })
            .collect()
    }
}

impl FromCSL<csl::MintsAssets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(value: &csl::MintsAssets) -> Self {
        (0..value.len())
            .map(|idx| value.get(idx).unwrap())
            .fold(BTreeMap::new(), |acc, m| {
                let ass = BTreeMap::from_csl(&m);
                union_b_tree_maps_with(|l, r| l + r, [&acc, &ass])
            })
    }
}

impl TryFromPLA<BTreeMap<TokenName, BigInt>> for csl::MintAssets {
    fn try_from_pla(val: &BTreeMap<TokenName, BigInt>) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::MintAssets::new(), |mut acc, (k, v)| {
                acc.insert(&k.try_to_csl()?, &v.try_to_csl()?)
                    .map_err(TryFromPLAError::CSLJsError)?;
                Ok(acc)
            })
    }
}

impl FromCSL<csl::Mint> for Value {
    fn from_csl(mint: &csl::Mint) -> Self {
        let keys = mint.keys();
        Value(
            (0..keys.len())
                .map(|idx| {
                    let sh = keys.get(idx);
                    let ass = mint.get(&sh).unwrap_or(csl::MintsAssets::new());
                    (
                        CurrencySymbol::NativeToken(MintingPolicyHash::from_csl(&sh)),
                        BTreeMap::from_csl(&ass),
                    )
                })
                .collect::<BTreeMap<CurrencySymbol, BTreeMap<TokenName, BigInt>>>(),
        )
    }
}

///////////////
// TokenName //
///////////////

/// Name of a token. This can be any arbitrary bytearray
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TokenName(pub LedgerBytes);

impl TokenName {
    /// Ada tokenname (empty bytestring)
    pub fn ada() -> TokenName {
        TokenName(LedgerBytes(Vec::new()))
    }

    pub fn is_empty(&self) -> bool {
        self.0 .0.is_empty()
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        if bytes.len() <= 32 {
            Ok(TokenName(LedgerBytes(bytes)))
        } else {
            Err(ConversionError::invalid_bytestring_length(
                "TokenName",
                32,
                "less than or equal to",
                &bytes,
            ))
        }
    }

    /// Convert a UTF8 string into a TokenName (use from_str to convert from a hexadecimal string)
    pub fn from_string(str: &str) -> Result<Self, ConversionError> {
        TokenName::from_bytes(String::from(str).into_bytes())
    }

    /// Convert TokenName to string if it is a valid UTF8 bytestring
    pub fn try_into_string(self) -> Result<String, std::string::FromUtf8Error> {
        String::from_utf8(self.0 .0)
    }
}

impl FromStr for TokenName {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(token_name)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing TokenName '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

/// Nom parser for TokenName
/// Expects a hexadecimal string representation of up to 32
pub(crate) fn token_name(input: &str) -> IResult<&str, TokenName, VerboseError<&str>> {
    map_res(ledger_bytes, |LedgerBytes(bytes)| {
        TokenName::from_bytes(bytes)
    })(input)
}

impl IsPlutusData for TokenName {
    fn to_plutus_data(&self) -> PlutusData {
        self.0.to_plutus_data()
    }

    fn from_plutus_data(data: &PlutusData) -> Result<Self, PlutusDataError> {
        IsPlutusData::from_plutus_data(data).map(Self)
    }
}

/// Serialize into a hexadecimal string
/// It tries to decode the token name from UTF8 when the alternate flag is used (e.g.: format!("{:#}", ac)),
/// if failsed it prepends the hex value with `0x`
impl fmt::Display for TokenName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let utf8_str = std::str::from_utf8(&self.0 .0);

            match utf8_str {
                Ok(str) => write!(f, "{}", str),
                Err(_) => write!(f, "0x{}", self.0),
            }
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl FromCSL<csl::AssetName> for TokenName {
    fn from_csl(value: &csl::AssetName) -> Self {
        TokenName(LedgerBytes(value.name()))
    }
}

impl TryFromPLA<TokenName> for csl::AssetName {
    fn try_from_pla(val: &TokenName) -> Result<Self, TryFromPLAError> {
        csl::AssetName::new(val.0 .0.to_owned()).map_err(TryFromPLAError::CSLJsError)
    }
}

////////////////
// AssetClass //
////////////////

/// AssetClass is uniquely identifying a specific asset
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct AssetClass {
    pub currency_symbol: CurrencySymbol,
    pub token_name: TokenName,
}

/// Serialize into two hexadecimal strings divided by a . (e.g. aabbcc.001122)
/// It tries to decode the token name from UTF8 when the alternate flag is used (e.g.: format!("{:#}", ac)),
/// if failsed it prepends the hex value with `0x`
impl fmt::Display for AssetClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.token_name.is_empty() {
            self.currency_symbol.fmt(f)
        } else {
            self.currency_symbol.fmt(f)?;
            ".".fmt(f)?;
            self.token_name.fmt(f)
        }
    }
}

impl FromStr for AssetClass {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(asset_class)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing AssetClass '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

/// Nom parser for AssetClass
/// Expects a currency symbol and token name both in hexadecimal format, divided by a `.`
/// In case the token name is empty, the divider is not required
/// E.g.:
///   - 11223344556677889900112233445566778899001122334455667788.001122aabbcc
///   - 11223344556677889900112233445566778899001122334455667788
pub(crate) fn asset_class(input: &str) -> IResult<&str, AssetClass, VerboseError<&str>> {
    let (input, cs) = currency_symbol(input)?;

    let (input, tn) = alt((
        preceded(eof, success(TokenName::ada())),
        preceded(char('.'), token_name),
    ))(input)?;

    Ok((
        input,
        AssetClass {
            currency_symbol: cs,
            token_name: tn,
        },
    ))
}

////////////////
// Lovelace //
////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Lovelace(pub BigInt);
