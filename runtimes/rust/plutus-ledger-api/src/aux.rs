use std::{
    collections::BTreeMap,
    iter::{empty, once},
    str::FromStr,
};

use nom::{
    branch::alt,
    character::complete::{char, digit1},
    combinator::{map_res, opt, recognize},
    error::VerboseError,
    multi::many1,
    sequence::tuple,
    IResult,
};
use num_bigint::BigInt;

use crate::error::ConversionError;

/// Create a container C from one element.
pub fn singleton<T, C>(value: T) -> C
where
    C: FromIterator<T>,
{
    once(value).collect()
}

/// Create an empty container.
pub fn none<T, C>() -> C
where
    C: FromIterator<T>,
{
    empty::<T>().collect()
}

/// Union two BTreeMaps, call f to resolve conflicts if duplicate keys are encountered.
pub fn union_btree_maps_with<K: Clone + Ord, V: Clone, F: Fn(V, V) -> V>(
    f: F,
    l: BTreeMap<K, V>,
    r: BTreeMap<K, V>,
) -> BTreeMap<K, V> {
    r.into_iter().fold(l.clone(), |mut acc, (k, vr)| {
        let v = if let Some((_, vl)) = acc.remove_entry(&k) {
            f(vl, vr)
        } else {
            vr
        };
        acc.insert(k, v);
        acc
    })
}

pub fn union_b_tree_maps_with<const N: usize, K: Clone + Ord, V: Clone, F: Fn(&V, &V) -> V>(
    f: F,
    maps: [&BTreeMap<K, V>; N],
) -> BTreeMap<K, V> {
    maps.into_iter().fold(BTreeMap::new(), |acc, m| {
        m.iter().fold(acc, |mut acc, (k, v)| {
            acc.entry(k.clone())
                .and_modify(|va: &mut V| *va = f(va, v))
                .or_insert(v.clone());

            acc
        })
    })
}

/// Verify that a given bytestring has the expected length
pub(crate) fn guard_bytes(
    ctx: &str,
    bytes: Vec<u8>,
    expected: usize,
) -> Result<Vec<u8>, ConversionError> {
    if bytes.len() == expected {
        Ok(bytes)
    } else {
        Err(ConversionError::invalid_bytestring_length(
            ctx, expected, "equal to", &bytes,
        ))
    }
}

/// Nom parser for BigInt
/// Expects an arbitrary length decimal integer, optionally signed
pub(crate) fn big_int(i: &str) -> IResult<&str, BigInt, VerboseError<&str>> {
    map_res(
        recognize(tuple((opt(alt((char('-'), char('+')))), many1(digit1)))),
        |s: &str| BigInt::from_str(s),
    )(i)
}
