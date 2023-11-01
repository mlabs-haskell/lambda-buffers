//! Proptest strategies for most common types
//!
//! These strategies always return valid values.

use num_bigint::{BigInt, Sign};
use proptest::arbitrary::{any, StrategyFor};
use proptest::char::CharStrategy;
use proptest::collection::vec;
use proptest::collection::{btree_map, btree_set};
use proptest::option;
use proptest::prelude::{prop_oneof, Just};
use proptest::result::maybe_err;
use proptest::strategy::Strategy;
use std::collections::{BTreeMap, BTreeSet};

/// Strategy to generate an arbitrary boolean
pub fn arb_bool() -> StrategyFor<bool> {
    any::<bool>()
}

/// Strategy to generate an arbitrary `Sign`
/// Only used internally, to generate `BigInt`s
fn arb_sign() -> impl Strategy<Value = Sign> {
    // NoSign is only used for 0 values so we're not generating it here
    prop_oneof![Just(Sign::Minus), Just(Sign::Plus)]
}

/// Strategy to generate an arbitrary BigInt
pub fn arb_integer() -> impl Strategy<Value = BigInt> {
    // Generating 5 vectors of with random u32 values, which gives a max bound of u32::MAX ^ 5
    (arb_sign(), vec(any::<u32>(), 5)).prop_map(|(sign, value)| {
        // As NoSign is only used for 0 values, we switch to NoSign when an empty vector is generated
        BigInt::new(if value.is_empty() { Sign::NoSign } else { sign }, value)
    })
}

/// Strategy to generate an arbitrary character
pub fn arb_char<'a>() -> CharStrategy<'a> {
    any::<char>()
}

/// Strategy to generate an arbitrary bytestring
pub fn arb_bytes() -> StrategyFor<Vec<u8>> {
    any::<Vec<u8>>()
}

/// Strategy to generate an arbitrary string
pub fn arb_text() -> StrategyFor<String> {
    any::<String>()
}

/// Strategy to generate a complicated data structure
pub fn arb_complicated(
) -> impl Strategy<Value = BTreeMap<String, Result<BTreeSet<char>, Option<Result<Vec<u8>, bool>>>>>
{
    btree_map(
        arb_text(),
        maybe_err(
            btree_set(arb_char(), 20),
            option::of(maybe_err(arb_bytes(), arb_bool())),
        ),
        20,
    )
}
