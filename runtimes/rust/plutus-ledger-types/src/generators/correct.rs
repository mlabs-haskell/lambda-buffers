use crate::address::{
    Address, CertificateIndex, ChainPointer, Credential, StakingCredential, TransactionIndex,
};
use crate::crypto::{Ed25519PubKeyHash, LedgerBytes};
use crate::datum::{Datum, DatumHash, OutputDatum};
use crate::feature_traits::FeatureTraits;
use crate::interval::{Extended, LowerBound, PlutusInterval, UpperBound};
use crate::ledger_state::Slot;
use crate::plutus_data::PlutusData;
use crate::redeemer::Redeemer;
use crate::script::{MintingPolicyHash, ScriptHash, ValidatorHash};
use crate::transaction::{
    POSIXTime, TransactionHash, TransactionInput, TransactionOutput, TxInInfo,
};
use crate::value::{ada_token_name, AssetClass, CurrencySymbol, TokenName, Value};
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

/// Strategy to generate an arbitrary bytestring with a fixed length
pub fn arb_ledger_bytes(length: usize) -> impl Strategy<Value = LedgerBytes> {
    (vec(any::<u8>(), length)).prop_map(LedgerBytes)
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

/// Strategy to generate an asset class
///
/// This generator will only generate valid asset classes, the Ada token name will always be empty
pub fn arb_asset_class() -> impl Strategy<Value = AssetClass> {
    (arb_currency_symbol(), arb_token_name()).prop_map(|(currency_symbol, token_name)| {
        let token_name = match currency_symbol {
            CurrencySymbol::Ada => ada_token_name(),
            CurrencySymbol::NativeToken(_) => token_name,
        };
        AssetClass {
            currency_symbol,
            token_name,
        }
    })
}

/// Strategy to generate a currency symbol
///
/// In order to avoid generating too much Ada symbols, this generator is configured such that it
/// only has 25% chance of getting Ada, and 75% of getting a native token
pub fn arb_currency_symbol() -> impl Strategy<Value = CurrencySymbol> {
    prop_oneof![
        1 =>Just(CurrencySymbol::Ada),
        3 =>arb_minting_policy_hash().prop_map(CurrencySymbol::NativeToken)
    ]
}

/// Strategy to generate a token name
pub fn arb_token_name() -> impl Strategy<Value = TokenName> {
    arb_ledger_bytes(32).prop_map(TokenName)
}

/// Strategy to generate a minting policy hash
pub fn arb_minting_policy_hash() -> impl Strategy<Value = MintingPolicyHash> {
    arb_script_hash().prop_map(MintingPolicyHash)
}

/// Strategy to generate a validator hash
pub fn arb_validator_hash() -> impl Strategy<Value = ValidatorHash> {
    arb_script_hash().prop_map(ValidatorHash)
}

/// Strategy to generate a ScriptHash
pub fn arb_script_hash() -> impl Strategy<Value = ScriptHash> {
    arb_ledger_bytes(28).prop_map(ScriptHash)
}

/// Strategy to generate a Value
///
/// This generator will try to balance the result, such that there's a 50% chance that Ada is
/// included in the Value
pub fn arb_value() -> impl Strategy<Value = Value> {
    prop_oneof![
        arb_native_tokens(),
        (arb_native_tokens(), arb_integer()).prop_map(|(Value(outer_dict), amount)| {
            let mut outer_dict = outer_dict.clone();
            let inner_dict = BTreeMap::from([(ada_token_name(), amount)]);
            outer_dict.insert(CurrencySymbol::Ada, inner_dict);
            Value(outer_dict)
        })
    ]
}

/// Strategy to generate a Value
pub fn arb_native_tokens() -> impl Strategy<Value = Value> {
    btree_map(
        arb_minting_policy_hash().prop_map(CurrencySymbol::NativeToken),
        btree_map(arb_token_name(), arb_integer(), 5),
        5,
    )
    .prop_map(Value)
}

/// Strategy to generate an arbitrary PlutusData with a maximum depth of 5 recursions
pub fn arb_plutus_data() -> impl Strategy<Value = PlutusData> {
    arb_plutus_data_leaf().prop_recursive(5, 64, 16, |arb_data| {
        prop_oneof![
            arb_integer().prop_map(PlutusData::Integer),
            arb_bytes().prop_map(PlutusData::Bytes),
            vec(arb_data.clone(), 5).prop_map(PlutusData::List),
            vec((arb_data.clone(), arb_data.clone()), 5).prop_map(PlutusData::Map),
            (arb_integer(), vec(arb_data.clone(), 5))
                .prop_map(|(id, fields)| PlutusData::Constr(id, fields)),
        ]
    })
}

/// Leaf generator for PlutusData recursive generator
fn arb_plutus_data_leaf() -> impl Strategy<Value = PlutusData> {
    prop_oneof![
        arb_integer().prop_map(PlutusData::Integer),
        arb_bytes().prop_map(PlutusData::Bytes),
    ]
}

/// Strategy to generate Ed25519 public key hash
pub fn arb_ed25519_pub_key_hash() -> impl Strategy<Value = Ed25519PubKeyHash> {
    arb_ledger_bytes(28).prop_map(Ed25519PubKeyHash)
}

/// Strategy to generate a Datum hash
pub fn arb_datum_hash() -> impl Strategy<Value = DatumHash> {
    arb_ledger_bytes(32).prop_map(DatumHash)
}

/// Strategy to generate a Datum
pub fn arb_datum() -> impl Strategy<Value = Datum> {
    arb_plutus_data().prop_map(Datum)
}

/// Strategy to generate a Redeemer
pub fn arb_redeemer() -> impl Strategy<Value = Redeemer> {
    arb_plutus_data().prop_map(Redeemer)
}

/// Strategy to generate an Extended set
pub fn arb_extended<T>(element: T) -> impl Strategy<Value = Extended<T::Value>>
where
    T: Strategy,
    T::Value: FeatureTraits + Clone,
{
    prop_oneof![
        Just(Extended::NegInf),
        Just(Extended::PosInf),
        element.prop_map(Extended::Finite)
    ]
}

/// Strategy to generate an extended POSIX time
pub fn arb_extended_posix_time() -> impl Strategy<Value = Extended<POSIXTime>> {
    arb_extended(arb_posix_time())
}

/// Strategy to generate a POSIX Time
pub fn arb_posix_time() -> impl Strategy<Value = POSIXTime> {
    (0..1000000).prop_map(|int| POSIXTime(BigInt::from(int)))
}

/// Strategy to generate an UpperBound
pub fn arb_upper_bound<T>(element: T) -> impl Strategy<Value = UpperBound<T::Value>>
where
    T: Strategy,
    T::Value: FeatureTraits + Clone,
{
    (arb_extended(element), arb_bool()).prop_map(|(bound, closed)| UpperBound { bound, closed })
}

/// Strategy to generate a LowerBound
pub fn arb_lower_bound<T>(element: T) -> impl Strategy<Value = LowerBound<T::Value>>
where
    T: Strategy,
    T::Value: FeatureTraits + Clone,
{
    (arb_extended(element), arb_bool()).prop_map(|(bound, closed)| LowerBound { bound, closed })
}

/// Strategy to generate a PlutusInterval
///
/// This implementation is not normalized, so impossible values might be generated
pub fn arb_plutus_interval<T>(
    lower_bound: T,
    upper_bound: T,
) -> impl Strategy<Value = PlutusInterval<T::Value>>
where
    T: Strategy,
    T::Value: FeatureTraits + Clone,
{
    (arb_lower_bound(lower_bound), arb_upper_bound(upper_bound))
        .prop_map(|(from, to)| PlutusInterval { from, to })
}

/// Strategy to generate a PlutusInterval
///
/// This implementation is not normalized, so impossible values might be generated
pub fn arb_plutus_interval_posix_time() -> impl Strategy<Value = PlutusInterval<POSIXTime>> {
    arb_plutus_interval(arb_posix_time(), arb_posix_time())
}

/// Strategy to generate a Cardano address
pub fn arb_address() -> impl Strategy<Value = Address> {
    (arb_credential(), option::of(arb_staking_credential())).prop_map(
        |(credential, staking_credential)| Address {
            credential,
            staking_credential,
        },
    )
}

/// Strategy to generate a chain pointer
pub fn arb_chain_pointer() -> impl Strategy<Value = ChainPointer> {
    (arb_slot(), arb_transaction_index(), arb_certificate_index()).prop_map(
        |(slot_number, transaction_index, certificate_index)| ChainPointer {
            slot_number,
            transaction_index,
            certificate_index,
        },
    )
}

/// Strategy to generate a slot number
pub fn arb_slot() -> impl Strategy<Value = Slot> {
    arb_integer().prop_map(Slot)
}

/// Strategy to generate a transaction index
pub fn arb_transaction_index() -> impl Strategy<Value = TransactionIndex> {
    arb_integer().prop_map(TransactionIndex)
}

/// Strategy to generate a certificate index
pub fn arb_certificate_index() -> impl Strategy<Value = CertificateIndex> {
    arb_integer().prop_map(CertificateIndex)
}

/// Strategy to generate a staking credential
pub fn arb_staking_credential() -> impl Strategy<Value = StakingCredential> {
    prop_oneof![
        arb_credential().prop_map(StakingCredential::Hash),
        arb_chain_pointer().prop_map(StakingCredential::Pointer)
    ]
}

/// Strategy to generate a credential
pub fn arb_credential() -> impl Strategy<Value = Credential> {
    prop_oneof![
        arb_ed25519_pub_key_hash().prop_map(Credential::PubKey),
        arb_validator_hash().prop_map(Credential::Script)
    ]
}

/// Strategy to generate a transaction hash
pub fn arb_transaction_hash() -> impl Strategy<Value = TransactionHash> {
    arb_ledger_bytes(32).prop_map(TransactionHash)
}

/// Strategy to generate a transaction input
pub fn arb_transaction_input() -> impl Strategy<Value = TransactionInput> {
    (arb_transaction_hash(), arb_integer()).prop_map(|(transaction_id, index)| TransactionInput {
        transaction_id,
        index,
    })
}

/// Strategy to generate transaction output
pub fn arb_transaction_output() -> impl Strategy<Value = TransactionOutput> {
    (
        arb_address(),
        arb_value(),
        arb_output_datum(),
        option::of(arb_script_hash()),
    )
        .prop_map(
            |(address, value, datum, reference_script)| TransactionOutput {
                address,
                value,
                datum,
                reference_script,
            },
        )
}

/// Strategy to generate an output datum
pub fn arb_output_datum() -> impl Strategy<Value = OutputDatum> {
    prop_oneof![
        Just(OutputDatum::None),
        arb_datum_hash().prop_map(OutputDatum::DatumHash),
        arb_datum().prop_map(OutputDatum::InlineDatum)
    ]
}

/// Strategy to generate a TxInInfo
pub fn arb_tx_in_info() -> impl Strategy<Value = TxInInfo> {
    (arb_transaction_input(), arb_transaction_output()).prop_map(|(transaction_input, resolved)| {
        TxInInfo {
            transaction_input,
            resolved,
        }
    })
}
