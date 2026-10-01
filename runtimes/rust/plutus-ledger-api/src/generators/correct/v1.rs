//! Proptest strategies for Plutus V1 types
//!
//! These strategies always return valid values.
use crate::feature_traits::FeatureTraits;
use crate::generators::correct::primitive::{arb_bool, arb_bytes, arb_integer, arb_natural};
use crate::plutus_data::PlutusData;
use crate::v1::address::{
    Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential, TransactionIndex,
};
use crate::v1::assoc_map::AssocMap;
use crate::v1::crypto::{Ed25519PubKeyHash, LedgerBytes, PaymentPubKeyHash, StakePubKeyHash};
use crate::v1::datum::{Datum, DatumHash};
use crate::v1::interval::{Extended, Interval, LowerBound, PlutusInterval, UpperBound};
use crate::v1::redeemer::{Redeemer, RedeemerHash};
use crate::v1::script::{MintingPolicyHash, ScriptHash, ValidatorHash};
use crate::v1::transaction::{
    DCert, POSIXTime, ScriptContext, ScriptPurpose, TransactionHash, TransactionInfo,
    TransactionInput, TransactionOutput, TxInInfo,
};
use crate::v1::value::Lovelace;
use crate::v2::value::{AssetClass, CurrencySymbol, TokenName, Value};
use num_bigint::BigInt;
use proptest::collection::btree_map;
use proptest::collection::vec;
use proptest::option;
use proptest::prelude::{any, prop_oneof, Just};
use proptest::strategy::Strategy;
use std::collections::BTreeMap;

/// Strategy to generate an arbitrary bytestring with a fixed length
pub fn arb_ledger_bytes(length: usize) -> impl Strategy<Value = LedgerBytes> {
    (vec(any::<u8>(), length)).prop_map(LedgerBytes)
}

/// Strategy to generate an asset class
///
/// This generator will only generate valid asset classes, the Ada token name will always be empty
pub fn arb_asset_class() -> impl Strategy<Value = AssetClass> {
    (arb_currency_symbol(), arb_token_name()).prop_map(|(currency_symbol, token_name)| {
        let token_name = match currency_symbol {
            CurrencySymbol::Ada => TokenName::ada(),
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
        (arb_native_tokens(), arb_natural(2)).prop_map(|(Value(outer_dict), amount)| {
            let mut outer_dict = outer_dict.clone();
            let inner_dict = BTreeMap::from([(TokenName::ada(), amount)]);
            outer_dict.insert(CurrencySymbol::Ada, inner_dict);
            Value(outer_dict)
        })
    ]
}

/// Strategy to generate a Value
pub fn arb_native_tokens() -> impl Strategy<Value = Value> {
    btree_map(
        arb_minting_policy_hash().prop_map(CurrencySymbol::NativeToken),
        btree_map(arb_token_name(), arb_natural(1), 5),
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
            (arb_natural(1), vec(arb_data.clone(), 5))
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

/// Strategy to generate a Datum hash
pub fn arb_redeemer_hash() -> impl Strategy<Value = RedeemerHash> {
    arb_ledger_bytes(32).prop_map(RedeemerHash)
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
    (0..2000000000).prop_map(|int| POSIXTime(BigInt::from(int)))
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

/// Strategy to generate a Interval
pub fn arb_interval<T>(lower_bound: T, upper_bound: T) -> impl Strategy<Value = Interval<T::Value>>
where
    T: Strategy,
    T::Value: FeatureTraits + Clone,
{
    (lower_bound, upper_bound).prop_flat_map(|(lb, ub)| {
        prop_oneof![
            4 => Just(Interval::Finite(lb.clone(), ub.clone())),
            4 => Just(Interval::StartAt(lb.clone())),
            4 => Just(Interval::StartAfter(lb)),
            4 => Just(Interval::EndAt(ub.clone())),
            4 => Just(Interval::EndBefore(ub)),
            1 => Just(Interval::Always),
            1 => Just(Interval::Never)
        ]
    })
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

/// Strategy to generate a Interval
pub fn arb_interval_posix_time() -> impl Strategy<Value = Interval<POSIXTime>> {
    (arb_posix_time(), arb_posix_time()).prop_flat_map(|(x, y)| {
        if x > y {
            arb_interval(Just(y), Just(x))
        } else {
            arb_interval(Just(x), Just(y))
        }
    })
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
    arb_natural(1).prop_map(Slot)
}

/// Strategy to generate a transaction index
pub fn arb_transaction_index() -> impl Strategy<Value = TransactionIndex> {
    arb_natural(1).prop_map(TransactionIndex)
}

/// Strategy to generate a certificate index.
pub fn arb_certificate_index() -> impl Strategy<Value = CertificateIndex> {
    arb_natural(1).prop_map(CertificateIndex)
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
    (arb_transaction_hash(), arb_natural(1)).prop_map(|(transaction_id, index)| TransactionInput {
        transaction_id,
        index,
    })
}

/// Strategy to generate transaction output
pub fn arb_transaction_output() -> impl Strategy<Value = TransactionOutput> {
    (arb_address(), arb_value(), option::of(arb_datum_hash())).prop_map(
        |(address, value, datum_hash)| TransactionOutput {
            address,
            value,
            datum_hash,
        },
    )
}

/// Strategy to generate a TxInInfo
pub fn arb_tx_in_info() -> impl Strategy<Value = TxInInfo> {
    (arb_transaction_input(), arb_transaction_output())
        .prop_map(|(reference, output)| TxInInfo { reference, output })
}

/// Strategy to generate an AssocMap, given the strategies to generate keys and values
pub fn arb_assoc_map<K: std::fmt::Debug, V: std::fmt::Debug>(
    arb_k: impl Strategy<Value = K>,
    arb_v: impl Strategy<Value = V>,
) -> impl Strategy<Value = AssocMap<K, V>> {
    vec((arb_k, arb_v), 10).prop_map(AssocMap)
}

/// Strategy to generate a PaymentPubKeyHash
pub fn arb_payment_pub_key_hash() -> impl Strategy<Value = PaymentPubKeyHash> {
    arb_ed25519_pub_key_hash().prop_map(PaymentPubKeyHash)
}

/// Strategy to generate a DCert
pub fn arb_d_cert() -> impl Strategy<Value = DCert> {
    prop_oneof![
        arb_staking_credential().prop_map(DCert::DelegRegKey),
        arb_staking_credential().prop_map(DCert::DelegDeRegKey),
        (arb_staking_credential(), arb_payment_pub_key_hash())
            .prop_map(|(sc, pkh)| DCert::DelegDelegate(sc, pkh)),
        (arb_payment_pub_key_hash(), arb_payment_pub_key_hash())
            .prop_map(|(p1, p2)| DCert::PoolRegister(p1, p2)),
        (arb_payment_pub_key_hash(), arb_natural(1)).prop_map(|(pkh, i)| DCert::PoolRetire(pkh, i)),
        Just(DCert::Genesis),
        Just(DCert::Mir)
    ]
}

/// Strategy to generate a ScriptPurpose
pub fn arb_script_purpose() -> impl Strategy<Value = ScriptPurpose> {
    prop_oneof![
        arb_currency_symbol().prop_map(ScriptPurpose::Minting),
        arb_transaction_input().prop_map(ScriptPurpose::Spending),
        arb_staking_credential().prop_map(ScriptPurpose::Rewarding),
        arb_d_cert().prop_map(ScriptPurpose::Certifying)
    ]
}

/// Strategy to generate a TransactionInfo. Note that its inputs, outputs, d_cert,
/// signatories and datums field will each have a length of 0 to 5
pub fn arb_transaction_info() -> impl Strategy<Value = TransactionInfo> {
    (
        vec(arb_tx_in_info(), 5),
        vec(arb_transaction_output(), 5),
        arb_value(),
        arb_value(),
        vec(arb_d_cert(), 5),
        vec((arb_staking_credential(), arb_natural(1)), 5),
        arb_plutus_interval_posix_time(),
        vec(arb_payment_pub_key_hash(), 5),
        vec((arb_datum_hash(), arb_datum()), 5),
        arb_transaction_hash(),
    )
        .prop_map(
            |(inputs, outputs, fee, mint, d_cert, wdrl, valid_range, signatories, datums, id)| {
                TransactionInfo {
                    inputs,
                    outputs,
                    fee,
                    mint,
                    d_cert,
                    wdrl,
                    valid_range,
                    signatories,
                    datums,
                    id,
                }
            },
        )
}

/// Strategy to generate a ScriptContext
pub fn arb_script_context() -> impl Strategy<Value = ScriptContext> {
    (arb_script_purpose(), arb_transaction_info())
        .prop_map(|(purpose, tx_info)| ScriptContext { purpose, tx_info })
}

pub fn arb_lovelace() -> impl Strategy<Value = Lovelace> {
    arb_natural(1).prop_map(Lovelace)
}

pub fn arb_stake_pub_key_hash() -> impl Strategy<Value = StakePubKeyHash> {
    arb_ed25519_pub_key_hash().prop_map(StakePubKeyHash)
}
