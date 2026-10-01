//! Proptest strategies for Plutus V2 types
//!
//! These strategies always return valid values.
use super::primitive::arb_natural;
use super::v1::{
    arb_assoc_map, arb_d_cert, arb_payment_pub_key_hash, arb_plutus_interval_posix_time,
    arb_redeemer, arb_script_purpose, arb_staking_credential, arb_transaction_hash,
};
use crate::generators::correct::v1::{
    arb_address, arb_datum, arb_datum_hash, arb_script_hash, arb_transaction_input, arb_value,
};
use crate::v2::datum::OutputDatum;
use crate::v2::transaction::{ScriptContext, TransactionInfo, TransactionOutput, TxInInfo};
use proptest::collection::vec;
use proptest::option;
use proptest::prelude::{prop_oneof, Just};
use proptest::strategy::Strategy;

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
    (arb_transaction_input(), arb_transaction_output())
        .prop_map(|(reference, output)| TxInInfo { reference, output })
}

pub fn arb_transaction_info() -> impl Strategy<Value = TransactionInfo> {
    (
        vec(arb_tx_in_info(), 5),
        vec(arb_tx_in_info(), 5),
        vec(arb_transaction_output(), 5),
        arb_value(),
        arb_value(),
        vec(arb_d_cert(), 5),
        arb_assoc_map(arb_staking_credential(), arb_natural(1)),
        arb_plutus_interval_posix_time(),
        vec(arb_payment_pub_key_hash(), 5),
        arb_assoc_map(arb_script_purpose(), arb_redeemer()),
        arb_assoc_map(arb_datum_hash(), arb_datum()),
        arb_transaction_hash(),
    )
        .prop_map(
            |(
                inputs,
                reference_inputs,
                outputs,
                fee,
                mint,
                d_cert,
                wdrl,
                valid_range,
                signatories,
                redeemers,
                datums,
                id,
            )| {
                TransactionInfo {
                    inputs,
                    reference_inputs,
                    outputs,
                    fee,
                    mint,
                    d_cert,
                    wdrl,
                    valid_range,
                    signatories,
                    redeemers,
                    datums,
                    id,
                }
            },
        )
}

pub fn arb_script_context() -> impl Strategy<Value = ScriptContext> {
    (arb_script_purpose(), arb_transaction_info())
        .prop_map(|(purpose, tx_info)| ScriptContext { purpose, tx_info })
}
