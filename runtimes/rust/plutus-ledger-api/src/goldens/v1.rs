//! Golden test data or Plutus V1 types
use crate::{
    plutus_data::PlutusData,
    v1::{
        address::{Address, Credential, StakingCredential},
        crypto::{Ed25519PubKeyHash, LedgerBytes, PaymentPubKeyHash},
        datum::{Datum, DatumHash},
        interval::{Interval, PlutusInterval},
        redeemer::{Redeemer, RedeemerHash},
        script::{MintingPolicyHash, ScriptHash, ValidatorHash},
        transaction::{
            DCert, POSIXTime, ScriptContext, ScriptPurpose, TransactionHash, TransactionInfo,
            TransactionInput, TransactionOutput, TxInInfo,
        },
        value::{AssetClass, CurrencySymbol, TokenName, Value},
    },
    v2::address::{CertificateIndex, ChainPointer, Slot, TransactionIndex},
};
use num_bigint::BigInt;

pub fn sample_script_hash() -> ScriptHash {
    ScriptHash(LedgerBytes([1].repeat(28).to_vec()))
}

pub fn sample_currency_symbol() -> CurrencySymbol {
    CurrencySymbol::NativeToken(MintingPolicyHash(sample_script_hash()))
}

pub fn sample_token_name() -> TokenName {
    TokenName::from_string("Something").unwrap()
}

pub fn sample_asset_class() -> AssetClass {
    AssetClass {
        currency_symbol: sample_currency_symbol(),
        token_name: sample_token_name(),
    }
}

pub fn sample_value() -> Value {
    Value::token_value(
        &sample_currency_symbol(),
        &sample_token_name(),
        &BigInt::from(123),
    ) + Value::ada_value(&BigInt::from(234))
}

pub fn sample_plutus_interval() -> PlutusInterval<POSIXTime> {
    PlutusInterval::from(Interval::StartAt(POSIXTime(BigInt::from(1723106785))))
}

pub fn sample_ed25519_pub_key_hash() -> Ed25519PubKeyHash {
    Ed25519PubKeyHash(LedgerBytes([0].repeat(28).to_vec()))
}

pub fn sample_credential() -> Credential {
    Credential::Script(ValidatorHash(sample_script_hash()))
}

pub fn sample_staking_credential() -> StakingCredential {
    StakingCredential::Hash(sample_credential())
}

pub fn sample_chain_pointer() -> ChainPointer {
    ChainPointer {
        slot_number: Slot(134561.into()),
        transaction_index: TransactionIndex(4.into()),
        certificate_index: CertificateIndex(10.into()),
    }
}

pub fn sample_address() -> Address {
    Address {
        credential: Credential::PubKey(sample_ed25519_pub_key_hash()),
        staking_credential: Some(sample_staking_credential()),
    }
}

pub fn sample_transaction_hash() -> TransactionHash {
    TransactionHash(LedgerBytes([0].repeat(32).to_vec()))
}

pub fn sample_transaction_input() -> TransactionInput {
    TransactionInput {
        transaction_id: sample_transaction_hash(),
        index: BigInt::from(3),
    }
}

pub fn sample_datum_hash() -> DatumHash {
    DatumHash(LedgerBytes([0].repeat(32).to_vec()))
}

pub fn sample_plutus_data() -> PlutusData {
    PlutusData::constr(1, vec![PlutusData::bytes("Something".as_bytes().to_vec())])
}

pub fn sample_datum() -> Datum {
    Datum(sample_plutus_data())
}

pub fn sample_redeemer_hash() -> RedeemerHash {
    RedeemerHash(LedgerBytes([0].repeat(32).to_vec()))
}

pub fn sample_redeemer() -> Redeemer {
    Redeemer(PlutusData::Integer(BigInt::from(144)))
}

pub fn sample_tx_in_info() -> TxInInfo {
    TxInInfo {
        reference: sample_transaction_input(),
        output: sample_transaction_output(),
    }
}

pub fn sample_transaction_output() -> TransactionOutput {
    TransactionOutput {
        address: sample_address(),
        value: sample_value(),
        datum_hash: Some(sample_datum_hash()),
    }
}

pub fn sample_payment_pub_key_hash() -> PaymentPubKeyHash {
    PaymentPubKeyHash(sample_ed25519_pub_key_hash())
}

pub fn sample_script_purpose() -> ScriptPurpose {
    ScriptPurpose::Minting(sample_currency_symbol())
}

pub fn sample_dcert() -> DCert {
    DCert::DelegDelegate(sample_staking_credential(), sample_payment_pub_key_hash())
}

pub fn sample_transaction_info() -> TransactionInfo {
    TransactionInfo {
        inputs: vec![sample_tx_in_info()],
        outputs: vec![sample_transaction_output()],
        fee: sample_value(),
        mint: sample_value(),
        d_cert: vec![sample_dcert()],
        wdrl: vec![(sample_staking_credential(), BigInt::from(12))],
        valid_range: sample_plutus_interval(),
        signatories: vec![sample_payment_pub_key_hash()],
        datums: vec![(sample_datum_hash(), sample_datum())],
        id: sample_transaction_hash(),
    }
}

pub fn sample_script_context() -> ScriptContext {
    ScriptContext {
        tx_info: sample_transaction_info(),
        purpose: sample_script_purpose(),
    }
}
