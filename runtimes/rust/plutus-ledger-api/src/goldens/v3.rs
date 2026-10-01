//! Golden test data or Plutus V3 types (incomplete)
use num_bigint::BigInt;

pub use super::v2::{
    sample_address, sample_asset_class, sample_chain_pointer, sample_credential,
    sample_currency_symbol, sample_datum, sample_datum_hash, sample_dcert,
    sample_ed25519_pub_key_hash, sample_output_datum, sample_payment_pub_key_hash,
    sample_plutus_data, sample_plutus_interval, sample_redeemer, sample_redeemer_hash,
    sample_script_hash, sample_staking_credential, sample_token_name, sample_transaction_output,
    sample_value,
};
use crate::v3::{
    crypto::LedgerBytes,
    transaction::{
        ColdCommitteeCredential, DRepCredential, HotCommitteeCredential, TransactionHash,
        TransactionInput, TxInInfo,
    },
};

pub fn sample_transaction_hash() -> TransactionHash {
    TransactionHash(LedgerBytes([0].repeat(32).to_vec()))
}

pub fn sample_transaction_input() -> TransactionInput {
    TransactionInput {
        transaction_id: sample_transaction_hash(),
        index: BigInt::from(3),
    }
}

pub fn sample_tx_in_info() -> TxInInfo {
    TxInInfo {
        reference: sample_transaction_input(),
        output: sample_transaction_output(),
    }
}

pub fn sample_cold_committee_credential() -> ColdCommitteeCredential {
    ColdCommitteeCredential(sample_credential())
}

pub fn sample_hot_committee_credential() -> HotCommitteeCredential {
    HotCommitteeCredential(sample_credential())
}

pub fn sample_drep_committee_credential() -> DRepCredential {
    DRepCredential(sample_credential())
}

// TODO(szg251): Missing implementations
// DRep
// Delegatee
// TxCert
// Vote
// Voter
// GovernanceActionId
// Committee
// Constitution
// ProtocolVersion
// ChangedParameters
// GovernanceAction
// ProposalProcedure
// ScriptPurpose
// ScriptInfo
// TransactionInfo
// ScriptContext
