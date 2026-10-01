//! Proptest strategies for Plutus V3 types
//!
//! These strategies always return valid values.

use proptest::{
    collection::vec,
    option,
    prelude::{Just, Strategy},
    prop_oneof,
};

use crate::{
    generators::correct::{
        primitive::arb_integer,
        v1::{
            arb_currency_symbol, arb_datum, arb_ed25519_pub_key_hash, arb_lovelace,
            arb_payment_pub_key_hash, arb_stake_pub_key_hash,
        },
    },
    v3::{
        ratio::Rational,
        transaction::{
            ChangedParameters, ColdCommitteeCredential, Committee, Constitution, DRep,
            DRepCredential, Delegatee, GovernanceAction, GovernanceActionId,
            HotCommitteeCredential, ProposalProcedure, ProtocolVersion, ScriptContext, ScriptInfo,
            ScriptPurpose, TransactionHash, TransactionInfo, TransactionInput, TxCert, TxInInfo,
            Vote, Voter,
        },
    },
};

use super::{
    primitive::arb_natural,
    v1::{
        arb_assoc_map, arb_credential, arb_datum_hash, arb_ledger_bytes, arb_plutus_data,
        arb_plutus_interval_posix_time, arb_redeemer, arb_script_hash, arb_value,
    },
    v2::arb_transaction_output,
};

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

/// Strategy to generate cold committee credentials
pub fn arb_cold_committee_credential() -> impl Strategy<Value = ColdCommitteeCredential> {
    arb_credential().prop_map(ColdCommitteeCredential)
}

/// Strategy to generate hot committee credentials
pub fn arb_hot_committee_credential() -> impl Strategy<Value = HotCommitteeCredential> {
    arb_credential().prop_map(HotCommitteeCredential)
}

/// Strategy to generate DRep credentials
pub fn arb_d_rep_credential() -> impl Strategy<Value = DRepCredential> {
    arb_credential().prop_map(DRepCredential)
}

/// Strategy to generate DReps
pub fn arb_d_rep() -> impl Strategy<Value = DRep> {
    prop_oneof![
        arb_d_rep_credential().prop_map(DRep::DRep),
        Just(DRep::AlwaysAbstain),
        Just(DRep::AlwaysNoConfidence)
    ]
}

/// Strategy to generate delegatees
pub fn arb_delegatee() -> impl Strategy<Value = Delegatee> {
    prop_oneof![
        arb_stake_pub_key_hash().prop_map(Delegatee::Stake),
        arb_d_rep().prop_map(Delegatee::Vote),
        (arb_stake_pub_key_hash(), arb_d_rep()).prop_map(|(h, r)| Delegatee::StakeVote(h, r))
    ]
}

/// Strategy to generate tx certs
pub fn arb_tx_cert() -> impl Strategy<Value = TxCert> {
    prop_oneof![
        (arb_credential(), option::of(arb_lovelace())).prop_map(|(c, l)| TxCert::RegStaking(c, l)),
        (arb_credential(), option::of(arb_lovelace()))
            .prop_map(|(c, l)| TxCert::UnRegStaking(c, l)),
        (arb_credential(), arb_delegatee()).prop_map(|(c, d)| TxCert::DelegStaking(c, d)),
        (arb_credential(), arb_delegatee(), arb_lovelace())
            .prop_map(|(c, d, l)| TxCert::RegDeleg(c, d, l)),
        (arb_d_rep_credential(), arb_lovelace()).prop_map(|(d, l)| TxCert::RegDRep(d, l)),
        arb_d_rep_credential().prop_map(TxCert::UpdateDRep),
        (arb_d_rep_credential(), arb_lovelace()).prop_map(|(d, l)| TxCert::UnRegDRep(d, l)),
        (arb_ed25519_pub_key_hash(), arb_ed25519_pub_key_hash())
            .prop_map(|(pkh1, pkh2)| TxCert::PoolRegister(pkh1, pkh2)),
        (arb_ed25519_pub_key_hash(), arb_integer()).prop_map(|(pkh, i)| TxCert::PoolRetire(pkh, i)),
        (
            arb_cold_committee_credential(),
            arb_hot_committee_credential()
        )
            .prop_map(|(c, h)| TxCert::AuthHotCommittee(c, h)),
        arb_cold_committee_credential().prop_map(TxCert::ResignColdCommittee)
    ]
}

/// Strategy to generate voters
pub fn arb_voter() -> impl Strategy<Value = Voter> {
    prop_oneof![
        arb_hot_committee_credential().prop_map(Voter::CommitteeVoter),
        arb_d_rep_credential().prop_map(Voter::DRepVoter),
        arb_ed25519_pub_key_hash().prop_map(Voter::StakePoolVoter)
    ]
}

/// Strategy to generate votes
pub fn arb_vote() -> impl Strategy<Value = Vote> {
    prop_oneof![Just(Vote::VoteNo), Just(Vote::VoteYes), Just(Vote::Abstain)]
}

/// Strategy to generate governance action ids
pub fn arb_governance_action_id() -> impl Strategy<Value = GovernanceActionId> {
    (arb_transaction_hash(), arb_integer()).prop_map(|(tx_id, gov_action_id)| GovernanceActionId {
        tx_id,
        gov_action_id,
    })
}

/// Strategy to generate committees
pub fn arb_committee() -> impl Strategy<Value = Committee> {
    (
        arb_assoc_map(arb_cold_committee_credential(), arb_integer()),
        arb_rational(),
    )
        .prop_map(|(members, quorum)| Committee { members, quorum })
}

/// Strategy to generate rationals
pub fn arb_rational() -> impl Strategy<Value = Rational> {
    (arb_integer(), arb_integer()).prop_map(|(n, d)| Rational(n, d))
}

/// Strategy to generate constitutions
pub fn arb_constitution() -> impl Strategy<Value = Constitution> {
    option::of(arb_script_hash()).prop_map(|constitution_script| Constitution {
        constitution_script,
    })
}

/// Strategy to generate protocol versions
pub fn arb_protocol_version() -> impl Strategy<Value = ProtocolVersion> {
    (arb_natural(1), arb_natural(1)).prop_map(|(major, minor)| ProtocolVersion { major, minor })
}

/// Strategy to generate change parameters
pub fn arb_changed_parameters() -> impl Strategy<Value = ChangedParameters> {
    arb_plutus_data().prop_map(ChangedParameters)
}

/// Strategy to generate governance actions
pub fn arb_governance_action() -> impl Strategy<Value = GovernanceAction> {
    prop_oneof![
        (
            option::of(arb_governance_action_id()),
            arb_changed_parameters(),
            option::of(arb_script_hash())
        )
            .prop_map(|(g, c, s)| GovernanceAction::ParameterChange(g, c, s)),
        (
            option::of(arb_governance_action_id()),
            arb_protocol_version()
        )
            .prop_map(|(g, p)| GovernanceAction::HardForkInitiation(g, p)),
        (
            arb_assoc_map(arb_credential(), arb_lovelace()),
            option::of(arb_script_hash())
        )
            .prop_map(|(a, s)| GovernanceAction::TreasuryWithdrawals(a, s)),
        option::of(arb_governance_action_id()).prop_map(GovernanceAction::NoConfidence),
        (
            option::of(arb_governance_action_id()),
            vec(arb_cold_committee_credential(), 5),
            arb_assoc_map(arb_cold_committee_credential(), arb_integer()),
            arb_rational()
        )
            .prop_map(|(g, c, cm, q)| GovernanceAction::UpdateCommittee(g, c, cm, q)),
        (option::of(arb_governance_action_id()), arb_constitution())
            .prop_map(|(g, c)| GovernanceAction::NewConstitution(g, c)),
        Just(GovernanceAction::InfoAction)
    ]
}

/// Strategy to generate protocol procedures
pub fn arb_proposal_procedure() -> impl Strategy<Value = ProposalProcedure> {
    (arb_lovelace(), arb_credential(), arb_governance_action()).prop_map(|(l, c, g)| {
        ProposalProcedure {
            deposit: l,
            return_addr: c,
            governance_action: g,
        }
    })
}

/// Strategy to generate script purposes
pub fn arb_script_purpose() -> impl Strategy<Value = ScriptPurpose> {
    prop_oneof![
        arb_currency_symbol().prop_map(ScriptPurpose::Minting),
        arb_transaction_input().prop_map(ScriptPurpose::Spending),
        arb_credential().prop_map(ScriptPurpose::Rewarding),
        (arb_integer(), arb_tx_cert()).prop_map(|(i, c)| ScriptPurpose::Certifying(i, c)),
        arb_voter().prop_map(ScriptPurpose::Voting),
        (arb_integer(), arb_proposal_procedure()).prop_map(|(i, p)| ScriptPurpose::Proposing(i, p))
    ]
}

/// Strategy to generate script info
pub fn arb_script_info() -> impl Strategy<Value = ScriptInfo> {
    prop_oneof![
        arb_currency_symbol().prop_map(ScriptInfo::Minting),
        (arb_transaction_input(), option::of(arb_datum()))
            .prop_map(|(i, d)| ScriptInfo::Spending(i, d)),
        arb_credential().prop_map(ScriptInfo::Rewarding),
        (arb_integer(), arb_tx_cert()).prop_map(|(i, c)| ScriptInfo::Certifying(i, c)),
        arb_voter().prop_map(ScriptInfo::Voting),
        (arb_integer(), arb_proposal_procedure()).prop_map(|(i, p)| ScriptInfo::Proposing(i, p))
    ]
}

/// Strategy to generate transaction info
pub fn arb_transaction_info() -> impl Strategy<Value = TransactionInfo> {
    (
        vec(arb_tx_in_info(), 5),
        vec(arb_tx_in_info(), 5),
        vec(arb_transaction_output(), 5),
        arb_lovelace(),
        arb_value(),
        vec(arb_tx_cert(), 5),
        arb_assoc_map(arb_credential(), arb_lovelace()),
        arb_plutus_interval_posix_time(),
        vec(arb_payment_pub_key_hash(), 5),
        arb_assoc_map(arb_script_purpose(), arb_redeemer()),
        arb_assoc_map(arb_datum_hash(), arb_datum()),
        // HACK(chfanghr): Strategy is not implemented for longer tuples
        (
            arb_transaction_hash(),
            arb_assoc_map(
                arb_voter(),
                arb_assoc_map(arb_governance_action_id(), arb_vote()),
            ),
            vec(arb_proposal_procedure(), 5),
            option::of(arb_lovelace()),
            option::of(arb_lovelace()),
        ),
    )
        .prop_map(
            |(
                inputs,
                reference_inputs,
                outputs,
                fee,
                mint,
                tx_certs,
                wdrl,
                valid_range,
                signatories,
                redeemers,
                datums,
                (id, votes, proposal_procedures, current_treasury_amount, treasury_donation),
            )| {
                TransactionInfo {
                    inputs,
                    reference_inputs,
                    outputs,
                    fee,
                    mint,
                    tx_certs,
                    wdrl,
                    valid_range,
                    signatories,
                    redeemers,
                    datums,
                    id,
                    votes,
                    proposal_procedures,
                    current_treasury_amount,
                    treasury_donation,
                }
            },
        )
}

/// Strategy to generate a TxInInfo
pub fn arb_tx_in_info() -> impl Strategy<Value = TxInInfo> {
    (arb_transaction_input(), arb_transaction_output())
        .prop_map(|(reference, output)| TxInInfo { reference, output })
}

/// Strategy to generate script contexts
pub fn arb_script_context() -> impl Strategy<Value = ScriptContext> {
    (arb_transaction_info(), arb_redeemer(), arb_script_info()).prop_map(
        |(tx_info, redeemer, script_info)| ScriptContext {
            tx_info,
            redeemer,
            script_info,
        },
    )
}
