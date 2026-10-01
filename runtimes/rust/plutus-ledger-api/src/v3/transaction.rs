//! Types related to Cardano transactions.

use std::{fmt, str::FromStr};

use anyhow::anyhow;
use cardano_serialization_lib as csl;
#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use nom::{
    character::complete::char,
    combinator::{all_consuming, map, map_res},
    error::{context, VerboseError},
    sequence::{preceded, tuple},
    Finish, IResult,
};
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "chrono")]
pub use crate::v1::transaction::POSIXTimeConversionError;
pub use crate::v2::transaction::{
    DCert, POSIXTime, POSIXTimeRange, TransactionOutput, TransactionOutputWithExtraInfo,
    WithdrawalsWithExtraInfo,
};
use crate::{
    self as plutus_ledger_api,
    aux::{big_int, guard_bytes},
    csl::{
        csl_to_pla::FromCSL,
        pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL},
    },
    error::ConversionError,
    plutus_data::{IsPlutusData, PlutusData},
    v2::{
        address::Credential,
        assoc_map::AssocMap,
        crypto::{PaymentPubKeyHash, StakePubKeyHash},
        datum::{Datum, DatumHash},
        redeemer::Redeemer,
        script::ScriptHash,
        value::{CurrencySymbol, Lovelace, Value},
    },
};

use super::{
    crypto::{ledger_bytes, Ed25519PubKeyHash, LedgerBytes},
    ratio::Rational,
};

/////////////////////
// TransactionHash //
/////////////////////

/// 32-bytes blake2b256 hash of a transaction body.
///
/// Also known as Transaction ID or `TxID`.
/// Note: Plutus docs might incorrectly state that it uses SHA256.
/// V3 TransactionHash uses a more efficient Plutus Data encoding
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionHash(pub LedgerBytes);

impl fmt::Display for TransactionHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TransactionHash {
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, ConversionError> {
        Ok(TransactionHash(LedgerBytes(guard_bytes(
            "ScriptHash",
            bytes,
            32,
        )?)))
    }
}

impl FromCSL<csl::TransactionHash> for TransactionHash {
    fn from_csl(value: &csl::TransactionHash) -> Self {
        TransactionHash(LedgerBytes(value.to_bytes()))
    }
}

impl TryFromPLA<TransactionHash> for csl::TransactionHash {
    fn try_from_pla(val: &TransactionHash) -> Result<Self, TryFromPLAError> {
        csl::TransactionHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}

/// Nom parser for TransactionHash
/// Expects a hexadecimal string representation of 32 bytes
/// E.g.: 1122334455667788990011223344556677889900112233445566778899001122
pub(crate) fn transaction_hash(input: &str) -> IResult<&str, TransactionHash, VerboseError<&str>> {
    context(
        "transaction_hash",
        map_res(ledger_bytes, |LedgerBytes(bytes)| {
            TransactionHash::from_bytes(bytes)
        }),
    )(input)
}

impl FromStr for TransactionHash {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(transaction_hash)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing TransactionHash '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

//////////////////////
// TransactionInput //
//////////////////////

/// An input of a transaction
///
/// Also know as `TxOutRef` from Plutus, this identifies a UTxO by its transacton hash and index
/// inside the transaction
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionInput {
    pub transaction_id: TransactionHash,
    pub index: BigInt,
}

/// Serializing into a hexadecimal tx hash, followed by an tx id after a # (e.g. aabbcc#1)
impl fmt::Display for TransactionInput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}#{}", self.transaction_id.0, self.index)
    }
}

impl FromCSL<csl::TransactionInput> for TransactionInput {
    fn from_csl(value: &csl::TransactionInput) -> Self {
        TransactionInput {
            transaction_id: TransactionHash::from_csl(&value.transaction_id()),
            index: BigInt::from_csl(&value.index()),
        }
    }
}

impl TryFromPLA<TransactionInput> for csl::TransactionInput {
    fn try_from_pla(val: &TransactionInput) -> Result<Self, TryFromPLAError> {
        Ok(csl::TransactionInput::new(
            &val.transaction_id.try_to_csl()?,
            val.index.try_to_csl()?,
        ))
    }
}

impl FromCSL<csl::TransactionInputs> for Vec<TransactionInput> {
    fn from_csl(value: &csl::TransactionInputs) -> Self {
        (0..value.len())
            .map(|idx| TransactionInput::from_csl(&value.get(idx)))
            .collect()
    }
}

impl TryFromPLA<Vec<TransactionInput>> for csl::TransactionInputs {
    fn try_from_pla(val: &Vec<TransactionInput>) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::TransactionInputs::new(), |mut acc, input| {
                acc.add(&input.try_to_csl()?);
                Ok(acc)
            })
    }
}

/// Nom parser for TransactionInput
/// Expects a transaction hash of 32 bytes in hexadecimal followed by a # and an integer index
/// E.g.: 1122334455667788990011223344556677889900112233445566778899001122#1
pub(crate) fn transaction_input(
    input: &str,
) -> IResult<&str, TransactionInput, VerboseError<&str>> {
    map(
        tuple((transaction_hash, preceded(char('#'), big_int))),
        |(transaction_id, index)| TransactionInput {
            transaction_id,
            index,
        },
    )(input)
}

impl FromStr for TransactionInput {
    type Err = ConversionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(transaction_input)(s)
            .finish()
            .map_err(|err| {
                ConversionError::ParseError(anyhow!(
                    "Error while parsing TransactionInput '{}': {}",
                    s,
                    err
                ))
            })
            .map(|(_, cs)| cs)
    }
}

/////////////////////////////
// ColdCommitteeCredential //
/////////////////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ColdCommitteeCredential(pub Credential);

////////////////////////////
// HotCommitteeCredential //
////////////////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct HotCommitteeCredential(pub Credential);

////////////////////
// DrepCredential //
////////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct DRepCredential(pub Credential);

//////////
// DRep //
//////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum DRep {
    DRep(DRepCredential),
    AlwaysAbstain,
    AlwaysNoConfidence,
}

///////////////
// Delegatee //
///////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum Delegatee {
    Stake(StakePubKeyHash),
    Vote(DRep),
    StakeVote(StakePubKeyHash, DRep),
}

////////////
// TxCert //
////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum TxCert {
    /// Register staking credential with an optional deposit amount
    RegStaking(Credential, Option<Lovelace>),
    /// Un-Register staking credential with an optional refund amount
    UnRegStaking(Credential, Option<Lovelace>),
    /// Delegate staking credential to a Delegatee
    DelegStaking(Credential, Delegatee),
    /// Register and delegate staking credential to a Delegatee in one certificate. Note that deposit is mandatory.
    RegDeleg(Credential, Delegatee, Lovelace),
    /// Register a DRep with a deposit value. The optional anchor is omitted.
    RegDRep(DRepCredential, Lovelace),
    /// Update a DRep. The optional anchor is omitted.
    UpdateDRep(DRepCredential),
    /// UnRegister a DRep with mandatory refund value
    UnRegDRep(DRepCredential, Lovelace),
    /// A digest of the PoolParams
    PoolRegister(
        /// pool id
        Ed25519PubKeyHash,
        // pool vrf
        Ed25519PubKeyHash,
    ),
    /// The retirement certificate and the Epoch in which the retirement will take place
    PoolRetire(Ed25519PubKeyHash, BigInt),
    /// Authorize a Hot credential for a specific Committee member's cold credential
    AuthHotCommittee(ColdCommitteeCredential, HotCommitteeCredential),
    ResignColdCommittee(ColdCommitteeCredential),
}

//////////
// Vote //
//////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum Voter {
    CommitteeVoter(HotCommitteeCredential),
    DRepVoter(DRepCredential),
    StakePoolVoter(Ed25519PubKeyHash),
}

///////////
// Voter //
///////////

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum Vote {
    VoteNo,
    VoteYes,
    Abstain,
}

////////////////////////
// GovernanceActionId //
////////////////////////

/// Similar to TransactionInput, but for GovernanceAction.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct GovernanceActionId {
    pub tx_id: TransactionHash,
    pub gov_action_id: BigInt,
}

///////////////
// Committee //
///////////////

#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Committee {
    /// Committee members with epoch number when each of them expires
    pub members: AssocMap<ColdCommitteeCredential, BigInt>,
    /// Quorum of the committee that is necessary for a successful vote
    pub quorum: Rational,
}

//////////////////
// Constitution //
//////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Constitution {
    /// Optional guardrail script
    pub constitution_script: Option<ScriptHash>,
}

/////////////////////
// ProtocolVersion //
/////////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ProtocolVersion {
    pub major: BigInt,
    pub minor: BigInt,
}

///////////////////////
// ChangedParameters //
///////////////////////

// TODO(chfanghr): check invariant according to https://github.com/IntersectMBO/plutus/blob/bb33f082d26f8b6576d3f0d423be53eddfb6abd8/plutus-ledger-api/src/PlutusLedgerApi/V3/Contexts.hs#L338-L364
/// A Plutus Data object containing proposed parameter changes.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ChangedParameters(pub PlutusData);

//////////////////////
// GovernanceAction //
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum GovernanceAction {
    /// Propose to change the protocol parameters
    ParameterChange(
        Option<GovernanceActionId>,
        ChangedParameters,
        // The hash of the constitution script
        Option<ScriptHash>,
    ),
    /// Propose to update protocol version
    HardForkInitiation(Option<GovernanceActionId>, ProtocolVersion),
    /// Propose to withdraw from the cardano treasury
    TreasuryWithdrawals(
        AssocMap<Credential, Lovelace>,
        // The hash of the constitution script
        Option<ScriptHash>,
    ),
    /// Propose to create a state of no-confidence in the current constitutional committee
    NoConfidence(Option<GovernanceActionId>),
    /// Propose to update the members of the constitutional committee and/or its signature threshold and/or terms
    UpdateCommittee(
        Option<GovernanceActionId>,
        /// Committee members to be removed
        Vec<ColdCommitteeCredential>,
        /// Committee members to be added
        AssocMap<ColdCommitteeCredential, BigInt>,
        /// New quorum
        Rational,
    ),
    /// Propose to modify the constitution or its guardrail script
    NewConstitution(Option<GovernanceActionId>, Constitution),
    InfoAction,
}

///////////////////////
// ProposalProcedure //
///////////////////////

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ProposalProcedure {
    pub deposit: Lovelace,
    pub return_addr: Credential,
    pub governance_action: GovernanceAction,
}

///////////////////
// ScriptPurpose //
///////////////////

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum ScriptPurpose {
    Minting(CurrencySymbol),
    Spending(TransactionInput),
    Rewarding(Credential),
    Certifying(
        /// 0-based index of the given `TxCert` in `the `tx_certs` field of the `TransactionInfo`
        BigInt,
        TxCert,
    ),
    Voting(Voter),
    Proposing(
        /// 0-based index of the given `ProposalProcedure` in `proposal_procedures` field of the `TransactionInfo`
        BigInt,
        ProposalProcedure,
    ),
}

////////////////
// ScriptInfo //
////////////////

#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub enum ScriptInfo {
    Minting(CurrencySymbol),
    Spending(TransactionInput, Option<Datum>),
    Rewarding(Credential),
    Certifying(BigInt, TxCert),
    Voting(Voter),
    Proposing(BigInt, ProposalProcedure),
}

/////////////////////
// TransactionInfo //
/////////////////////

#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionInfo {
    pub inputs: Vec<TxInInfo>,
    pub reference_inputs: Vec<TxInInfo>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Lovelace,
    pub mint: Value,
    pub tx_certs: Vec<TxCert>,
    pub wdrl: AssocMap<Credential, Lovelace>,
    pub valid_range: POSIXTimeRange,
    pub signatories: Vec<PaymentPubKeyHash>,
    pub redeemers: AssocMap<ScriptPurpose, Redeemer>,
    pub datums: AssocMap<DatumHash, Datum>,
    pub id: TransactionHash,
    pub votes: AssocMap<Voter, AssocMap<GovernanceActionId, Vote>>,
    pub proposal_procedures: Vec<ProposalProcedure>,
    pub current_treasury_amount: Option<Lovelace>,
    pub treasury_donation: Option<Lovelace>,
}

//////////////
// TxInInfo //
//////////////

/// An input of a pending transaction.
#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TxInInfo {
    pub reference: TransactionInput,
    pub output: TransactionOutput,
}

impl From<(TransactionInput, TransactionOutput)> for TxInInfo {
    fn from((reference, output): (TransactionInput, TransactionOutput)) -> TxInInfo {
        TxInInfo { reference, output }
    }
}

///////////////////
// ScriptContext //
///////////////////

#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ScriptContext {
    pub tx_info: TransactionInfo,
    pub redeemer: Redeemer,
    pub script_info: ScriptInfo,
}
