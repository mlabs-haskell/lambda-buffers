//! Types related to Cardano transactions.

use std::collections::BTreeMap;

use cardano_serialization_lib as csl;
#[cfg(feature = "lbf")]
use lbr_prelude::json::Json;
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate as plutus_ledger_api;
use crate::csl::csl_to_pla::{FromCSL, TryFromCSL, TryFromCSLError, TryToPLA};
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};
use crate::plutus_data::IsPlutusData;
#[cfg(feature = "chrono")]
pub use crate::v1::transaction::POSIXTimeConversionError;
pub use crate::v1::transaction::{
    DCert, POSIXTime, POSIXTimeRange, ScriptPurpose, TransactionHash, TransactionInput,
};

use super::address::AddressWithExtraInfo;
use super::{
    address::{Address, RewardAddressWithExtraInfo, StakingCredential},
    assoc_map::AssocMap,
    crypto::PaymentPubKeyHash,
    datum::{Datum, DatumHash, OutputDatum},
    redeemer::Redeemer,
    script::ScriptHash,
    value::Value,
};

///////////////////////
// TransactionOutput //
///////////////////////

/// An output of a transaction
///
/// This must include the target address, an optional datum, an optional reference script, and the
/// amount of output tokens
#[derive(Clone, Debug, PartialEq, Eq, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionOutput {
    pub address: Address,
    pub value: Value,
    pub datum: OutputDatum,
    pub reference_script: Option<ScriptHash>,
}

impl TryFromCSL<csl::TransactionOutput> for TransactionOutput {
    fn try_from_csl(value: &csl::TransactionOutput) -> Result<Self, TryFromCSLError> {
        Ok(TransactionOutput {
            address: value.address().try_to_pla()?,
            datum: if value.has_data_hash() {
                OutputDatum::DatumHash(DatumHash::from_csl(&value.data_hash().unwrap()))
            } else if value.has_plutus_data() {
                OutputDatum::InlineDatum(Datum(value.plutus_data().unwrap().try_to_pla()?))
            } else {
                OutputDatum::None
            },
            reference_script: if value.has_script_ref() {
                let script_ref = value.script_ref().unwrap();
                let script_hash = if script_ref.is_native_script() {
                    script_ref.native_script().unwrap().hash()
                } else {
                    script_ref.plutus_script().unwrap().hash()
                };
                Some(ScriptHash::from_csl(&script_hash))
            } else {
                None
            },
            value: Value::from_csl(&value.amount()),
        })
    }
}

impl TryFromCSL<csl::TransactionOutputs> for Vec<TransactionOutput> {
    fn try_from_csl(value: &csl::TransactionOutputs) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| TransactionOutput::try_from_csl(&value.get(idx)))
            .collect()
    }
}

#[derive(Clone, Debug)]
pub struct TransactionOutputWithExtraInfo<'a> {
    pub transaction_output: &'a TransactionOutput,
    pub scripts: &'a BTreeMap<ScriptHash, csl::PlutusScript>,
    pub network_id: u8,
    pub data_cost: &'a csl::DataCost,
}

impl TryFromPLA<TransactionOutputWithExtraInfo<'_>> for csl::TransactionOutput {
    fn try_from_pla(val: &TransactionOutputWithExtraInfo<'_>) -> Result<Self, TryFromPLAError> {
        let mut output_builder = csl::TransactionOutputBuilder::new().with_address(
            &AddressWithExtraInfo {
                address: &val.transaction_output.address,
                network_tag: val.network_id,
            }
            .try_to_csl()?,
        );

        output_builder = match &val.transaction_output.datum {
            OutputDatum::None => output_builder,
            OutputDatum::InlineDatum(Datum(d)) => output_builder.with_plutus_data(&d.try_to_csl()?),
            OutputDatum::DatumHash(dh) => output_builder.with_data_hash(&dh.try_to_csl()?),
        };

        let script_ref = val
            .transaction_output
            .reference_script
            .clone()
            .map(|script_hash| -> Result<_, TryFromPLAError> {
                let script = val
                    .scripts
                    .get(&script_hash)
                    .ok_or(TryFromPLAError::MissingScript(script_hash))?;
                Ok(csl::ScriptRef::new_plutus_script(script))
            })
            .transpose()?;

        if let Some(script_ref) = &script_ref {
            output_builder = output_builder.with_script_ref(script_ref);
        };

        let value_without_min_utxo = val.transaction_output.value.try_to_csl()?;

        let mut calc = csl::MinOutputAdaCalculator::new_empty(val.data_cost)
            .map_err(TryFromPLAError::CSLJsError)?;
        calc.set_amount(&value_without_min_utxo);
        match &val.transaction_output.datum {
            OutputDatum::None => {}
            OutputDatum::InlineDatum(Datum(d)) => {
                calc.set_plutus_data(&d.try_to_csl()?);
            }
            OutputDatum::DatumHash(dh) => {
                calc.set_data_hash(&dh.try_to_csl()?);
            }
        };
        if let Some(script_ref) = script_ref {
            calc.set_script_ref(&script_ref);
        }

        let required_coin = calc.calculate_ada().map_err(TryFromPLAError::CSLJsError)?;
        let coin = std::cmp::max(value_without_min_utxo.coin(), required_coin);

        let value = match value_without_min_utxo.multiasset() {
            Some(multiasset) => csl::Value::new_with_assets(&coin, &multiasset),
            None => csl::Value::new(&coin),
        };

        output_builder
            .next()
            .map_err(TryFromPLAError::CSLJsError)?
            .with_value(&value)
            .build()
            .map_err(TryFromPLAError::CSLJsError)
    }
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

// TransactionInfo //
/////////////////////

/// A pending transaction as seen by validator scripts, also known as TxInfo in Plutus
#[derive(Debug, PartialEq, Eq, Clone, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionInfo {
    pub inputs: Vec<TxInInfo>,
    pub reference_inputs: Vec<TxInInfo>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Value,
    pub mint: Value,
    pub d_cert: Vec<DCert>,
    pub wdrl: AssocMap<StakingCredential, BigInt>,
    pub valid_range: POSIXTimeRange,
    pub signatories: Vec<PaymentPubKeyHash>,
    pub redeemers: AssocMap<ScriptPurpose, Redeemer>,
    pub datums: AssocMap<DatumHash, Datum>,
    pub id: TransactionHash,
}

#[derive(Clone, Debug)]
pub struct WithdrawalsWithExtraInfo<'a> {
    pub withdrawals: &'a AssocMap<StakingCredential, BigInt>,
    pub network_tag: u8,
}

impl TryFromPLA<WithdrawalsWithExtraInfo<'_>> for csl::Withdrawals {
    fn try_from_pla(val: &WithdrawalsWithExtraInfo<'_>) -> Result<Self, TryFromPLAError> {
        val.withdrawals
            .0
            .iter()
            .try_fold(csl::Withdrawals::new(), |mut acc, (s, q)| {
                acc.insert(
                    &RewardAddressWithExtraInfo {
                        staking_credential: s,
                        network_tag: val.network_tag,
                    }
                    .try_to_csl()?,
                    &q.try_to_csl()?,
                );
                Ok(acc)
            })
    }
}

///////////////////
// ScriptContext //
///////////////////

/// The context that is presented to the currently-executing script.
#[derive(Debug, PartialEq, Eq, Clone, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ScriptContext {
    pub tx_info: TransactionInfo,
    pub purpose: ScriptPurpose,
}
