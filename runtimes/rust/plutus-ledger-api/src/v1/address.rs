//! Types related to Cardano addresses
use std::str::FromStr;

use anyhow::anyhow;
use cardano_serialization_lib as csl;

#[cfg(feature = "lbf")]
use lbr_prelude::json::{self, Error, Json};
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate as plutus_ledger_api;
use crate::csl::csl_to_pla::{FromCSL, TryFromCSL, TryFromCSLError, TryToPLA};
use crate::csl::pla_to_csl::{TryFromPLA, TryFromPLAError, TryToCSL};
use crate::plutus_data::{
    parse_constr, parse_fixed_len_constr_fields, IsPlutusData, PlutusData, PlutusDataError,
};
use crate::v1::crypto::Ed25519PubKeyHash;
use crate::v1::script::ValidatorHash;

/////////////
// Address //
/////////////

/// Shelley Address for wallets or validators
///
/// An address consists of a payment part (credential) and a delegation part (staking_credential).
/// In order to serialize an address to `bech32`, the network kind must be known.
/// For a better understanding of all the Cardano address types, read [CIP 19](https://cips.cardano.org/cips/cip19/)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Address {
    pub credential: Credential,
    pub staking_credential: Option<StakingCredential>,
}

impl Address {
    pub fn with_extra_info(&self, network_tag: u8) -> AddressWithExtraInfo {
        AddressWithExtraInfo {
            address: self,
            network_tag,
        }
    }
}

impl FromStr for Address {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let csl_addr = csl::Address::from_bech32(s)
            .map_err(|err| anyhow!("Couldn't parse bech32 address: {}", err))?;
        csl_addr
            .try_to_pla()
            .map_err(|err| anyhow!("Couldn't convert address: {}", err))
    }
}

impl TryFromCSL<csl::Address> for Address {
    fn try_from_csl(value: &csl::Address) -> Result<Self, TryFromCSLError> {
        if let Some(addr) = csl::BaseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_cred())),
            })
        } else if let Some(addr) = csl::PointerAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_pointer())),
            })
        } else if let Some(addr) = csl::EnterpriseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: None,
            })
        } else {
            Err(TryFromCSLError::ImpossibleConversion(format!(
                "Unable to represent address {:?}",
                value
            )))
        }
    }
}

#[derive(Clone, Debug)]
/// Address with network information. The `WithExtraInfo` variant has Display instance, serializing into
/// a bech32 address format.
pub struct AddressWithExtraInfo<'a> {
    pub address: &'a Address,
    pub network_tag: u8,
}

impl TryFromPLA<AddressWithExtraInfo<'_>> for csl::Address {
    fn try_from_pla(val: &AddressWithExtraInfo<'_>) -> Result<Self, TryFromPLAError> {
        let payment = val.address.credential.try_to_csl()?;

        Ok(match val.address.staking_credential {
            None => csl::EnterpriseAddress::new(val.network_tag, &payment).to_address(),
            Some(ref sc) => match sc {
                StakingCredential::Hash(c) => {
                    csl::BaseAddress::new(val.network_tag, &payment, &c.try_to_csl()?).to_address()
                }
                StakingCredential::Pointer(ptr) => {
                    csl::PointerAddress::new(val.network_tag, &payment, &ptr.try_to_csl()?)
                        .to_address()
                }
            },
        })
    }
}

/// Serializing into a bech32 address format.
impl std::fmt::Display for AddressWithExtraInfo<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bech32_addr: Option<String> = self
            .try_to_csl()
            .ok()
            .and_then(|csl_addr: csl::Address| csl_addr.to_bech32(None).ok());
        match bech32_addr {
            Some(addr) => write!(f, "{}", addr),
            None => write!(f, "INVALID ADDRESS {:?}", self),
        }
    }
}

////////////////
// Credential //
////////////////

/// A public key hash or validator hash credential (used as a payment or a staking credential)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Constr"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Credential {
    PubKey(Ed25519PubKeyHash),
    Script(ValidatorHash),
}

#[cfg(feature = "lbf")]
impl Json for Credential {
    fn to_json(&self) -> serde_json::Value {
        match self {
            Credential::PubKey(pkh) => {
                json::json_constructor("PubKeyCredential", vec![pkh.to_json()])
            }
            Credential::Script(val_hash) => {
                json::json_constructor("ScriptCredential", vec![val_hash.to_json()])
            }
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        json::case_json_constructor(
            "Plutus.V1.Credential",
            vec![
                (
                    "PubKeyCredential",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [pkh] => Ok(Credential::PubKey(Json::from_json(pkh)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.Credential".to_owned(),
                        }),
                    }),
                ),
                (
                    "ScriptCredential",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val_hash] => Ok(Credential::Script(Json::from_json(val_hash)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.Credential".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl FromCSL<csl::Credential> for Credential {
    fn from_csl(value: &csl::Credential) -> Self {
        match value.kind() {
            csl::CredKind::Key => {
                Credential::PubKey(Ed25519PubKeyHash::from_csl(&value.to_keyhash().unwrap()))
            }
            csl::CredKind::Script => {
                Credential::Script(ValidatorHash::from_csl(&value.to_scripthash().unwrap()))
            }
        }
    }
}

impl TryFromPLA<Credential> for csl::Credential {
    fn try_from_pla(val: &Credential) -> Result<Self, TryFromPLAError> {
        match val {
            Credential::PubKey(pkh) => Ok(csl::Credential::from_keyhash(&pkh.try_to_csl()?)),
            Credential::Script(sh) => Ok(csl::Credential::from_scripthash(&sh.0.try_to_csl()?)),
        }
    }
}

///////////////////////
// StakingCredential //
///////////////////////

/// Credential (public key hash or pointer) used for staking
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum StakingCredential {
    Hash(Credential),
    Pointer(ChainPointer),
}

// NOTE(chfanghr): ChainPointer doesn't have a IsPlutusData instance so derive doesn't work here.
impl IsPlutusData for StakingCredential {
    fn to_plutus_data(&self) -> PlutusData {
        match self {
            StakingCredential::Hash(credential) => {
                PlutusData::Constr(BigInt::from(0), vec![credential.to_plutus_data()])
            }
            StakingCredential::Pointer(ChainPointer {
                slot_number,
                transaction_index,
                certificate_index,
            }) => PlutusData::Constr(
                BigInt::from(1),
                vec![
                    slot_number.to_plutus_data(),
                    transaction_index.to_plutus_data(),
                    certificate_index.to_plutus_data(),
                ],
            ),
        }
    }

    fn from_plutus_data(data: &PlutusData) -> Result<Self, PlutusDataError> {
        let (tag, fields) = parse_constr(data)?;
        match tag {
            0 => {
                let [field] = parse_fixed_len_constr_fields::<1>(fields)?;
                Ok(Self::Hash(Credential::from_plutus_data(field)?))
            }
            1 => {
                let [field_0, field_1, field_2] = parse_fixed_len_constr_fields::<3>(fields)?;
                Ok(Self::Pointer(ChainPointer {
                    slot_number: Slot::from_plutus_data(field_0)?,
                    transaction_index: TransactionIndex::from_plutus_data(field_1)?,
                    certificate_index: CertificateIndex::from_plutus_data(field_2)?,
                }))
            }
            _ => Err(PlutusDataError::UnexpectedPlutusInvariant {
                wanted: "Constr with tag 0 or 1".to_owned(),
                got: tag.to_string(),
            }),
        }
    }
}

#[cfg(feature = "lbf")]
impl Json for StakingCredential {
    fn to_json(&self) -> serde_json::Value {
        match self {
            StakingCredential::Hash(pkh) => {
                json::json_constructor("StakingHash", vec![pkh.to_json()])
            }
            StakingCredential::Pointer(val_hash) => {
                json::json_constructor("StakingPtr", vec![val_hash.to_json()])
            }
        }
    }

    fn from_json(value: &serde_json::Value) -> Result<Self, Error> {
        json::case_json_constructor(
            "Plutus.V1.StakingCredential",
            vec![
                (
                    "StakingHash",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [pkh] => Ok(StakingCredential::Hash(Json::from_json(pkh)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.StakingCredential".to_owned(),
                        }),
                    }),
                ),
                (
                    "StakingPtr",
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [val_hash] => Ok(StakingCredential::Pointer(Json::from_json(val_hash)?)),
                        _ => Err(Error::UnexpectedArrayLength {
                            wanted: 1,
                            got: ctor_fields.len(),
                            parser: "Plutus.V1.StakingCredential".to_owned(),
                        }),
                    }),
                ),
            ],
            value,
        )
    }
}

impl FromCSL<csl::Credential> for StakingCredential {
    fn from_csl(value: &csl::Credential) -> Self {
        StakingCredential::Hash(Credential::from_csl(value))
    }
}

impl TryFromPLA<StakingCredential> for csl::Credential {
    fn try_from_pla(val: &StakingCredential) -> Result<Self, TryFromPLAError> {
        match val {
            StakingCredential::Hash(c) => c.try_to_csl(),
            StakingCredential::Pointer(_) => Err(TryFromPLAError::ImpossibleConversion(
                "cannot represent chain pointer".into(),
            )),
        }
    }
}

impl FromCSL<csl::Pointer> for StakingCredential {
    fn from_csl(value: &csl::Pointer) -> Self {
        StakingCredential::Pointer(ChainPointer::from_csl(value))
    }
}

#[derive(Clone, Debug)]
pub struct RewardAddressWithExtraInfo<'a> {
    pub staking_credential: &'a StakingCredential,
    pub network_tag: u8,
}

impl TryFromPLA<RewardAddressWithExtraInfo<'_>> for csl::RewardAddress {
    fn try_from_pla(val: &RewardAddressWithExtraInfo<'_>) -> Result<Self, TryFromPLAError> {
        Ok(csl::RewardAddress::new(
            val.network_tag,
            &val.staking_credential.try_to_csl()?,
        ))
    }
}

//////////////////
// ChainPointer //
//////////////////

/// In an address, a chain pointer refers to a point of the chain containing a stake key
/// registration certificate. A point is identified by 3 coordinates:
/// - An absolute slot number
/// - A transaction inder (within that slot)
/// - A (delegation) certificate index (within that transacton)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct ChainPointer {
    pub slot_number: Slot,
    pub transaction_index: TransactionIndex,
    pub certificate_index: CertificateIndex,
}

impl FromCSL<csl::Pointer> for ChainPointer {
    fn from_csl(value: &csl::Pointer) -> Self {
        ChainPointer {
            slot_number: Slot::from_csl(&value.slot_bignum()),
            transaction_index: TransactionIndex::from_csl(&value.tx_index_bignum()),
            certificate_index: CertificateIndex::from_csl(&value.cert_index_bignum()),
        }
    }
}

impl TryFromPLA<ChainPointer> for csl::Pointer {
    fn try_from_pla(val: &ChainPointer) -> Result<Self, TryFromPLAError> {
        Ok(csl::Pointer::new_pointer(
            &val.slot_number.try_to_csl()?,
            &val.transaction_index.try_to_csl()?,
            &val.certificate_index.try_to_csl()?,
        ))
    }
}

//////////
// Slot //
//////////

/// Number of slots elapsed since genesis
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct Slot(pub BigInt);

impl FromCSL<csl::BigNum> for Slot {
    fn from_csl(value: &csl::BigNum) -> Self {
        Slot(BigInt::from_csl(value))
    }
}

impl TryFromPLA<Slot> for csl::BigNum {
    fn try_from_pla(val: &Slot) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

//////////////////////
// CertificateIndex //
//////////////////////

/// Position of the certificate in a certain transaction
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct CertificateIndex(pub BigInt);

impl FromCSL<csl::BigNum> for CertificateIndex {
    fn from_csl(value: &csl::BigNum) -> Self {
        CertificateIndex(BigInt::from_csl(value))
    }
}

impl TryFromPLA<CertificateIndex> for csl::BigNum {
    fn try_from_pla(val: &CertificateIndex) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

//////////////////////
// TransactionIndex //
//////////////////////

/// Position of a transaction in a given slot
/// This is not identical to the index of a `TransactionInput`
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, IsPlutusData)]
#[is_plutus_data_derive_strategy = "Newtype"]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "lbf", derive(Json))]
pub struct TransactionIndex(pub BigInt);

impl FromCSL<csl::BigNum> for TransactionIndex {
    fn from_csl(value: &csl::BigNum) -> Self {
        TransactionIndex(BigInt::from_csl(value))
    }
}

impl TryFromPLA<TransactionIndex> for csl::BigNum {
    fn try_from_pla(val: &TransactionIndex) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}
