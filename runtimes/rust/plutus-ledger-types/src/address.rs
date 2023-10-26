use crate::crypto::Ed25519PubKeyHash;
use crate::ledger_state::Slot;
use crate::script::ValidatorHash;
use num_bigint::BigInt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Shelley Address for wallets or validators
///
/// An address consists of a payment part (credential) and a delegation part (staking_credential).
/// In order to serialize an address to `bech32`, the network kind must be known.
/// For a better understanding of all the Cardano address types, read [CIP 19](https://cips.cardano.org/cips/cip19/)
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Address {
    pub credential: Credential,
    pub staking_credential: Option<StakingCredential>,
}

/// A public key hash or validator hash credential (used as a payment or a staking credential)
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Credential {
    PubKey(Ed25519PubKeyHash),
    Script(ValidatorHash),
}

/// Credential (public key hash or pointer) used for staking
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum StakingCredential {
    Hash(Credential),
    Pointer(ChainPointer),
}

/// In an address, a chain pointer refers to a point of the chain containing a stake key
/// registration certificate. A point is identified by 3 coordinates:
/// - An absolute slot number
/// - A transaction inder (within that slot)
/// - A (delegation) certificate index (within that transacton)
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ChainPointer {
    pub slot: Slot,
    pub tx_ix: TransactionIndex,
    pub cert_ix: CertificateIndex,
}
/// Position of the certificate in a certain transaction
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CertificateIndex(pub BigInt);

/// Position of a transaction in a given slot
/// This is not identical to the index of a `TransactionInput`
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct TransactionIndex(pub u32);
