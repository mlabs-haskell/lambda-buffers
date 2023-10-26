#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// ED25519 public key hash
/// This is the standard cryptography in Cardano, commonly referred to as `PubKeyHash` in Plutus
/// and other libraries
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Ed25519PubKeyHash(pub Vec<u8>);

/// Standard public key hash used to verify a transaction witness
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PaymentPubKeyHash(pub Ed25519PubKeyHash);

/// Standard public key hash used to verify a staking
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct StakePubKeyHash(pub Ed25519PubKeyHash);
