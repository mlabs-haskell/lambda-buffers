#[cfg(test)]
mod csl_pla_roundtrip_tests {
    use cardano_serialization_lib as csl;
    use num_bigint::BigInt;
    use plutus_ledger_api::{
        csl::{
            csl_to_pla::{FromCSL, TryFromCSL},
            pla_to_csl::TryToCSL,
        },
        generators::correct::v1::{
            arb_address, arb_certificate_index, arb_chain_pointer, arb_credential, arb_datum_hash,
            arb_ed25519_pub_key_hash, arb_minting_policy_hash, arb_plutus_data, arb_script_hash,
            arb_slot, arb_token_name, arb_transaction_hash, arb_transaction_index,
            arb_transaction_input, arb_value,
        },
        v1::{
            address::{Address, StakingCredential},
            value::Value,
        },
    };
    use proptest::{prop_assert_eq, proptest, strategy::Strategy, test_runner::TestCaseError};

    fn try_to_from_prop<B, A: TryToCSL<B> + FromCSL<B> + PartialEq + std::fmt::Debug>(
        v: A,
    ) -> Result<(), TestCaseError> {
        Ok(prop_assert_eq!(
            A::from_csl(&<A as TryToCSL<B>>::try_to_csl(&v)?),
            v
        ))
    }

    fn try_to_try_from_prop<B, A: TryToCSL<B> + TryFromCSL<B> + PartialEq + std::fmt::Debug>(
        v: A,
    ) -> Result<(), TestCaseError> {
        Ok(prop_assert_eq!(
            A::try_from_csl(&<A as TryToCSL<B>>::try_to_csl(&v)?)?,
            v
        ))
    }

    proptest! {
      #[test]
      fn test_token_name(val in arb_token_name()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_minting_policy_hash(val in arb_minting_policy_hash()) {
          try_to_from_prop(val)?
      }

      // This is special because the CSL machinery always puts in at least a zero Ada in the value
      // But the arbitrary generated value by PLA does not.
      #[test]
      fn test_value(val in arb_value()) {
        let csl_val: csl::Value  = val.try_to_csl()?;
        prop_assert_eq!(
          Value::from_csl(&csl_val),
          // Add a zero ada value.
          Value::ada_value(&BigInt::from(0)) + val
        )
      }

      #[test]
      fn test_transaction_hash(val in arb_transaction_hash()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_transaction_input(val in arb_transaction_input()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_ed25519_pub_key_hash(val in arb_ed25519_pub_key_hash()) {
          try_to_from_prop::<csl::Ed25519KeyHash, _>(val)?
      }

      #[test]
      fn test_script_hash(val in arb_script_hash()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_staking_credential(val in arb_credential().prop_map(StakingCredential::Hash)) {
          try_to_from_prop::<csl::Credential, StakingCredential>(val)?
      }

      #[test]
      fn test_credential(val in arb_credential()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_slot(val in arb_slot()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_transaction_index(val in arb_transaction_index()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_certificate_index(val in arb_certificate_index()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_chain_pointer(val in arb_chain_pointer()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_plutus_data(val in arb_plutus_data()) {
          try_to_try_from_prop(val)?
      }

      #[test]
      fn test_datum_hash(val in arb_datum_hash()) {
          try_to_from_prop(val)?
      }

      #[test]
      fn test_address(val in arb_address()) {
        prop_assert_eq!(
            Address::try_from_csl(&val.with_extra_info(1).try_to_csl()?)?,
            val
        )
      }
    }
}
