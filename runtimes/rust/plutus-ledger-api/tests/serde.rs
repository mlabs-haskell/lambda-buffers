#[cfg(test)]
#[cfg(feature = "serde")]
mod serde_roundtrip_tests {
    fn from_to_json<T>(val: &T) -> Result<T, serde_json::Error>
    where
        T: serde::Serialize + for<'a> serde::Deserialize<'a> + PartialEq,
    {
        serde_json::from_str(&serde_json::to_string(&val)?)
    }

    mod v1 {
        use super::from_to_json;
        use plutus_ledger_api::generators::correct::{primitive::arb_integer, v1::*};
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn test_asset_class(val in arb_asset_class()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_value(val in arb_value()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_plutus_data(val in arb_plutus_data()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_plutus_interval(val in arb_plutus_interval_posix_time()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_address(val in arb_address()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_transaction_input(val in arb_transaction_input()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_transaction_output(val in arb_transaction_output()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_tx_in_info(val in arb_tx_in_info()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_redeemeer(val in arb_redeemer()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_redeemeer_hash(val in arb_redeemer_hash()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_bigint_assoc_map(val in arb_assoc_map(arb_integer(), arb_integer())) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_d_cert(val in arb_d_cert()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }

        proptest! {
            #[test]
            fn test_script_purpose(val in arb_script_purpose()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }

        proptest! {
            #[test]
            fn test_transaction_info(val in arb_transaction_info()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }

        proptest! {
            #[test]
            fn test_script_context(val in arb_script_context()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }
    }
    mod v2 {
        use super::from_to_json;
        use plutus_ledger_api::generators::correct::v2::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn test_transaction_output(val in arb_transaction_output()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_tx_in_info(val in arb_tx_in_info()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_output_datum(val in arb_output_datum()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_transaction_info(val in arb_transaction_info()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }

        proptest! {
            #[test]
            fn test_script_context(val in arb_script_context()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }
    }
    mod v3 {
        use super::from_to_json;
        use plutus_ledger_api::generators::correct::v3::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn test_cold_committee_credential(val in arb_cold_committee_credential()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_hot_committee_credential(val in arb_hot_committee_credential()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_d_rep_credential(val in arb_d_rep_credential()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_delegatee(val in arb_delegatee()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_tx_cert(val in arb_tx_cert()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_voter(val in arb_voter()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_vote(val in arb_vote()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_governance_action_id(val in arb_governance_action_id()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_committee(val in arb_committee()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_rational(val in arb_rational()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_constitution(val in arb_constitution()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_protocol_version(val in arb_protocol_version()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_changed_parameters(val in arb_changed_parameters()) {
                assert_eq!(val, from_to_json(&val)?)
            }


            #[test]
            fn test_governance_action(val in arb_governance_action()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_proposal_procedure(val in arb_proposal_procedure()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_script_purpose(val in arb_script_purpose()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_script_info(val in arb_script_info()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_transaction_info(val in arb_transaction_info()) {
                assert_eq!(val, from_to_json(&val)?)
            }

            #[test]
            fn test_script_context(val in arb_script_context()) {
                assert_eq!(val, from_to_json(&val)?)
            }
        }
    }
}
