#[cfg(test)]
#[cfg(feature = "plutus")]
mod pla_roundtrip_tests {
    use lbr_prelude::json::{Error, Json};
    fn from_to_json<T>(val: &T) -> Result<T, Error>
    where
        T: Json + PartialEq,
    {
        T::from_json(&val.to_json())
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
}
