#[cfg(test)]
mod plutusdata_roundtrip_tests {
    use plutus_ledger_api::plutus_data::{IsPlutusData, PlutusDataError};
    fn from_to_plutus_data<T>(val: &T) -> Result<T, PlutusDataError>
    where
        T: IsPlutusData + PartialEq,
    {
        T::from_plutus_data(&val.to_plutus_data())
    }

    mod golden_primitive {
        use std::collections::{BTreeMap, BTreeSet};

        use num_bigint::BigInt;
        use plutus_ledger_api::plutus_data::IsPlutusData;

        #[test]
        fn integer() {
            goldie::assert_debug!(BigInt::from(123456789).to_plutus_data())
        }

        #[test]
        fn bool() {
            goldie::assert_debug!(true.to_plutus_data())
        }

        #[test]
        fn char() {
            goldie::assert_debug!('凛'.to_plutus_data())
        }

        #[test]
        fn bytes() {
            goldie::assert_debug!([0u8, 1, 2, 3].repeat(10).to_vec().to_plutus_data())
        }

        #[test]
        fn text() {
            goldie::assert_debug!(String::from("Somethingsomething").to_plutus_data())
        }

        #[test]
        fn maybe_some() {
            goldie::assert_debug!(Some(BigInt::from(1234)).to_plutus_data())
        }

        #[test]
        fn maybe_none() {
            goldie::assert_debug!(None::<BigInt>.to_plutus_data())
        }

        #[test]
        fn result_ok() {
            goldie::assert_debug!(Ok::<bool, BigInt>(false).to_plutus_data())
        }

        #[test]
        fn result_err() {
            goldie::assert_debug!(Err::<bool, BigInt>(BigInt::from(1234)).to_plutus_data())
        }

        #[test]
        fn vec() {
            goldie::assert_debug!([0, 1, 2, 3]
                .repeat(20)
                .into_iter()
                .map(BigInt::from)
                .collect::<Vec<BigInt>>()
                .to_plutus_data())
        }

        #[test]
        fn set() {
            goldie::assert_debug!([0, 1, 2]
                .into_iter()
                .map(BigInt::from)
                .collect::<BTreeSet<BigInt>>()
                .to_plutus_data())
        }

        #[test]
        fn map() {
            goldie::assert_debug!([(0, "Hey"), (1, "There"), (2, "Foo"), (3, "Bar")]
                .into_iter()
                .map(|(k, v)| (BigInt::from(k), String::from(v)))
                .collect::<BTreeMap<BigInt, String>>()
                .to_plutus_data())
        }
    }

    mod prop_primitive {
        use super::from_to_plutus_data;
        use plutus_ledger_api::generators::correct::primitive::*;
        use proptest::collection::{btree_map, btree_set, vec};
        use proptest::option;
        use proptest::prelude::*;
        use proptest::result::maybe_err;

        proptest! {
            #[test]
            fn integer(val in arb_integer()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn bool(val in arb_bool() ) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn char(val in arb_char() ) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn bytes(val in arb_bytes() ) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn text(val in arb_text() ) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn maybe(val in option::of(arb_integer())) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn result(val in maybe_err(arb_bool(), arb_integer())) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn vector(val in vec(arb_integer(), 20)) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn set(val in btree_set(arb_integer(), 20)) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn map(val in btree_map(arb_integer(), arb_text(), 20)) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn complicated(val in arb_complicated()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }
        }
    }
    mod golden_v1 {
        use num_bigint::BigInt;
        use plutus_ledger_api::{
            goldens::v1::{
                sample_address, sample_asset_class, sample_datum_hash, sample_dcert,
                sample_payment_pub_key_hash, sample_plutus_interval, sample_redeemer,
                sample_redeemer_hash, sample_script_context, sample_script_purpose,
                sample_transaction_info, sample_transaction_input, sample_transaction_output,
                sample_tx_in_info, sample_value,
            },
            plutus_data::IsPlutusData,
            v1::assoc_map::AssocMap,
        };

        #[test]
        fn v1_asset_class() {
            goldie::assert_debug!(sample_asset_class().to_plutus_data())
        }

        #[test]
        fn v1_value() {
            goldie::assert_debug!(sample_value().to_plutus_data())
        }

        #[test]
        fn v1_plutus_interval() {
            goldie::assert_debug!(sample_plutus_interval().to_plutus_data())
        }

        #[test]
        fn v1_address() {
            goldie::assert_debug!(sample_address().to_plutus_data())
        }

        #[test]
        fn v1_transaction_input() {
            goldie::assert_debug!(sample_transaction_input().to_plutus_data())
        }

        #[test]
        fn v1_transaction_output() {
            goldie::assert_debug!(sample_transaction_output().to_plutus_data())
        }

        #[test]
        fn v1_tx_in_info() {
            goldie::assert_debug!(sample_tx_in_info().to_plutus_data())
        }

        #[test]
        fn v1_redeemeer() {
            goldie::assert_debug!(sample_redeemer().to_plutus_data())
        }

        #[test]
        fn v1_redeemeer_hash() {
            goldie::assert_debug!(sample_redeemer_hash().to_plutus_data())
        }

        #[test]
        fn v1_datum_hash() {
            goldie::assert_debug!(sample_datum_hash().to_plutus_data())
        }

        #[test]
        fn v1_bigint_assoc_map() {
            goldie::assert_debug!(AssocMap::from(
                [(1, 123), (0, 321), (2, 456)]
                    .into_iter()
                    .map(|(k, v)| (BigInt::from(k), BigInt::from(v)))
                    .collect::<Vec<_>>()
            )
            .to_plutus_data())
        }

        #[test]
        fn v1_arb_payment_pub_key_hash() {
            goldie::assert_debug!(sample_payment_pub_key_hash().to_plutus_data())
        }

        #[test]
        fn v1_d_cert() {
            goldie::assert_debug!(sample_dcert().to_plutus_data())
        }

        #[test]
        fn v1_script_purpose() {
            goldie::assert_debug!(sample_script_purpose().to_plutus_data())
        }

        #[test]
        fn v1_transaction_info() {
            goldie::assert_debug!(sample_transaction_info().to_plutus_data())
        }

        #[test]
        fn v1_script_context() {
            goldie::assert_debug!(sample_script_context().to_plutus_data())
        }
    }
    mod prop_v1 {
        use super::from_to_plutus_data;
        use plutus_ledger_api::generators::correct::{primitive::arb_integer, v1::*};
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn v1_asset_class(val in arb_asset_class()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_value(val in arb_value()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_plutus_data(val in arb_plutus_data()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_plutus_interval(val in arb_plutus_interval_posix_time()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_address(val in arb_address()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_transaction_input(val in arb_transaction_input()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_transaction_output(val in arb_transaction_output()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_tx_in_info(val in arb_tx_in_info()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_redeemeer(val in arb_redeemer()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_redeemeer_hash(val in arb_redeemer_hash()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_bigint_assoc_map(val in arb_assoc_map(arb_integer(), arb_integer())) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v1_arb_payment_pub_key_hash(val in arb_payment_pub_key_hash()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }

            #[test]
            fn v1_d_cert(val in arb_d_cert()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }

            #[test]
            fn v1_script_purpose(val in arb_script_purpose()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }

            #[test]
            fn v1_transaction_info(val in arb_transaction_info()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }

            #[test]
            fn v1_script_context(val in arb_script_context()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }


            #[test]
            fn v1_lovelace(val in arb_lovelace()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }
        }
    }
    mod golden_v2 {
        use plutus_ledger_api::{
            goldens::v2::{
                sample_output_datum, sample_script_context, sample_transaction_info,
                sample_transaction_output, sample_tx_in_info,
            },
            plutus_data::IsPlutusData,
        };

        #[test]
        fn v2_transaction_output() {
            goldie::assert_debug!(sample_transaction_output().to_plutus_data())
        }

        #[test]
        fn v2_tx_in_info() {
            goldie::assert_debug!(sample_tx_in_info().to_plutus_data())
        }

        #[test]
        fn v2_output_datum() {
            goldie::assert_debug!(sample_output_datum().to_plutus_data())
        }

        #[test]
        fn v2_transaction_info() {
            goldie::assert_debug!(sample_transaction_info().to_plutus_data())
        }

        #[test]
        fn v2_script_context() {
            goldie::assert_debug!(sample_script_context().to_plutus_data())
        }
    }
    mod prop_v2 {
        use super::from_to_plutus_data;
        use plutus_ledger_api::generators::correct::v2::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn v2_transaction_output(val in arb_transaction_output()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v2_tx_in_info(val in arb_tx_in_info()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v2_output_datum(val in arb_output_datum()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v2_transaction_info(val in arb_transaction_info()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }

            #[test]
            fn v2_script_context(val in arb_script_context()) {
                assert_eq!(val, from_to_plutus_data(&val)?)
            }
        }
    }

    mod prop_v3 {
        use super::from_to_plutus_data;
        use plutus_ledger_api::generators::correct::v3::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn v3_cold_committee_credential(val in arb_cold_committee_credential()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_hot_committee_credential(val in arb_hot_committee_credential()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_d_rep_credential(val in arb_d_rep_credential()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_d_rep(val in arb_d_rep()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_delegatee(val in arb_delegatee()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_tx_cert(val in arb_tx_cert()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_voter(val in arb_voter()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_vote(val in arb_vote()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_governance_action_id(val in arb_governance_action_id()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_committee(val in arb_committee()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_rational(val in arb_rational()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_constitution(val in arb_constitution()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_protocol_version(val in arb_protocol_version()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_changed_parameters(val in arb_changed_parameters()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_governance_action(val in arb_governance_action()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_proposal_procedure(val in arb_proposal_procedure()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_script_purpose(val in arb_script_purpose()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_script_info(val in arb_script_info()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }


            #[test]
            fn v3_transaction_info(val in arb_transaction_info()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }

            #[test]
            fn v3_script_context(val in arb_script_context()) {
                assert_eq!(val, from_to_plutus_data(&val)?);
            }
        }
    }
}
