#[cfg(test)]
mod goldens;
mod utils;

mod json_tests {
    use crate::goldens;
    use crate::utils;
    use plutus_ledger_api::json::Json;

    fn from_to_golden_test<T: Json + std::fmt::Debug + Eq>(title: &str, goldens: Vec<T>) {
        let golden_dir = "data/lbt-plutus-golden-data";
        utils::assert_goldens(
            golden_dir,
            title,
            ".json",
            |golden, _index, _fp, text| {
                let res = T::from_json_string(text).unwrap();
                pretty_assertions::assert_eq!(golden, &res);
                pretty_assertions::assert_eq!(golden.to_json_string(), text);
            },
            goldens,
        )
    }

    #[test]
    fn plutus_data_from_to_golden_test() {
        from_to_golden_test("PlutusV1.PlutusData", goldens::plutus_data_goldens())
    }

    #[test]
    fn address_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Address", goldens::address_goldens())
    }

    #[test]
    fn credential_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Credential", goldens::credential_goldens())
    }

    #[test]
    fn staking_credential_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV1.StakingCredential",
            goldens::staking_credential_goldens(),
        )
    }

    #[test]
    fn pubkeyhash_from_to_golden_test() {
        from_to_golden_test("PlutusV1.PubKeyHash", goldens::pubkeyhash_goldens())
    }

    #[test]
    fn bytes_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Bytes", goldens::bytes_goldens())
    }

    #[test]
    fn interval_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Interval", goldens::interval_goldens())
    }

    #[test]
    fn extended_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Extended", goldens::extended_goldens())
    }

    #[test]
    fn lower_bound_from_to_golden_test() {
        from_to_golden_test("PlutusV1.LowerBound", goldens::lower_bound_goldens())
    }

    #[test]
    fn upper_bound_from_to_golden_test() {
        from_to_golden_test("PlutusV1.UpperBound", goldens::upper_bound_goldens())
    }

    #[test]
    fn posix_time_from_to_golden_test() {
        from_to_golden_test("PlutusV1.POSIXTime", goldens::posix_time_goldens())
    }

    #[test]
    fn posix_time_range_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV1.POSIXTimeRange",
            goldens::posix_time_range_goldens(),
        )
    }
    #[test]
    fn ada_currency_symbol_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV1.CurrencySymbol",
            [
                vec![goldens::ada_currency_symbol_golden()],
                goldens::currency_symbol_goldens(),
            ]
            .concat(),
        )
    }

    #[test]
    fn token_name_from_to_golden_test() {
        from_to_golden_test("PlutusV1.TokenName", goldens::token_name_goldens())
    }

    #[test]
    fn asset_class_from_to_golden_test() {
        from_to_golden_test("PlutusV1.AssetClass", goldens::asset_class_goldens())
    }

    #[test]
    fn value_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Value", goldens::value_goldens())
    }

    #[test]
    fn redeemer_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Redeemer", goldens::redeemer_goldens())
    }

    #[test]
    fn datum_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Datum", goldens::datum_goldens())
    }

    #[test]
    fn redeemer_hash_from_to_golden_test() {
        from_to_golden_test("PlutusV1.RedeemerHash", goldens::redeemer_hash_goldens())
    }

    #[test]
    fn datum_hash_from_to_golden_test() {
        from_to_golden_test("PlutusV1.DatumHash", goldens::datum_hash_goldens())
    }

    #[test]
    fn script_hash_from_to_golden_test() {
        from_to_golden_test("PlutusV1.ScriptHash", goldens::script_hash_goldens())
    }

    #[test]
    fn tx_id_from_to_golden_test() {
        from_to_golden_test("PlutusV1.TxId", goldens::tx_id_goldens())
    }

    #[test]
    fn tx_out_ref_from_to_golden_test() {
        from_to_golden_test("PlutusV1.TxOutRef", goldens::tx_out_ref_goldens())
    }

    #[test]
    fn map_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Map", goldens::map_goldens())
    }

    #[test]
    fn tx_in_info_v1_from_to_golden_test() {
        from_to_golden_test("PlutusV1.TxInInfo", goldens::tx_in_info_goldens_v1())
    }

    #[test]
    fn tx_out_v1_from_to_golden_test() {
        from_to_golden_test("PlutusV1.TxOut", goldens::tx_out_goldens_v1())
    }

    #[test]
    fn dcert_v1_from_to_golden_test() {
        from_to_golden_test("PlutusV1.DCert", goldens::dcert_goldens())
    }

    #[test]
    fn script_purpose_v1_from_to_golden_test() {
        from_to_golden_test("PlutusV1.ScriptPurpose", goldens::script_purpose_goldens())
    }

    #[test]
    fn tx_info_v1_from_to_golden_test() {
        from_to_golden_test("PlutusV1.TxInfo", goldens::tx_info_goldens_v1())
    }

    #[test]
    fn script_context_v1_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV1.ScriptContext",
            goldens::script_context_goldens_v1(),
        )
    }

    #[test]
    fn tx_in_info_v2_from_to_golden_test() {
        from_to_golden_test("PlutusV2.TxInInfo", goldens::tx_in_info_goldens_v2())
    }

    #[test]
    fn out_datum_from_to_golden_test() {
        from_to_golden_test("PlutusV2.OutputDatum", goldens::out_datum_goldens())
    }

    #[test]
    fn tx_out_v2_from_to_golden_test() {
        from_to_golden_test("PlutusV2.TxOut", goldens::tx_out_goldens_v2())
    }

    #[test]
    fn tx_info_v2_from_to_golden_test() {
        from_to_golden_test("PlutusV2.TxInfo", goldens::tx_info_goldens_v2())
    }

    #[test]
    fn script_context_v2_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV2.ScriptContext",
            goldens::script_context_goldens_v2(),
        )
    }

    #[test]
    fn rational_from_to_golden_test() {
        from_to_golden_test("PlutusV3.Rational", goldens::rational_goldens())
    }

    #[test]
    fn tx_id_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.TxId", goldens::tx_id_goldens_v3())
    }

    #[test]
    fn tx_out_ref_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.TxOutRef", goldens::tx_out_ref_goldens_v3())
    }

    #[test]
    fn cold_committee_credential_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.ColdCommitteeCredential",
            goldens::cold_committee_credential_goldens_v3(),
        )
    }

    #[test]
    fn hot_committee_credential_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.HotCommitteeCredential",
            goldens::hot_committee_credential_goldens_v3(),
        )
    }

    #[test]
    fn drep_credential_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.DRepCredential",
            goldens::drep_credential_goldens_v3(),
        )
    }

    #[test]
    fn drep_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.DRep", goldens::drep_goldens_v3())
    }

    #[test]
    fn delegatee_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.Delegatee", goldens::delegatee_goldens_v3())
    }

    #[test]
    fn lovelace_from_to_golden_test() {
        from_to_golden_test("PlutusV1.Lovelace", goldens::lovelace_goldens_v3())
    }

    #[test]
    fn tx_cert_from_to_golden_test() {
        from_to_golden_test("PlutusV3.TxCert", goldens::tx_cert_goldens_v3())
    }

    #[test]
    fn voter_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.Voter", goldens::voter_goldens_v3())
    }

    #[test]
    fn vote_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.Vote", goldens::vote_goldens_v3())
    }

    #[test]
    fn governance_action_id_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.GovernanceActionId",
            goldens::governance_action_id_goldens_v3(),
        )
    }

    #[test]
    fn committee_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.Committee", goldens::committee_goldens_v3())
    }

    #[test]
    fn constitution_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.Constitution", goldens::constitution_goldens_v3())
    }

    #[test]
    fn protocol_version_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.ProtocolVersion",
            goldens::protocol_vertion_goldens_v3(),
        )
    }

    #[test]
    fn changed_parameters_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.ChangedParameters",
            goldens::changed_parameters_goldens_v3(),
        )
    }

    #[test]
    fn governance_action_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.GovernanceAction",
            goldens::governance_action_goldens_v3(),
        )
    }

    #[test]
    fn proposal_procedure_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.ProposalProcedure",
            goldens::proposal_procedure_goldens_v3(),
        )
    }

    #[test]
    fn script_purpose_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.ScriptPurpose",
            goldens::script_purpose_goldens_v3(),
        )
    }

    #[test]
    fn script_info_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.ScriptInfo", goldens::script_info_goldens_v3())
    }

    #[test]
    fn tx_in_info_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.TxInInfo", goldens::tx_in_info_goldens_v3())
    }

    #[test]
    fn tx_info_v3_from_to_golden_test() {
        from_to_golden_test("PlutusV3.TxInfo", goldens::tx_info_goldens_v3())
    }

    #[test]
    fn script_context_v3_from_to_golden_test() {
        from_to_golden_test(
            "PlutusV3.ScriptContext",
            goldens::script_context_goldens_v3(),
        )
    }
}

mod plutus_data_tests {
    use crate::utils;
    use plutus_ledger_api::json::Json;
    use plutus_ledger_api::plutus_data::{IsPlutusData, PlutusData};

    fn from_to_golden_test<T: IsPlutusData + std::fmt::Debug + Eq>(title: &str, goldens: Vec<T>) {
        let golden_dir = "data/lbt-plutus-golden-data";
        utils::assert_goldens(
            golden_dir,
            title,
            ".pd.json",
            |golden, _index, _fp, text| {
                let pd = PlutusData::from_json_string(text).unwrap();
                let res = T::from_plutus_data(&pd).unwrap();

                assert_eq!(golden, &res);
                assert_eq!(golden.to_plutus_data().to_json_string(), text);
            },
            goldens,
        )
    }

    mod foo_tests {
        use super::from_to_golden_test;
        use crate::goldens;

        #[test]
        fn foo_a_from_to_golden_tests() {
            from_to_golden_test("Foo.A", goldens::a_goldens())
        }

        #[test]
        fn foo_b_from_to_golden_tests() {
            from_to_golden_test("Foo.B", goldens::b_goldens())
        }

        #[test]
        fn foo_c_from_to_golden_tests() {
            from_to_golden_test("Foo.C", goldens::c_goldens())
        }

        #[test]
        fn foo_d_from_to_golden_tests() {
            from_to_golden_test("Foo.D", goldens::d_goldens())
        }

        #[test]
        fn foo_f_int_from_to_golden_tests() {
            from_to_golden_test("Foo.FInt", goldens::f_int_goldens())
        }

        #[test]
        fn foo_g_int_from_to_golden_tests() {
            from_to_golden_test("Foo.GInt", goldens::g_int_goldens())
        }
    }

    mod days_tests {
        use super::from_to_golden_test;
        use crate::goldens;

        #[test]
        fn days_from_to_golden_test() {
            from_to_golden_test("Days.Day", goldens::day_goldens())
        }

        #[test]
        fn workdays_from_to_golden_test() {
            from_to_golden_test("Days.WorkDay", goldens::workday_goldens())
        }

        #[test]
        fn freedays_from_to_golden_test() {
            from_to_golden_test("Days.FreeDay", goldens::freeday_goldens())
        }
    }

    mod plutus_tests {
        use super::from_to_golden_test;
        use crate::goldens;

        #[test]
        fn plutus_data_from_to_golden_test() {
            from_to_golden_test("PlutusV1.PlutusData", goldens::plutus_data_goldens())
        }

        #[test]
        fn address_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Address", goldens::address_goldens())
        }

        #[test]
        fn credential_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Credential", goldens::credential_goldens())
        }

        #[test]
        fn staking_credential_from_to_golden_test() {
            from_to_golden_test(
                "PlutusV1.StakingCredential",
                goldens::staking_credential_goldens(),
            )
        }

        #[test]
        fn pubkeyhash_from_to_golden_test() {
            from_to_golden_test("PlutusV1.PubKeyHash", goldens::pubkeyhash_goldens())
        }

        #[test]
        fn bytes_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Bytes", goldens::bytes_goldens())
        }

        #[test]
        fn interval_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Interval", goldens::interval_goldens())
        }

        #[test]
        fn extended_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Extended", goldens::extended_goldens())
        }

        #[test]
        fn lower_bound_from_to_golden_test() {
            from_to_golden_test("PlutusV1.LowerBound", goldens::lower_bound_goldens())
        }

        #[test]
        fn upper_bound_from_to_golden_test() {
            from_to_golden_test("PlutusV1.UpperBound", goldens::upper_bound_goldens())
        }

        #[test]
        fn posix_time_from_to_golden_test() {
            from_to_golden_test("PlutusV1.POSIXTime", goldens::posix_time_goldens())
        }

        #[test]
        fn posix_time_range_from_to_golden_test() {
            from_to_golden_test(
                "PlutusV1.POSIXTimeRange",
                goldens::posix_time_range_goldens(),
            )
        }
        #[test]
        fn ada_currency_symbol_from_to_golden_test() {
            from_to_golden_test(
                "PlutusV1.CurrencySymbol",
                [
                    vec![goldens::ada_currency_symbol_golden()],
                    goldens::currency_symbol_goldens(),
                ]
                .concat(),
            )
        }

        #[test]
        fn token_name_from_to_golden_test() {
            from_to_golden_test("PlutusV1.TokenName", goldens::token_name_goldens())
        }

        #[test]
        fn asset_class_from_to_golden_test() {
            from_to_golden_test("PlutusV1.AssetClass", goldens::asset_class_goldens())
        }

        #[test]
        fn value_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Value", goldens::value_goldens())
        }

        #[test]
        fn redeemer_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Redeemer", goldens::redeemer_goldens())
        }

        #[test]
        fn datum_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Datum", goldens::datum_goldens())
        }

        #[test]
        fn redeemer_hash_from_to_golden_test() {
            from_to_golden_test("PlutusV1.RedeemerHash", goldens::redeemer_hash_goldens())
        }

        #[test]
        fn datum_hash_from_to_golden_test() {
            from_to_golden_test("PlutusV1.DatumHash", goldens::datum_hash_goldens())
        }

        #[test]
        fn script_hash_from_to_golden_test() {
            from_to_golden_test("PlutusV1.ScriptHash", goldens::script_hash_goldens())
        }

        #[test]
        fn tx_id_from_to_golden_test() {
            from_to_golden_test("PlutusV1.TxId", goldens::tx_id_goldens())
        }

        #[test]
        fn tx_out_ref_from_to_golden_test() {
            from_to_golden_test("PlutusV1.TxOutRef", goldens::tx_out_ref_goldens())
        }

        #[test]
        fn map_from_to_golden_test() {
            from_to_golden_test("PlutusV1.Map", goldens::map_goldens())
        }

        #[test]
        fn tx_in_info_v1_from_to_golden_test() {
            from_to_golden_test("PlutusV1.TxInInfo", goldens::tx_in_info_goldens_v1())
        }

        #[test]
        fn tx_out_v1_from_to_golden_test() {
            from_to_golden_test("PlutusV1.TxOut", goldens::tx_out_goldens_v1())
        }

        #[test]
        fn dcert_v1_from_to_golden_test() {
            from_to_golden_test("PlutusV1.DCert", goldens::dcert_goldens())
        }

        #[test]
        fn script_purpose_v1_from_to_golden_test() {
            from_to_golden_test("PlutusV1.ScriptPurpose", goldens::script_purpose_goldens())
        }

        #[test]
        fn tx_info_v1_from_to_golden_test() {
            from_to_golden_test("PlutusV1.TxInfo", goldens::tx_info_goldens_v1())
        }

        #[test]
        fn script_context_v1_from_to_golden_test() {
            from_to_golden_test(
                "PlutusV1.ScriptContext",
                goldens::script_context_goldens_v1(),
            )
        }

        #[test]
        fn tx_in_info_v2_from_to_golden_test() {
            from_to_golden_test("PlutusV2.TxInInfo", goldens::tx_in_info_goldens_v2())
        }

        #[test]
        fn out_datum_from_to_golden_test() {
            from_to_golden_test("PlutusV2.OutputDatum", goldens::out_datum_goldens())
        }

        #[test]
        fn tx_out_v2_from_to_golden_test() {
            from_to_golden_test("PlutusV2.TxOut", goldens::tx_out_goldens_v2())
        }

        #[test]
        fn tx_info_v2_from_to_golden_test() {
            from_to_golden_test("PlutusV2.TxInfo", goldens::tx_info_goldens_v2())
        }

        #[test]
        fn script_context_v2_from_to_golden_test() {
            from_to_golden_test(
                "PlutusV2.ScriptContext",
                goldens::script_context_goldens_v2(),
            )
        }
    }

    mod prelude_tests {
        use super::from_to_golden_test;
        use crate::goldens;

        #[test]
        fn bool_from_to_golden_test() {
            from_to_golden_test("Prelude.Bool", goldens::bool_goldens())
        }

        #[test]
        fn maybe_from_to_golden_test() {
            from_to_golden_test("Prelude.Maybe", goldens::maybe_goldens())
        }

        #[test]
        fn either_from_to_golden_test() {
            from_to_golden_test("Prelude.Either", goldens::either_goldens())
        }

        #[test]
        fn list_from_to_golden_test() {
            from_to_golden_test("Prelude.List", goldens::list_goldens())
        }
    }
}
