#[cfg(test)]
mod display_serialisation_tests {
    mod golden_v1 {
        use plutus_ledger_api::{
            goldens::v1::{
                sample_address, sample_asset_class, sample_currency_symbol,
                sample_transaction_input, sample_value,
            },
            v1::value::CurrencySymbol,
            v3::value::TokenName,
        };

        #[test]
        fn v1_currency_symbol_display_1() {
            goldie::assert!(format!("{}", sample_currency_symbol()))
        }

        #[test]
        fn v1_currency_symbol_display_2() {
            goldie::assert!(format!("{}", CurrencySymbol::Ada))
        }

        #[test]
        fn v1_currency_symbol_display_3() {
            goldie::assert!(format!("{:#}", CurrencySymbol::Ada))
        }

        #[test]
        fn v1_token_name_display_1() {
            goldie::assert!(format!(
                "{}",
                TokenName::from_bytes(vec![255, 244, 233, 222]).unwrap()
            ))
        }

        #[test]
        fn v1_token_name_display_2() {
            goldie::assert!(format!("{}", TokenName::from_string("TestToken").unwrap()))
        }

        #[test]
        fn v1_token_name_display_3() {
            goldie::assert!(format!(
                "{:#}",
                TokenName::from_bytes(vec![255, 244, 233, 222]).unwrap()
            ))
        }

        #[test]
        fn v1_token_name_display_4() {
            goldie::assert!(format!(
                "{:#}",
                TokenName::from_string("TestToken").unwrap()
            ))
        }

        #[test]
        fn v1_asset_class_display_1() {
            goldie::assert!(format!("{}", sample_asset_class()))
        }

        #[test]
        fn v1_asset_class_display_2() {
            goldie::assert!(format!("{:#}", sample_asset_class()))
        }

        #[test]
        fn v1_value_display_1() {
            goldie::assert!(format!("{}", sample_value()))
        }

        #[test]
        fn v1_value_display_2() {
            goldie::assert!(format!("{:#}", sample_value()))
        }

        #[test]
        fn v1_address_display_1() {
            goldie::assert!(format!("{}", sample_address().with_extra_info(0)))
        }

        #[test]
        fn v1_address_display_2() {
            goldie::assert!(format!("{}", sample_address().with_extra_info(1)))
        }

        #[test]
        fn v1_transaction_input_display() {
            goldie::assert!(format!("{}", sample_transaction_input()))
        }
    }

    mod props_v1 {
        use std::{fmt::Display, str::FromStr};

        use plutus_ledger_api::{
            generators::correct::v1::{
                arb_address, arb_asset_class, arb_currency_symbol, arb_transaction_input, arb_value,
            },
            v1::{address::Address, value::TokenName},
        };
        use proptest::{prelude::*, string::string_regex};

        fn from_to_string<T>(val: &T) -> Result<T, T::Err>
        where
            T: FromStr + Display + PartialEq,
        {
            T::from_str(&val.to_string())
        }

        proptest! {

            #[test]
            fn currency_symbol(val in arb_currency_symbol()) {
                assert_eq!(val, from_to_string(&val)?);
            }

            #[test]
            fn token_name(val in string_regex("[a-zA-Z0-9]{0,32}").unwrap()) {
                let token_name = TokenName::from_string(&val).unwrap();

                assert_eq!(token_name.try_into_string().unwrap(), val);
            }

            #[test]
            fn asset_class(val in arb_asset_class()) {
                assert_eq!(val, from_to_string(&val)?);
            }

            #[test]
            fn value(val in arb_value()) {
                assert_eq!(val, from_to_string(&val)?);
            }

            #[test]
            fn address(val in arb_address()) {
                let roundtripped = Address::from_str(&val.with_extra_info(0).to_string()).unwrap();

                assert_eq!(val, roundtripped);
            }

            #[test]
            fn transaction_input(val in arb_transaction_input()) {
                assert_eq!(val, from_to_string(&val)?);
            }
        }
    }
}
