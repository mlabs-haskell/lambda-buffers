#[cfg(test)]
mod plutusdata_roundtrip_tests {
    use plutus_ledger_types::generators::correct::*;
    use plutus_ledger_types::plutus_data::{FromPlutusData, PlutusDataError, ToPlutusData};
    use proptest::collection::{btree_map, btree_set, vec};
    use proptest::option;
    use proptest::prelude::*;
    use proptest::result::maybe_err;

    fn from_to_plutus_data<T>(val: &T) -> Result<T, PlutusDataError>
    where
        T: ToPlutusData + FromPlutusData + PartialEq,
    {
        T::from_plutus_data(val.to_plutus_data())
    }

    proptest! {
        #[test]
        fn test_integer(val in arb_integer()) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_bool(val in arb_bool() ) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_char(val in arb_char() ) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_bytes(val in arb_bytes() ) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_text(val in arb_text() ) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_maybe(val in option::of(arb_integer())) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_result(val in maybe_err(arb_bool(), arb_integer())) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_vec(val in vec(arb_integer(), 20)) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_set(val in btree_set(arb_integer(), 20)) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_map(val in btree_map(arb_integer(), arb_text(), 20)) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }

    proptest! {
        #[test]
        fn test_complicated(val in arb_complicated()) {
            assert_eq!(val, from_to_plutus_data(&val)?);
        }
    }
}
