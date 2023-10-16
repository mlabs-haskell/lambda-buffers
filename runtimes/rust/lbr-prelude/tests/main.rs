#[cfg(test)]
mod tests {
    mod json_roundtrip_tests {
        use lbr_prelude::error::Error;
        use lbr_prelude::generators::correct::{
            arb_bool, arb_bytes, arb_char, arb_complicated, arb_integer, arb_text,
        };
        use lbr_prelude::json::Json;
        use proptest::collection::{hash_map, hash_set, vec};
        use proptest::option;
        use proptest::prelude::*;
        use proptest::result::maybe_err;

        fn from_to_json<T>(val: &T) -> Result<T, Error>
        where
            T: Json + PartialEq,
        {
            T::from_json(val.to_json()?)
        }

        proptest! {
            #[test]
            fn test_integer(val in arb_integer()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_bool(val in arb_bool() ) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_char(val in arb_char() ) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_bytes(val in arb_bytes() ) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_text(val in arb_text() ) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_maybe(val in option::of(arb_integer())) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_result(val in maybe_err(arb_bool(), arb_integer())) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_vec(val in vec(arb_integer(), 20)) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_set(val in hash_set(arb_integer(), 20)) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_map(val in hash_map(arb_integer(), arb_text(), 20)) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_complicated(val in arb_complicated()) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }
    }
}
