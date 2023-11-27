#[cfg(test)]
mod tests {
    mod json_roundtrip_tests {
        use lbr_prelude::error::Error;
        use lbr_prelude::generators::correct::{
            arb_bool, arb_bytes, arb_char, arb_complicated, arb_integer, arb_text,
        };
        use lbr_prelude::json::Json;
        use proptest::collection::{btree_map, btree_set, vec};
        use proptest::option;
        use proptest::prelude::*;
        use proptest::result::maybe_err;

        fn from_to_json<T>(val: &T) -> Result<T, Error>
        where
            T: Json + PartialEq,
        {
            T::from_json(&val.to_json()?)
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
            fn test_set(val in btree_set(arb_integer(), 20)) {
                assert_eq!(val, from_to_json(&val)?);
            }
        }

        proptest! {
            #[test]
            fn test_map(val in btree_map(arb_integer(), arb_text(), 20)) {
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

    mod lamval_builtins {
        use lbr_prelude::json::lamval;
        use lbr_prelude::json::{self, Json};
        use serde_json::Value;

        #[test]
        fn test_json_array() {
            let result = json::json_array(vec![
                Value::String("a".to_owned()),
                Value::String("b".to_owned()),
                Value::String("c".to_owned()),
            ]);
            let expected = Value::Array(vec![
                Value::String("a".to_owned()),
                Value::String("b".to_owned()),
                Value::String("c".to_owned()),
            ]);
            assert_eq!(result, expected);
        }

        #[test]
        fn test_case_json_array() {
            let x0 = "Test";
            let x1 = Box::new(|vals: &Vec<Value>| vals.iter().map(String::from_json).collect());
            let x2 = Value::Array(vec![
                Value::String("a".to_owned()),
                Value::String("b".to_owned()),
                Value::String("c".to_owned()),
            ]);

            let result: Vec<String> = json::case_json_array(&x0, x1.clone(), &x2).unwrap();
            let lamval_result: Vec<String> = lamval::case_json_array(&x0)(x1)(&x2).unwrap();
            let expected = vec!["a".to_owned(), "b".to_owned(), "c".to_owned()];

            assert_eq!(result, expected);
            assert_eq!(lamval_result, expected);
        }
    }
}
