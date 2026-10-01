#[cfg(test)]
mod value_tests {
    mod ring_ish {
        use std::ops::Neg;

        use num_bigint::BigInt;
        use num_traits::{One, Zero};
        use plutus_ledger_api::{
            generators::correct::{primitive::arb_integer, v1::arb_value},
            v1::value::Value,
        };
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn test_additive_left_identity(val in arb_value()) {
              assert_eq!(&Value::zero() + &val, val);
            }

            #[test]
            fn test_additive_right_identity(val in arb_value()) {
              assert_eq!(&val + &Value::zero(), val);
            }

            #[test]
            fn test_additive_commutative(x in arb_value(), y in arb_value()) {
              assert_eq!(&x + &y, &y + &x);
            }

            #[test]
            fn test_additive_associativity(x in arb_value(), y in arb_value(), z in arb_value()) {
              assert_eq!(&x + (&y + &z), (&z + &y) + &x);
            }

            #[test]
            fn test_scalar_multiplicative_left_identity(val in arb_value()) {
              assert_eq!(BigInt::one() * &val, val);
            }

            #[test]
            fn test_scalar_multiplicative_right_identity(val in arb_value()) {
              assert_eq!(&val * BigInt::one(), val);
            }

            #[test]
            fn test_scalar_left_distributivity_mul_over_add(x in arb_integer(), y in arb_value(), z in arb_value()) {
              assert_eq!(&x * (&y + &z), (&x * &y) + (&x * &z));
            }

            #[test]
            fn test_scalar_right_distributivity_mul_over_add(x in arb_value(), y in arb_value(), z in arb_integer()) {
              assert_eq!((&x + &y) * &z, (&x * &z) + (&y * &z));
            }

            #[test]
            fn test_scalar_multiplicative_associativity(x in arb_value(), y in arb_integer(), z in arb_integer()) {
              assert_eq!(&x * (&y * &z), (&z * &y) * &x);
            }

            #[test]
            fn test_scalar_annihilation(val in arb_value()) {
              assert_eq!(&val * BigInt::zero(), BigInt::zero() * &val);
              assert_eq!(Value::zero(), (BigInt::zero() * &val).normalize());
            }

            #[test]
            fn test_additive_inverse_annihilation(val in arb_value()) {
              assert_eq!((&val + (&val).neg()).normalize(), Value::zero());
            }

            #[test]
            fn test_minus(x in arb_value(), y in arb_value()) {
              assert_eq!(&x - &y, &x + (&y).neg());
            }
        }
    }

    mod other_utils {
        use plutus_ledger_api::generators::correct::{
            primitive::arb_integer,
            v1::{arb_currency_symbol, arb_token_name, arb_value},
        };
        use plutus_ledger_api::v1::value::Value;
        use proptest::prelude::*;

        proptest! {
          #[test]
          fn test_token_value_amount_roundtrip(cs in arb_currency_symbol(), tn in arb_token_name(), amount in arb_integer()){
            assert_eq!(Value::token_value(&cs, &tn, &amount).get_token_amount(&cs, &tn), amount);
          }


          #[test]
          fn test_insert_token_amount_roundtrip(val in arb_value(), cs in arb_currency_symbol(), tn in arb_token_name(), amount in arb_integer()){
            assert_eq!(val.insert_token(&cs, &tn, &amount).get_token_amount(&cs, &tn), amount);
          }
        }
    }
}
