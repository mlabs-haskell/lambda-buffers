use lbr_prelude::json::Json;
use lbr_prelude_derive::Json;
use num_bigint::BigInt;
use serde_json::{Number, Value};

#[derive(Json)]
pub struct MyTuple(pub String, pub BigInt);

fn main() {
    let data = MyTuple(String::from("Cicakutya"), BigInt::from(15));

    let expected = Value::Array(vec![
        Value::String(String::from("Cicakutya")),
        Value::Number(Number::from(15)),
    ]);

    let actual = data.to_json();

    assert_eq!(actual, expected);
}
