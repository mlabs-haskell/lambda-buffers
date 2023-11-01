use lbr_prelude::json::Json;
use lbr_prelude_derive::Json;
use num_bigint::BigInt;
use serde_json::{Number, Value};

#[derive(Json)]
pub struct Person {
    name: String,
    age: BigInt,
}

fn main() {
    let data = Person {
        name: "田中太郎".to_owned(),
        age: BigInt::from(89),
    };

    let expected = Value::Object(serde_json::Map::from_iter([
        ("name".to_owned(), Value::String("田中太郎".to_owned())),
        ("age".to_owned(), Value::Number(Number::from(89))),
    ]));

    let actual = data.to_json().unwrap();

    assert_eq!(actual, expected);
}
