use lbr_prelude::json::{json_constructor, Json};
use lbr_prelude_derive::Json;
use num_bigint::BigInt;
use serde_json::{Number, Value};

#[derive(Json)]
pub enum MyEnum {
    MyString(String),
    MyCompound(BigInt, String),
    MyNothing,
}

fn main() {
    let data0 = MyEnum::MyString(String::from("Cicakutya"));
    let data1 = MyEnum::MyCompound(BigInt::from(15), String::from("Kutyacica"));
    let data2 = MyEnum::MyNothing;

    let expected0 = json_constructor("MyString", &vec![Value::String(String::from("Cicakutya"))]);
    let expected1 = json_constructor(
        "MyCompound",
        &vec![
            Value::Number(Number::from(15)),
            Value::String(String::from("Kutyacica")),
        ],
    );
    let expected2 = json_constructor("Nothing", &Vec::new());

    let actual0 = data0.to_json();
    let actual1 = data1.to_json();
    let actual2 = data2.to_json();

    assert_eq!(actual0, expected0);
    assert_eq!(actual1, expected1);
    assert_eq!(actual2, expected2);
}
