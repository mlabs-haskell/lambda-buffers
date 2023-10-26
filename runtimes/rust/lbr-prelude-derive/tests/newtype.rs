use lbr_prelude::json::Json;
use lbr_prelude_derive::Json;

#[derive(Json)]
pub struct Newtype(pub String);

fn main() {
    let data = String::from("なんてこった");
    let newtype = Newtype(data.clone());

    let data_json = data.to_json().unwrap();
    let newtype_json = newtype.to_json().unwrap();

    assert_eq!(data_json, newtype_json);
}
