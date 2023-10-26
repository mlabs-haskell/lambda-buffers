extern crate proc_macro;
use crate::json::Json;
use proc_macro::TokenStream;

#[proc_macro_derive(Json)]
pub fn derive_answer_fn(_item: TokenStream) -> TokenStream {
    "impl Json for {item} {
        fn to_json(&self) -> Result<serde_json::Value, Error> {
            self.0.to_json()
        }

        fn from_json(value: serde_json::Value) -> Result<Self, Error> {
            Ok(Self(Json::from_json(value)?))
        }
    }"
    .parse()
    .unwrap()
}
