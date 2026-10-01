use quote::ToTokens;
use syn::{parse_macro_input, DeriveInput};

pub(crate) mod derive_impl;

#[proc_macro_derive(IsPlutusData, attributes(is_plutus_data_derive_strategy))]
pub fn derive_is_plutus_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_impl::get_is_plutus_data_instance(input)
        .unwrap()
        .into_token_stream()
        .into()
}
