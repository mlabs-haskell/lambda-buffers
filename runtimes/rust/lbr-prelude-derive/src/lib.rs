use proc_macro::TokenStream;
use quote::quote;
use syn::{self, parse_macro_input, DeriveInput};

/// Derive a `Json` trait implementation
#[proc_macro_derive(Json)]
pub fn derive_json_fn(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;

    let expanded = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => impl_struct(ident, fields_named),
            syn::Fields::Unnamed(fields_unnamed) => {
                if fields_unnamed.unnamed.len() == 1 {
                    impl_newtype(ident)
                } else {
                    impl_tuple(ident, fields_unnamed)
                }
            }
            syn::Fields::Unit => unimplemented!("Units are unsupported"),
        },
        syn::Data::Enum(data_enum) => impl_enum(ident, &data_enum.variants),
        syn::Data::Union(_data_union) => unimplemented!("Unions are unsupported"),
    };

    TokenStream::from(expanded)
}

/// Derive `Json` implementations for a struct type
/// All fields must implement the `Json` trait
fn impl_struct(ident: &syn::Ident, fields_named: &syn::FieldsNamed) -> proc_macro2::TokenStream {
    let named = &fields_named.named;

    // Insert keys and values of the JSON object into a dict
    let dict_insert = named.iter().map(|field| {
        let key = &field.ident;
        let key_str = key.as_ref().unwrap().to_string();
        quote! {
            dict.insert(#key_str.to_owned(), self.#key.to_json()?);
        }
    });

    let to_json_impl = quote! {
        fn to_json(&self) -> Result<serde_json::Value, lbr_prelude::error::Error> {
            let mut dict = serde_json::Map::new();
            #(#dict_insert)*

            Ok(serde_json::Value::Object(dict))
        }
    };

    // Get the values from the JSON object
    let dict_get = named.iter().map(|field| {
        let key = &field.ident;
        let key_str = key.as_ref().unwrap().to_string();
        quote! {
            let #key = dict
                .get(#key_str)
                .ok_or(lbr_prelude::error::Error::UnexpectedFieldName {
                    wanted: #key_str.to_owned(),
                    got: dict.keys().cloned().collect(),
                })
                .cloned()
                .and_then(lbr_prelude::json::Json::from_json)?;
        }
    });

    let keys = named.iter().map(|field| &field.ident);

    let from_json_impl = quote! {
        fn from_json(value: serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
            match value {
                serde_json::Value::Object(dict) => {
                    #(#dict_get)*

                    Ok(Self {
                        #(#keys,)*
                    })
                }
                _ => Err(lbr_prelude::error::Error::UnexpectedJsonType {
                    wanted: lbr_prelude::error::JsonType::Object,
                    got: lbr_prelude::error::JsonType::from(&value),
                }),
            }
        }
    };

    quote! {
        impl lbr_prelude::json::Json for #ident {
            #to_json_impl
            #from_json_impl
        }
    }
}

/// Derive `Json` implementations for a tuple struct type
/// All fields must implement the `Json` trait
fn impl_tuple(ident: &syn::Ident, fields_unnamed: &syn::FieldsUnnamed) -> proc_macro2::TokenStream {
    let arity = fields_unnamed.unnamed.len();
    let to_json_indices = (0..arity).map(syn::Index::from);
    let from_json_indices = to_json_indices.clone();
    quote! {
        impl lbr_prelude::json::Json for #ident {
            fn to_json(&self) -> Result<serde_json::Value, lbr_prelude::error::Error> {
                Ok(serde_json::Value::Array(vec![
                    #(self.#to_json_indices.to_json()?,)*
                ]))
            }

            fn from_json(value: serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
                Vec::from_json(value).and_then(|vec: Vec<serde_json::Value>| {
                    if vec.len() == #arity {
                        Ok(Self(
                            #(Json::from_json(vec[#from_json_indices].clone())?,)*
                        ))
                    } else {
                        Err(lbr_prelude::error::Error::UnexpectedArrayLength {
                            wanted: #arity,
                            got: vec.len(),
                        })
                    }
                })
            }
        }
    }
}

/// Derive transparent `Json` implementations for a tuple type, the wrapper will not be present
/// in the serialised format
/// The enclosed field must implement the `Json` trait
fn impl_newtype(ident: &syn::Ident) -> proc_macro2::TokenStream {
    quote! {
        impl lbr_prelude::json::Json for #ident {
            fn to_json(&self) -> Result<serde_json::Value, lbr_prelude::error::Error> {
                self.0.to_json()
            }

            fn from_json(value: serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
                Ok(Self(lbr_prelude::json::Json::from_json(value)?))
            }
        }
    }
}

fn impl_enum(
    _ident: &syn::Ident,
    _variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> proc_macro2::TokenStream {
    unimplemented!("Enums are unsupported")
}
