use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{self, parse_macro_input, DeriveInput};

/// Derive a `Json` trait implementation
///
/// Currently we only support a subset of types with an opinionated serialisation scheme. If you
/// need anything else than what this library provides, it is advised to hand write the
/// implementation.
///
/// Supported types:
/// - **unit structs (newtypes)**: this will simply remove the wrapper, and serialize the wrapped value
/// - **structs**: serialized into a Json Object
/// - **enums with unnamed fields**: serialized into an object with the following schema: `{"name": string, "fields": any[]}`
#[proc_macro_derive(Json)]
pub fn derive_json_fn(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let ident = &ast.ident;

    let (to_json_impl, from_json_impl) = match &ast.data {
        syn::Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields_named) => impl_struct(ident, fields_named),
            syn::Fields::Unnamed(fields_unnamed) => {
                if fields_unnamed.unnamed.len() == 1 {
                    impl_newtype()
                } else {
                    impl_tuple(ident, fields_unnamed)
                }
            }
            syn::Fields::Unit => unimplemented!("Units are unsupported"),
        },
        syn::Data::Enum(data_enum) => impl_enum(ident, &data_enum.variants),
        syn::Data::Union(_data_union) => unimplemented!("Unions are unsupported"),
    };

    let (impl_generics, ty_generics, where_clause) = &ast.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics lbr_prelude::json::Json for #ident #ty_generics #where_clause {

            #to_json_impl
            #from_json_impl
        }
    };

    TokenStream::from(expanded)
}

/// Derive `Json` implementations for a struct type
/// All fields must implement the `Json` trait
fn impl_struct(
    ident: &syn::Ident,
    fields_named: &syn::FieldsNamed,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let ident_str = ident.to_string();
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
                    parser: #ident_str.to_owned(),
                })
                .and_then(lbr_prelude::json::Json::from_json)?;
        }
    });

    let keys = named.iter().map(|field| &field.ident);

    let from_json_impl = quote! {
        fn from_json(value: &serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
            match value {
                serde_json::Value::Object(dict) => {
                    #(#dict_get)*

                    Ok(Self {
                        #(#keys,)*
                    })
                }
                _ => Err(lbr_prelude::error::Error::UnexpectedJsonType {
                    wanted: lbr_prelude::error::JsonType::Object,
                    got: lbr_prelude::error::JsonType::from(value),
                    parser: #ident_str.to_owned(),
                }),
            }
        }
    };

    (to_json_impl, from_json_impl)
}

/// Derive `Json` implementations for a tuple struct type
/// All fields must implement the `Json` trait
fn impl_tuple(
    ident: &syn::Ident,
    fields_unnamed: &syn::FieldsUnnamed,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let ident_str = ident.to_string();

    let arity = fields_unnamed.unnamed.len();
    let to_json_indices = (0..arity).map(syn::Index::from);
    let from_json_indices = to_json_indices.clone();

    let to_json_impl = quote! {
        fn to_json(&self) -> Result<serde_json::Value, lbr_prelude::error::Error> {
            Ok(serde_json::Value::Array(vec![
                #(self.#to_json_indices.to_json()?,)*
            ]))
        }
    };

    let from_json_impl = quote! {
        fn from_json(value: &serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
            Vec::from_json(value).and_then(|vec: Vec<serde_json::Value>| {
                if vec.len() == #arity {
                    Ok(Self(
                        #(Json::from_json(&vec[#from_json_indices])?,)*
                    ))
                } else {
                    Err(lbr_prelude::error::Error::UnexpectedArrayLength {
                        wanted: #arity,
                        got: vec.len(),
                    parser: #ident_str.to_owned(),
                    })
                }
            })
        }
    };

    (to_json_impl, from_json_impl)
}

/// Derive transparent `Json` implementations for a tuple type, the wrapper will not be present
/// in the serialised format
/// The enclosed field must implement the `Json` trait
fn impl_newtype() -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let to_json_impl = quote! {
        fn to_json(&self) -> Result<serde_json::Value, lbr_prelude::error::Error> {
            self.0.to_json()
        }
    };

    let from_json_impl = quote! {
        fn from_json(value: &serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
            Ok(Self(lbr_prelude::json::Json::from_json(value)?))
        }
    };

    (to_json_impl, from_json_impl)
}

/// Derive `Json` implementation for an enum type
/// All fields must implement the `Json` trait
fn impl_enum(
    ident: &syn::Ident,
    variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let ident_str = ident.to_string();
    // Arms of the pattern match over the variants of the original data
    let to_json_match_arms = variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        let variant_str = variant.ident.to_string();

        match &variant.fields {
            syn::Fields::Named(_fields_named) => {
                unimplemented!("Enums with named fields are unsupported.")
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                let arity = fields_unnamed.unnamed.len();
                let fields = (0..arity).map(|i| format_ident!("f{}", i));
                let fields_2 = fields.clone();

                quote! {
                    #ident::#variant_ident( #(#fields),* ) =>
                        lbr_prelude::json::json_constructor(#variant_str, &vec![
                           #(#fields_2.to_json()?,)*
                        ]),
                }
            }
            syn::Fields::Unit => quote! {
                #ident::#variant_ident =>
                    lbr_prelude::json::json_constructor(#variant_str, &Vec::with_capacity(0)),
            },
        }
    });

    let to_json_impl = quote! {
        fn to_json(&self) -> Result<serde_json::Value, lbr_prelude::error::Error> {
            Ok(match self {
                #(#to_json_match_arms)*
            })
        }
    };

    // Arms of the pattern match over the JSON name and fields
    let from_json_match_arms = variants.iter().map(|variant| {
        let variant_ident = &variant.ident;
        let variant_str = variant.ident.to_string();

        match &variant.fields {
            syn::Fields::Named(_fields_named) => {
                unimplemented!("Enums with named fields are unsupported.")
            }

            syn::Fields::Unnamed(fields_unnamed) => {
                let arity = fields_unnamed.unnamed.len();
                let fields = (0..arity).map(syn::Index::from);

                quote! {
                    (
                        #variant_str,
                        Box::new(|ctor_fields| {
                            if ctor_fields.len() == #arity {
                                Ok(#ident::#variant_ident(
                                    #(Json::from_json(&ctor_fields[#fields])?),*
                                ))
                            } else {
                                Err(lbr_prelude::error::Error::UnexpectedArrayLength {
                                    wanted: #arity,
                                    got: ctor_fields.len(),
                                    parser: #ident_str.to_owned(),
                                })
                            }
                        })
                    )
                }
            }

            syn::Fields::Unit => quote! {
                (
                    #variant_str,
                    Box::new(|ctor_fields| match &ctor_fields[..] {
                        [] => Ok(#ident::#variant),
                        _ => Err(lbr_prelude::error::Error::UnexpectedArrayLength {
                            wanted: 0,
                            got: ctor_fields.len(),
                            parser: #ident_str.to_owned(),
                        })
                    })
                )

            },
        }
    });

    let from_json_impl = quote! {
        fn from_json(value: &serde_json::Value) -> Result<Self, lbr_prelude::error::Error> {
            lbr_prelude::json::case_json_constructor(#ident_str, vec![
                #(#from_json_match_arms),*
            ],
            value)
        }
    };

    (to_json_impl, from_json_impl)
}
