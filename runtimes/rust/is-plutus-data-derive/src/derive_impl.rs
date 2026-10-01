use std::str::FromStr;

use quote::format_ident;
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
    Arm, Attribute, Block, Data, DataEnum, DataStruct, DeriveInput, Error, Expr, ExprLit, Fields,
    FieldsNamed, FieldsUnnamed, Ident, Index, ItemImpl, Lit, Meta, Path, Result, Stmt,
};

pub(crate) fn get_is_plutus_data_instance(input: DeriveInput) -> Result<ItemImpl> {
    let type_name = &input.ident;

    let strategy = get_derive_strategy(&input)?;

    let plutus_data_input_var: Ident = parse_quote!(plutus_data);

    let (encoder, decoder) = match strategy {
        DeriveStrategy::Newtype => get_newtype_encoder_decoder(&input),
        DeriveStrategy::List => get_list_encoder_decoder(&input, &plutus_data_input_var),
        DeriveStrategy::Constr => get_constr_encoder_decoder(&input, &plutus_data_input_var),
    }?;

    let mut generics = input.generics;

    // TODO(chfanghr): Do we care about type role? Can we determine it?
    generics.type_params_mut().for_each(|param| {
        param.bounds.push(parse_quote!(plutus_data::IsPlutusData));
    });

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    Ok(parse_quote!(
        impl #impl_generics plutus_ledger_api::plutus_data::IsPlutusData for #type_name #type_generics #where_clause {
            fn to_plutus_data(&self) -> plutus_ledger_api::plutus_data::PlutusData {
                #encoder
            }

            fn from_plutus_data(plutus_data: &plutus_ledger_api::plutus_data::PlutusData) -> Result<Self, plutus_ledger_api::plutus_data::PlutusDataError>
                where Self: Sized {
                #decoder
            }
        }
    ))
}

#[derive(Debug)]
enum DeriveStrategy {
    Newtype,
    List,
    Constr,
}

#[derive(Debug, thiserror::Error)]
enum DeriveStrategyError {
    #[error("Unknown strategy {0}. Should be one of Newtype, List and Constr.")]
    UnknownStrategy(String),
    #[error("Unable to parse strategy. Should be a string literal Newtype, Constr or List.")]
    UnexpectedToken,
    #[error("More than one strategies specified.")]
    MoreThanOneSpecified,
}

impl Default for DeriveStrategy {
    fn default() -> Self {
        Self::Constr
    }
}

impl FromStr for DeriveStrategy {
    type Err = DeriveStrategyError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "Newtype" => Ok(Self::Newtype),
            "List" => Ok(Self::List),
            "Constr" => Ok(Self::Constr),
            _ => Err(DeriveStrategyError::UnknownStrategy(s.into())),
        }
    }
}
impl Parse for DeriveStrategy {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.call(Ident::parse)?;
        Self::from_str(&ident.to_string()).map_err(|unknown_strategy| {
            Error::new(
                ident.span(),
                format!("unknown strategy: {}", unknown_strategy),
            )
        })
    }
}

fn try_parse_derive_strategy(attr: &Attribute) -> Option<Result<DeriveStrategy>> {
    let value = match &attr.meta {
        Meta::NameValue(name_value) => name_value
            .path
            .is_ident("is_plutus_data_derive_strategy")
            .then_some(&name_value.value),
        _ => None,
    }?;

    Some(match &value {
        Expr::Lit(ExprLit {
            lit: Lit::Str(str_lit),
            ..
        }) => (|| -> Result<DeriveStrategy> {
            DeriveStrategy::from_str(&str_lit.value()).map_err(|err| Error::new(attr.span(), err))
        })(),
        _ => Err(Error::new(
            value.span(),
            DeriveStrategyError::UnexpectedToken,
        )),
    })
}

fn get_derive_strategy(input: &DeriveInput) -> Result<DeriveStrategy> {
    let mut derive_strategy_results: Vec<_> = input
        .attrs
        .iter()
        .map(try_parse_derive_strategy)
        .flatten()
        .collect();

    match derive_strategy_results.len() {
        0 => Ok(DeriveStrategy::default()),
        1 => derive_strategy_results.remove(0),
        _ => Err(Error::new(
            input.span(),
            DeriveStrategyError::MoreThanOneSpecified,
        )),
    }
}

#[derive(Debug, thiserror::Error)]
enum NewtypeStrategyError {
    #[error("Only struct types are supported by newtype strategy")]
    UnexpectedDataVariant,
    #[error("Newtype derivation expects exactly one filed")]
    NotSingleField,
}

fn get_newtype_encoder_decoder(input: &DeriveInput) -> Result<(Block, Block)> {
    let s = match &input.data {
        Data::Struct(s) => Ok(s),
        _ => Err(Error::new(
            input.span(),
            NewtypeStrategyError::UnexpectedDataVariant,
        )),
    }?;

    if s.fields.len() != 1 {
        Err(Error::new(
            input.span(),
            NewtypeStrategyError::NotSingleField,
        ))?
    }

    let field = s.fields.iter().next().unwrap();

    let encoder = match &field.ident {
        None => parse_quote!({ self.0.to_plutus_data() }),
        Some(ident) => parse_quote!({
            self.#ident.to_plutus_data()
        }),
    };

    let decoder = match &field.ident {
        Some(field_name) => {
            parse_quote!({
                Ok(Self {
                    #field_name: plutus_ledger_api::plutus_data::IsPlutusData::from_plutus_data(plutus_data)?
                })
            })
        }
        None => {
            parse_quote!({
                Ok(Self(
                    plutus_ledger_api::plutus_data::IsPlutusData::from_plutus_data(plutus_data)?,
                ))
            })
        }
    };

    Ok((encoder, decoder))
}

#[derive(Debug, thiserror::Error)]
enum ListStrategyError {
    #[error("Only struct types are supported by list strategy")]
    UnexpectedDataVariant,
}

fn get_list_encoder_decoder(
    input: &DeriveInput,
    plutus_data_input_var: &Ident,
) -> Result<(Block, Block)> {
    match &input.data {
        Data::Struct(s) => match &s.fields {
            Fields::Named(fields_named) => Ok((
                struct_with_named_fields_to_plutus_data_list(fields_named),
                struct_with_named_fields_from_plutus_data_list(fields_named, plutus_data_input_var),
            )),
            Fields::Unnamed(fields_unnamed) => Ok((
                struct_with_unnamed_fields_to_plutus_data_list(fields_unnamed),
                struct_with_unnamed_fields_from_plutus_data_list(
                    fields_unnamed,
                    plutus_data_input_var,
                ),
            )),
            Fields::Unit => Ok((
                struct_with_no_field_to_plutus_data_list(),
                struct_with_no_field_from_plutus_data_list(plutus_data_input_var),
            )),
        },
        _ => Err(Error::new(
            input.span(),
            ListStrategyError::UnexpectedDataVariant,
        )),
    }
}

#[derive(Debug, thiserror::Error)]
enum ConstrStrategyError {
    #[error("Union types are supported by constr strategy")]
    UnexpectedDataVariant,
}

fn get_constr_encoder_decoder(
    input: &DeriveInput,
    plutus_data_input_var: &Ident,
) -> Result<(Block, Block)> {
    Ok(match &input.data {
        Data::Enum(e) => get_enum_constr_encoder_decoder(e, plutus_data_input_var),
        Data::Struct(s) => get_struct_constr_encoder_decoder(s, plutus_data_input_var),
        _ => Err(Error::new(
            input.span(),
            ConstrStrategyError::UnexpectedDataVariant,
        ))?,
    })
}

fn get_enum_constr_encoder_decoder(e: &DataEnum, plutus_data_input_var: &Ident) -> (Block, Block) {
    (
        enum_to_plutus_data_constr(&e),
        enum_from_plutus_data_constr(&e, plutus_data_input_var),
    )
}

fn get_struct_constr_encoder_decoder(
    s: &DataStruct,
    plutus_data_input_var: &Ident,
) -> (Block, Block) {
    match &s.fields {
        Fields::Named(fields_named) => (
            struct_with_named_fields_to_plutus_data_constr(fields_named),
            struct_with_named_fields_from_plutus_data_constr(fields_named, plutus_data_input_var),
        ),
        Fields::Unnamed(fields_unnamed) => (
            struct_with_unnamed_fields_to_plutus_data_constr(fields_unnamed),
            struct_with_unnamed_fields_from_plutus_data_constr(
                fields_unnamed,
                plutus_data_input_var,
            ),
        ),
        Fields::Unit => (
            struct_with_no_field_to_plutus_data_constr(),
            struct_with_no_field_from_plutus_data_constr(plutus_data_input_var),
        ),
    }
}

fn enum_to_plutus_data_constr(e: &DataEnum) -> Block {
    let variants = &e.variants;
    let tags = 0..variants.len();

    let arms = tags.zip(variants.iter()).map(|(tag, variant)| {
        let variant_name = &variant.ident;
        let constructor: Path = parse_quote!(Self::#variant_name);
        let fields = &variant.fields;
        variant_to_plutus_data(&constructor, tag, fields)
    });

    parse_quote!({
        match &self {
            #(#arms),*
        }
    })
}

fn enum_from_plutus_data_constr(e: &DataEnum, plutus_data_input_var: &Ident) -> Block {
    let variants = &e.variants;
    let tags = 0..variants.len();
    let expected_tags_str = String::from("Constr with tag: ")
        + &tags
            .clone()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("/");
    let plutus_data_list_var: Ident = parse_quote!(plutus_data_list);

    let arms = tags.zip(variants.iter()).map(|(tag, variant)| {
        let variant_name = &variant.ident;
        let constructor: Path = parse_quote!(Self::#variant_name);
        let fields = &variant.fields;

        variant_from_plutus_data(&constructor, tag, fields, &plutus_data_list_var)
    });

    parse_quote!(
        {
            let (tag, #plutus_data_list_var) = plutus_ledger_api::plutus_data::parse_constr(#plutus_data_input_var)?;

            match tag {
                #(#arms),*
                tag => Err(plutus_ledger_api::plutus_data::PlutusDataError::UnexpectedPlutusInvariant {
                    wanted: format!(#expected_tags_str),
                    got: tag.to_string(),
                }),
            }
        }
    )
}

fn variant_to_plutus_data(constructor: &Path, tag: usize, fields: &Fields) -> Arm {
    match fields {
        Fields::Named(named) => variant_with_named_fields_to_plutus_data(&constructor, tag, &named),
        Fields::Unnamed(unnamed) => {
            variant_with_unnamed_field_to_plutus_data(&constructor, tag, &unnamed)
        }
        Fields::Unit => variant_with_no_field_to_plutus_data(&constructor, tag),
    }
}

fn variant_from_plutus_data(
    constructor: &Path,
    tag: usize,
    fields: &Fields,
    plutus_data_list_var: &Ident,
) -> Arm {
    let block = match fields {
        Fields::Named(named) => variant_with_named_fields_from_plutus_data_list(
            constructor,
            named,
            plutus_data_list_var,
        ),
        Fields::Unnamed(unnamed) => variant_with_unnamed_fields_from_plutus_data_list(
            constructor,
            unnamed,
            plutus_data_list_var,
        ),
        Fields::Unit => {
            variant_with_no_field_from_plutus_data_list(constructor, plutus_data_list_var)
        }
    };

    let tag = tag as u32;

    parse_quote!(
        #tag => #block
    )
}

fn variant_with_named_fields_to_plutus_data(
    constructor: &Path,
    tag: usize,
    fields_named: &FieldsNamed,
) -> Arm {
    let field_names = fields_named
        .named
        .iter()
        .map(|field| field.ident.as_ref().unwrap());

    let field_accessors = field_names
        .clone()
        .map(|field_name| -> Expr { parse_quote!(#field_name) })
        .collect::<Vec<_>>();

    let plutus_data_list = data_fields_to_list_of_plutus_data(&field_accessors);

    parse_quote!(
        #constructor{ #(#field_names),* } => plutus_ledger_api::plutus_data::PlutusData::Constr(#tag.into(), #plutus_data_list)
    )
}

fn variant_with_named_fields_from_plutus_data_list(
    constructor: &Path,
    fields_named: &FieldsNamed,
    plutus_data_list_var: &Ident,
) -> Block {
    data_with_named_fields_from_list_of_plutus_data(constructor, fields_named, plutus_data_list_var)
}

fn variant_with_unnamed_field_to_plutus_data(
    constructor: &Path,
    tag: usize,
    fields_unnamed: &FieldsUnnamed,
) -> Arm {
    let field_names = (0..fields_unnamed.unnamed.len()).map(|idx| format_ident!("field_{}", idx));

    let field_accessors = field_names
        .clone()
        .map(|field_name| -> Expr { parse_quote!(#field_name) })
        .collect::<Vec<_>>();

    let plutus_data_list = data_fields_to_list_of_plutus_data(&field_accessors);

    parse_quote!(
        #constructor(#(#field_names),*) => plutus_ledger_api::plutus_data::PlutusData::Constr(#tag.into(), #plutus_data_list)
    )
}

fn variant_with_unnamed_fields_from_plutus_data_list(
    constructor: &Path,
    fields_unnamed: &FieldsUnnamed,
    plutus_data_list_var: &Ident,
) -> Block {
    data_with_unnamed_fields_from_list_of_plutus_data(
        constructor,
        fields_unnamed,
        plutus_data_list_var,
    )
}

fn variant_with_no_field_to_plutus_data(constructor: &Path, tag: usize) -> Arm {
    parse_quote!(
        #constructor => plutus_ledger_api::plutus_data::PlutusData::Constr(#tag.into(), vec![])
    )
}

fn variant_with_no_field_from_plutus_data_list(
    constructor: &Path,
    plutus_data_list_var: &Ident,
) -> Block {
    data_with_no_fields_from_list_of_plutus_data(constructor, plutus_data_list_var)
}

fn struct_with_named_fields_to_list_of_plutus_data(fields: &FieldsNamed) -> Block {
    let field_accessors = fields
        .named
        .iter()
        .map(|field| -> Expr {
            let field_name = field.ident.as_ref().unwrap();

            parse_quote!(self.#field_name)
        })
        .collect::<Vec<_>>();

    data_fields_to_list_of_plutus_data(&field_accessors)
}

fn struct_with_named_fields_from_list_of_plutus_data(
    fields: &FieldsNamed,
    plutus_data_list_var: &Ident,
) -> Block {
    let constructor: Path = parse_quote!(Self);

    data_with_named_fields_from_list_of_plutus_data(&constructor, fields, &plutus_data_list_var)
}

fn struct_with_named_fields_to_plutus_data_list(fields: &FieldsNamed) -> Block {
    let to_list_of_plutus_data = struct_with_named_fields_to_list_of_plutus_data(fields);

    parse_quote!({
        plutus_ledger_api::plutus_data::PlutusData::List(#to_list_of_plutus_data)
    })
}

fn struct_with_named_fields_from_plutus_data_list(
    fields: &FieldsNamed,
    plutus_data_input_var: &Ident,
) -> Block {
    let list_of_plutus_data_var: Ident = parse_quote!(list_of_plutus_data);

    let from_list_of_plutus_data =
        struct_with_named_fields_from_list_of_plutus_data(fields, &list_of_plutus_data_var);

    parse_quote!({
        let #list_of_plutus_data_var = plutus_ledger_api::plutus_data::parse_list(#plutus_data_input_var)?;

        #from_list_of_plutus_data
    })
}

fn struct_with_named_fields_to_plutus_data_constr(fields: &FieldsNamed) -> Block {
    let to_list_of_plutus_data = struct_with_named_fields_to_list_of_plutus_data(fields);

    parse_quote!({
        plutus_ledger_api::plutus_data::PlutusData::Constr(0.into(), #to_list_of_plutus_data)
    })
}

fn struct_with_named_fields_from_plutus_data_constr(
    fields: &FieldsNamed,
    plutus_data_input_var: &Ident,
) -> Block {
    let plutus_data_list_var: Ident = parse_quote!(plutus_data_list);

    let from_plutus_data_list =
        struct_with_named_fields_from_list_of_plutus_data(fields, &plutus_data_list_var);

    parse_quote!({
        let #plutus_data_list_var = plutus_ledger_api::plutus_data::parse_constr_with_tag(#plutus_data_input_var, 0)?;

        #from_plutus_data_list
    })
}

fn struct_with_unnamed_fields_to_list_of_plutus_data(fields: &FieldsUnnamed) -> Block {
    let len = fields.unnamed.len();

    let field_accessors = (0..len)
        .into_iter()
        .map(|idx| -> Expr {
            let idx: Index = idx.into();

            parse_quote!(self.#idx)
        })
        .collect::<Vec<_>>();

    data_fields_to_list_of_plutus_data(&field_accessors)
}

fn struct_with_unnamed_fields_from_list_of_plutus_data(
    fields: &FieldsUnnamed,
    plutus_data_list_var: &Ident,
) -> Block {
    data_with_unnamed_fields_from_list_of_plutus_data(
        &parse_quote!(Self),
        fields,
        plutus_data_list_var,
    )
}

fn struct_with_unnamed_fields_to_plutus_data_list(fields: &FieldsUnnamed) -> Block {
    let to_list_of_plutus_data = struct_with_unnamed_fields_to_list_of_plutus_data(fields);

    parse_quote!({
        plutus_ledger_api::plutus_data::PlutusData::List(#to_list_of_plutus_data)
    })
}

fn struct_with_unnamed_fields_from_plutus_data_list(
    fields: &FieldsUnnamed,
    plutus_data_input_var: &Ident,
) -> Block {
    let list_of_plutus_data_var: Ident = parse_quote!(list_of_plutus_data);

    let from_list_of_plutus_data =
        struct_with_unnamed_fields_from_list_of_plutus_data(fields, &list_of_plutus_data_var);

    parse_quote!({
        let #list_of_plutus_data_var = plutus_ledger_api::plutus_data::parse_list(#plutus_data_input_var)?;

        #from_list_of_plutus_data
    })
}

fn struct_with_unnamed_fields_to_plutus_data_constr(fields: &FieldsUnnamed) -> Block {
    let to_list_of_plutus_data = struct_with_unnamed_fields_to_list_of_plutus_data(fields);

    parse_quote!({
        plutus_ledger_api::plutus_data::PlutusData::Constr(0.into(), #to_list_of_plutus_data)
    })
}

fn struct_with_unnamed_fields_from_plutus_data_constr(
    fields: &FieldsUnnamed,
    plutus_data_input_var: &Ident,
) -> Block {
    let fields_var: Ident = parse_quote!(fields);

    let from_fields = struct_with_unnamed_fields_from_list_of_plutus_data(fields, &fields_var);

    parse_quote!({
        let #fields_var = plutus_ledger_api::plutus_data::parse_constr_with_tag(#plutus_data_input_var, 0)?;

        #from_fields
    })
}

fn struct_with_no_field_to_plutus_data_list() -> Block {
    parse_quote!(plutus_data::PlutusData::Constr(0.into(), vec![]))
}

fn struct_with_no_field_from_plutus_data_list(plutus_data_input_var: &Ident) -> Block {
    let list_of_plutus_data_var: Ident = parse_quote!(list_of_plutus_data);

    let from_list_of_plutus_data =
        data_with_no_fields_from_list_of_plutus_data(&parse_quote!(Self), &list_of_plutus_data_var);

    parse_quote!({
        let #list_of_plutus_data_var = plutus_ledger_api::plutus_data::parse_list(#plutus_data_input_var)?;

        #from_list_of_plutus_data
    })
}

fn struct_with_no_field_to_plutus_data_constr() -> Block {
    parse_quote!(plutus_data::PlutusData::Constr(0.into(), vec![]))
}

fn struct_with_no_field_from_plutus_data_constr(plutus_data_input_var: &Ident) -> Block {
    let fields_var: Ident = parse_quote!(fields);

    let from_fields =
        data_with_no_fields_from_list_of_plutus_data(&parse_quote!(Self), &fields_var);

    parse_quote!({
        let #fields_var = plutus_ledger_api::plutus_data::parse_constr_with_tag(#plutus_data_input_var, 0)?;

        #from_fields
    })
}

fn data_fields_to_list_of_plutus_data(field_accessors: &[Expr]) -> Block {
    let fields_to_plutus_data = field_accessors
        .iter()
        .map(|a| -> Expr { parse_quote!(#a.to_plutus_data()) });

    parse_quote!({ vec![ #(#fields_to_plutus_data),* ] })
}

fn data_with_named_fields_from_list_of_plutus_data(
    constructor: &Path,
    fields_named: &FieldsNamed,
    plutus_data_list_var: &Ident,
) -> Block {
    let field_count = fields_named.named.len();

    let field_idents = fields_named
        .named
        .iter()
        .map(|field| field.ident.as_ref().unwrap());

    let unparsed_field_idents = field_idents
        .clone()
        .map(|field_ident| format_ident!("unparsed_{}", field_ident));

    let field_decoded_stmts = field_idents.clone().zip(unparsed_field_idents.clone()).map(
        |(field_ident, unparsed_field_ident)| -> Stmt {
            parse_quote!(
                let #field_ident = plutus_ledger_api::plutus_data::IsPlutusData::from_plutus_data(#unparsed_field_ident)?;
            )
        },
    );

    parse_quote!(
        {
            let [ #(#unparsed_field_idents),* ] = plutus_ledger_api::plutus_data::parse_fixed_len_constr_fields::<#field_count>(#plutus_data_list_var)?;
            #(#field_decoded_stmts)*
            Ok(#constructor{ #(#field_idents),* })
        }
    )
}

fn data_with_unnamed_fields_from_list_of_plutus_data(
    constructor: &Path,
    fields_unnamed: &FieldsUnnamed,
    plutus_data_list_var: &Ident,
) -> Block {
    let field_count = fields_unnamed.unnamed.len();

    let unparsed_field_idents =
        (0..field_count).map(|field_index| format_ident!("unparsed_{}", field_index));

    let parsed_field_idents =
        (0..field_count).map(|field_index| format_ident!("parsed_{}", field_index));

    let field_decoded_stmts = unparsed_field_idents
        .clone()
        .zip(parsed_field_idents.clone())
        .map(|(unparsed, parsed)| -> Stmt {
            parse_quote!(
                let #parsed = IsPlutusData::from_plutus_data(#unparsed)?;
            )
        });

    parse_quote!({
        let [ #(#unparsed_field_idents),* ] = plutus_ledger_api::plutus_data::parse_fixed_len_constr_fields::<#field_count>(#plutus_data_list_var)?;
        #(#field_decoded_stmts)*
        Ok(#constructor(#(#parsed_field_idents),*))
    })
}

fn data_with_no_fields_from_list_of_plutus_data(
    constructor: &Path,
    list_of_plutus_data_var: &Ident,
) -> Block {
    parse_quote!({
        let [ ] = plutus_ledger_api::plutus_data::parse_fixed_len_constr_fields::<0>(#list_of_plutus_data_var)?;
        Ok(#constructor)
    })
}
