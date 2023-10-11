extern crate proc_macro;
use anyhow::Error;
use codegen::{graphql::generate_helper_functions, map_helpers::generate_map_module};
use proc_macro::TokenStream;
use proc_macro2::Group;
use quote::{format_ident, quote};
use substreams_helpers_traits::FromBlock;
use syn::{
    parse_macro_input, Attribute, AttributeArgs, Data, DataStruct, DeriveInput, Field, File, Item,
    ItemMod, ItemStruct, Lit, LitStr, Meta, MetaList, NestedMeta, Path,
};
mod codegen;
mod parser;

#[proc_macro]
pub fn helpers_from_graphql(input: TokenStream) -> TokenStream {
    let input_str = parse_macro_input!(input as LitStr).value();
    let generated_code = generate_helper_functions(&input_str);
    TokenStream::from(generated_code)
}

// write a proc macro to rename a struct to structEvent

#[proc_macro_attribute]
pub fn rename_event(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as ItemStruct);

    let name = input.ident.clone();

    let event_name = format_ident!("{}Event", name);

    input.ident = event_name;

    let expanded = quote! {
       #input
    };

    expanded.into()
}

fn nested_meta_to_attribute(nested: &NestedMeta) -> Option<Attribute> {
    match nested {
        NestedMeta::Meta(meta) => Some(syn::parse_quote!( #[#meta])),
        _ => None,
    }
}

// write a macro to append an attribute to each struct in a module
#[proc_macro_attribute]
pub fn append_attribute(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as ItemMod);
    let (_, ref mut content) = input.content.as_mut().unwrap();
    let attributes = parse_macro_input!(attrs as AttributeArgs);

    for item in content {
        if let Item::Struct(ref mut s) = item {
            for attribute in &attributes {
                let attribute = nested_meta_to_attribute(&attribute);
                if let Some(attribute) = attribute {
                    s.attrs.push(attribute);
                }
            }
        }
    }

    let expanded = quote! {
       #input
    };

    expanded.into()
}

#[proc_macro_attribute]
pub fn append_derive(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as ItemMod);
    let (_, ref mut content) = input.content.as_mut().unwrap();
    let traits = parse_macro_input!(attrs as AttributeArgs);

    for item in content {
        if let Item::Struct(ref mut s) = item {
            for attr in &mut s.attrs {
                if attr.path.is_ident("derive") {
                    let meta = attr.parse_meta().unwrap();
                    if let Meta::List(meta_list) = meta {
                        let mut nested = meta_list.nested;
                        for trait_ in traits.clone() {
                            if nested.iter().any(|n| n == &trait_) {
                                continue;
                            }
                            nested.push(trait_);
                        }
                        let clone = attr.clone();
                        *attr = Attribute {
                            tokens: quote!((#nested)),
                            ..clone
                        }
                    }
                }
            }
        }
    }

    let expanded = quote! {
       #input
    };

    expanded.into()
}

#[proc_macro_derive(FromBlock)]
pub fn from_block_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_from_block(&ast)
}

fn impl_from_block(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    quote! {
        impl ::substreams_helpers_traits::FromBlock for #name { }
    }
    .into()
}

// used to grab a partciular field from an attribute
// example: get the from_event field from map_config
// #[map_config(from_event = "events::Approval")]
fn get_attribute_value(
    attributes: &Vec<Attribute>,
    attribute_name: &str,
    field_name: &str,
) -> Option<String> {
    for attribute in attributes {
        if attribute.path.is_ident(attribute_name) {
            if let Ok(meta) = attribute.parse_meta() {
                if let Meta::List(list) = meta {
                    for nested in list.nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                                let ident = nv.path.get_ident().map(|i| i.to_string());
                                if ident.as_deref() == Some(field_name) {
                                    if let syn::Lit::Str(lit_str) = &nv.lit {
                                        return Some(lit_str.value());
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
    }
    None
}

/// This function will grab a named attribute, that doesn't have a value
/// example: #[map_config(from_event)]
///
/// Would be grabbed as
/// get_named_attribute(&attributes, "map_config", ["from_event"])
fn get_named_attributes(
    attributes: &Vec<Attribute>,
    atribute_name: &str,
    field_names: &[&str],
) -> Option<String> {
    for attribute in attributes {
        if attribute.path.is_ident(atribute_name) {
            if let Ok(meta) = attribute.parse_meta() {
                if let Meta::List(list) = meta {
                    for nested in list.nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                                let ident = path.get_ident().map(|i| i.to_string());
                                if let Some(ident) = ident {
                                    if field_names.contains(&ident.as_str()) {
                                        return Some(ident);
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
    }
    None
}

fn get_glue_config(input: &Vec<Attribute>, field_name: &String) -> GlueConfig {
    // the way glue should work,
    // is we attach it to a field
    // this is also the glue_to value,
    // the glue_from value is also defaulted to the field name
    // and the glue_type is the argument to the "glue" attribute
    // Simple example:
    // #[glue(event)]
    // OR
    // #[glue(function)]
    //
    // more complex example:
    // #[glue(event, glue_from = "owner")]
    // #[glue(function, glue_from = "owner")]

    let mut glue_type = "event".to_string();

    for attr in input {
        if attr.path.is_ident("glue") {
            if let Ok(meta) = attr.parse_meta() {
                if let Meta::List(list) = meta {
                    for nested in list.nested {
                        match nested {
                            syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                                let ident = path.get_ident().map(|i| i.to_string());
                                if let Some(ident) = ident {
                                    match ident.as_str() {
                                        "event" | "function" | "message" => {
                                            glue_type = ident.to_string();
                                        }
                                        _ => (),
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
    }

    let glue_from = get_attribute_value(input, "glue", "glue_from").unwrap_or(field_name.clone());
    let glue_type = GlueType::try_from(glue_type.as_str()).expect("Failed to parse glue_type");
    GlueConfig {
        glue_from,
        glue_to: field_name.clone(),
        glue_type,
    }
}

fn get_glue(input: &syn::DeriveInput) -> bool {
    let attributes = &input.attrs;

    let mut glue = false;

    for attribute in attributes {
        if attribute.path.is_ident("glue") {
            glue = true;
        }
    }

    glue
}

fn get_fields_glue(input: &syn::DeriveInput) -> Vec<GlueConfig> {
    let mut fields = Vec::new();

    let struct_fields = match &input.data {
        Data::Struct(data) => {
            if let syn::Fields::Named(fields) = &data.fields {
                let named_fields_vec: Vec<_> = fields
                    .named
                    .pairs()
                    .map(|p| p.into_value().clone())
                    .collect();
                named_fields_vec
            } else {
                panic!("Only named fields are supported for now")
            }
        }
        _ => panic!("Only structs with named fields are supported for now"),
    };

    for field in struct_fields {
        let glue = get_glue_config(&field.attrs, &field.ident.as_ref().unwrap().to_string());

        fields.push(glue);
    }

    fields
}

#[derive(Debug)]
struct MapConfig {
    from_event: syn::Path,
    from_function: Option<syn::Path>,
    message: syn::Path,
    plural_message: syn::Path,
    module_name: String,
    use_glue: bool,
    from_address: Option<String>,
    /// The fields we can't map directly from the event, but need to glue together
    glue_fields: Vec<GlueConfig>,
    /// The fields we are omitting from the event or whatever
    skipped_fields: Vec<String>,
}

#[derive(Debug)]
struct GlueConfig {
    /// the name of the field we are gluing from
    glue_from: String,
    /// the name of the field we are gluing to
    glue_to: String,
    /// The thing we are grabbing from
    glue_type: GlueType,
}

#[derive(Debug)]
enum GlueType {
    Event,
    Function,
    Log,
}

impl GlueType {
    fn to_str(&self) -> &'static str {
        match self {
            GlueType::Event => "event",
            GlueType::Function => "function",
            GlueType::Log => "log",
        }
    }
}

impl TryFrom<&str> for GlueType {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "event" => Ok(GlueType::Event),
            "function" => Ok(GlueType::Function),
            "log" => Ok(GlueType::Log),
            _ => Err(Error::msg(format!(
                "glue_type must be one of event, function, or log"
            ))),
        }
    }
}

fn get_map_config(input: &syn::DeriveInput) -> MapConfig {
    let attributes: &Vec<Attribute> = &input.attrs;

    let from_event = get_attribute_value(attributes, "map_config", "from_event")
        .expect("Please provide a from_event attribute, like from_event = \"SomeEvent\",");
    let from_event = syn::parse_str(&from_event)
        .expect("Failed to parse from_event, please make sure this is a valid path to a struct");

    let from_function = if let Some(from_function) =
        get_attribute_value(attributes, "map_config", "from_function")
    {
        Some(syn::parse_str(&from_function).expect(
            "Failed to parse from_function, please make sure this is a valid path to a function",
        ))
    } else {
        None
    };

    let message = get_attribute_value(attributes, "map_config", "message")
        .expect("Please provide a message attribute, like message = \"SomeMessage\"");
    let message = syn::parse_str(&message)
        .expect("Failed to parse message, please make sure this is a valid path to a struct");

    let plural_message = get_attribute_value(attributes, "map_config", "plural_message")
        .expect("Please provide a plural_message attribute, like plural_message = \"SomeMessage\"");
    let plural_message = syn::parse_str(&plural_message).expect(
        "Failed to parse plural_message, please make sure this is a valid path to a struct",
    );

    let use_glue = get_glue(input);

    let module_name = get_attribute_value(attributes, "map_config", "module_name")
        .expect("Please provide a module_name attribute, like module_name = \"map_approvals\"");

    let from_address = get_attribute_value(attributes, "map_config", "from_address");

    let glue_fields = get_fields_glue(input);
    // TODO get skipped fields, with the #[omit] tag
    let skipped_fields = vec![];

    MapConfig {
        from_event,
        from_function,
        message,
        plural_message,
        module_name,
        use_glue,
        from_address,
        glue_fields,
        skipped_fields,
    }
}

#[proc_macro_derive(Map, attributes(map_config, glue))]
pub fn derive_map(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let map_config = get_map_config(&input);
    let MapConfig {
        from_event,
        from_function,
        message,
        plural_message,
        use_glue,
        module_name,
        from_address,
        glue_fields,
        skipped_fields,
    } = map_config;

    impl_glue_conversion(
        &from_event,
        &from_function,
        &message,
        &plural_message,
        module_name,
        from_address,
        glue_fields,
    )
}

fn impl_glue_conversion(
    event_path: &syn::Path,
    function_path: &Option<syn::Path>,
    message: &syn::Path,
    plural_message: &syn::Path,
    module_name: String,
    from_address: Option<String>,
    glue_fields: Vec<GlueConfig>,
) -> TokenStream {
    let intermediate_fields: Vec<_> = glue_fields
        .iter()
        .map(|field| {
            let glue_type = format_ident!("{}", field.glue_type.to_str());
            let glue_from = format_ident!("{}", field.glue_from);
            let field = format_ident!("{}", field.glue_to);
            quote! {
                let #field: ::substreams_helpers_traits::StringW = #glue_type.#glue_from.into();
            }
        })
        .collect();

    let field_conversions: Vec<_> = glue_fields
        .iter()
        .map(|field| {
            let field = format_ident!("{}", field.glue_to);
            quote! {
                #field: #field.into()
            }
        })
        .collect();

    let from_trait = if let Some(function_path) = function_path {
        quote! {
            #[derive(From, Into)]
            struct Dummy<'a>((#event_path, #function_path,  substreams_ethereum::block_view::LogView<'a>));

            impl FromBlockAndAddress for Dummy<'_> {
                fn from_block_and_address(
                    block: substreams_ethereum::pb::eth::v2::Block,
                    address: Option<&str>,
                ) -> Vec<Self> {
                    block
                        .logs()
                        .filter(|log| {
                            if let Some(address) = address {
                                log.address()
                                    == substreams::Hex::decode(address).expect("Failed to decode address")
                            } else {
                                true
                            }
                        })
                        .filter_map(|log| {
                            if let Some(event) = #event_path::match_and_decode(log){
                                let mut function = None;
                                for call in log.receipt.transaction.calls() {
                                    if let Some(function) = #function_path::match_and_decode(call) {
                                        return Some(Dummy((event, function, log)))
                                    }
                                }
                            }
                            return None;
                        })
                        .collect::<Vec<Self>>()
                }
            }

            impl From<Dummy<'_>> for #message {
                fn from(tuple: (#event_path, #function_path, substreams_ethereum::block_view::LogView)) -> Self {
                    let (event, function, log) = tuple;
                    #(#intermediate_fields)*
                    Self {
                        #(#field_conversions),*
                    }
                }
            }

            impl ::substreams_helpers_traits::FromBlock for Dummy<'_> {}

            impl ::substreams_helpers_traits::Map<Dummy<'_>, #message> for #plural_message {}
        }
    } else {
        quote! {

            #[derive(From, Into)]
            struct Dummy<'a>((#event_path, substreams_ethereum::block_view::LogView<'a>));

            impl FromBlockAndAddress for Dummy<'_> {
                fn from_block_and_address(
                    block: substreams_ethereum::pb::eth::v2::Block,
                    address: Option<&str>,
                ) -> Vec<Self> {
                    block
                        .logs()
                        .filter(|log| {
                            if let Some(address) = address {
                                log.address()
                                    == substreams::Hex::decode(address).expect("Failed to decode address")
                            } else {
                                true
                            }
                        })
                        .filter_map(|log| {
                            if let Some(event) = #event_path::match_and_decode(log){
                                return Some(Dummy((event, log)))
                            }
                            return None;
                        })
                        .collect::<Vec<Self>>()
                }
            }

            impl From<Dummy<'_>> for #message {
                fn from(dummy: Dummy) -> Self {
                    let (event, log) = dummy.into();
                    #(#intermediate_fields)*
                    Self {
                        #(#field_conversions),*
                    }
                }
            }

            impl ::substreams_helpers_traits::FromBlock for Dummy<'_> {}

            impl ::substreams_helpers_traits::Map<Dummy<'_>, #message> for #plural_message {}
        }
    };

    let function_name = format_ident!("{}", module_name);

    let from_address = if let Some(from_address) = from_address {
        quote! {
            Some(#from_address)
        }
    } else {
        quote! {
            None
        }
    };

    let module_code = quote! {
        #[substreams::handlers::map]
        pub fn #function_name(block: substreams_ethereum::pb::eth::v2::Block) -> Result<#plural_message, substreams::errors::Error> {
            #plural_message::map(block, #from_address)
        }
    };

    quote! {
        #from_trait
        #module_code
    }
    .into()
}

fn impl_conversion_traits(
    input: &syn::DeriveInput,
    event_path: &syn::Path,
    message: &syn::Path,
    plural_message: &syn::Path,
    module_name: String,
    from_address: Option<String>,
) -> TokenStream {
    let fields = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            syn::Fields::Named(fields) => fields,
            _ => panic!("Only named fields are supported for now"),
        },
        _ => panic!("Only structs with named fields are supported for now"),
    };

    let field_names = fields
        .named
        .iter()
        .map(|field| field.ident.as_ref().unwrap())
        .collect::<Vec<_>>();

    let intermediate_fields = field_names.iter().map(|field| {
        quote! {
            let #field: ::substreams_helpers_traits::StringW = event.#field.into();
        }
    });

    let field_conversions = field_names.iter().map(|field| {
        quote! {
            #field: #field.into()
        }
    });

    let from_trait = quote! {
        impl From<#event_path> for #message {
            fn from(event: #event_path) -> Self {
                #(#intermediate_fields)*
                Self {
                    #(#field_conversions),*
                }
            }
        }
    };

    let from_block = quote! {
        impl ::substreams_helpers_traits::FromBlock for #event_path {}
    };

    let map_block = quote! {
        impl ::substreams_helpers_traits::Map<#event_path, #message> for #plural_message {}
    };

    let function_name = format_ident!("{}", module_name);
    let from_address = if let Some(from_address) = from_address {
        quote! {
            Some(#from_address)
        }
    } else {
        quote! {
            None
        }
    };

    let module_code = quote! {
        #[substreams::handlers::map]
        pub fn #function_name(block: substreams_ethereum::pb::eth::v2::Block) -> Result<#plural_message, substreams::errors::Error> {
            #plural_message::map(block, #from_address)
        }
    };

    quote! {
        #from_trait
        #from_block
        #map_block
        #module_code
    }
    .into()
}

fn impl_map(
    input: &syn::DeriveInput,
    rename_message: &syn::Path,
    rename_event: &syn::Path,
    plural_name: &syn::Path,
) -> TokenStream {
    let name = rename_message;
    let event_name = rename_event;
    let plural_name = plural_name;

    let conversion_body = conversion_body(&input.data);

    quote!(
        impl ::substreams_helpers_traits::FromEvent<#event_name> for #name {
            fn from_event(event: #event_name) -> Option<Self> {
                #conversion_body
            }
        }

        impl ::substreams_helpers_traits::Map<#event_name, #name> for #plural_name {}
    )
    .into()
}

fn conversion_body(data: &Data) -> proc_macro2::TokenStream {
    fn handle_struct(data: &DataStruct) -> TokenStream {
        let mut fields = Vec::new();
        let struct_fields = match &data.fields {
            syn::Fields::Named(fields) => fields,
            _ => panic!("Only named fields are supported for now"),
        };

        for field in struct_fields.named.iter() {
            let field_name = field.ident.as_ref().unwrap();
            fields.push(field_name);
        }

        let tuple = quote! {
            (#(#fields),*)
        };

        let types = struct_fields.named.iter().map(|field| {
            quote! { ::substreams_helpers_traits::StringW }
        });
        let tuple_type = quote!((#(#types),*));

        let into_statements = struct_fields.named.iter().map(|field| {
            let field_name = field.ident.as_ref().unwrap();
            quote! {
                #field_name.into()
            }
        });

        let inner_into_tuple = quote! {
            (#(#into_statements),*)
        };

        let conversion = quote! {
            let #tuple = event.into();
            let #tuple: #tuple_type = #inner_into_tuple;
            let #tuple = #inner_into_tuple;
            Some(#tuple.into())
        };

        conversion.into()
    }

    let body = match data {
        Data::Struct(data_struct) => handle_struct(data_struct),
        _ => panic!("Only structs with named fields are supported for now"),
    };

    body.into()
}

/*
impl FromEvent<LoanLiquidatedEvent> for LoanLiquidated {
    fn from_event(event: LoanLiquidatedEvent) -> Option<Self> {
        // based on the event, we will extract all the values into a tuple
        // so this probably looks like:
        /*

        let mut fields = Vec::new();
        for field in event.fields // if the fields match FieldsNamed(fields)
            fields.push(field.name.unwrap());
        }

        let tuple = quote! {
            (#(#fields),*)
        }

        let typed_tuple = quote! {
            (#(#fields),*): (#(StringW),*)
        }

        let inner_into_tuple = quote! {
            (#(#fields).into(),*)
        }

        let conversion = quote! {
            let #tuple = event.into();
            let #typed_tuple = event.into();
            let #tuple = #inner_into_tuple;
            Some(#tuple.into())
        }
        ,*/
        let (a, b) = event.into();
        let (a, b): (StringW, StringW) = (a.into(), b.into());
        let (a, b) = (a.into(), b.into());
        Some((a, b).into())
    }
}
*/

// #[test]
// fn test_parser() {
//     let input = r#"
// type ProtocolCollateral @entity {
//   id: ID!
//   collateralToken: Token
//   derivedShit: Fart! @derivedFrom(sdflkasdflkjasdflk)
// }"#;

//     // println!(
//     //     "Graphql to rust enum\n\n\n\n{}",
//     //     parse_graphql_to_rust_enum(input.trim())
//     // );
// }
