extern crate proc_macro;
use codegen::{graphql::generate_helper_functions, map_helpers::generate_map_module};
use proc_macro::TokenStream;
use proc_macro2::Group;
use quote::{format_ident, quote};
use substreams_helpers_traits::FromBlock;
use syn::{
    parse_macro_input, Attribute, AttributeArgs, Data, DataStruct, DeriveInput, File, Item,
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

//#[proc_macro_derive(Map, attributes(from_event, message, plural_message, module_name))]
#[proc_macro_derive(Map, attributes(map_config))]
pub fn derive_map(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let mut from_event = None;
    let mut message = None;
    let mut plural_message = None;
    let mut module_name = None;
    let mut from_address = None;
    for attr in &input.attrs {
        if attr.path.is_ident("map_config") {
            if let Ok(meta) = attr.parse_meta() {
                if let Meta::List(list) = meta {
                    for nested in list.nested {
                        if let syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) = nested {
                            let ident = nv.path.get_ident().map(|i| i.to_string());
                            match ident.as_deref() {
                                Some("from_event") => {
                                    if let syn::Lit::Str(lit_str) = &nv.lit {
                                        from_event = Some(lit_str.value());
                                    }
                                }
                                Some("message") => {
                                    if let syn::Lit::Str(lit_str) = &nv.lit {
                                        message = Some(lit_str.value());
                                    }
                                }
                                Some("plural_message") => {
                                    if let syn::Lit::Str(lit_str) = &nv.lit {
                                        plural_message = Some(lit_str.value());
                                    }
                                }
                                Some("module_name") => {
                                    if let syn::Lit::Str(lit_str) = &nv.lit {
                                        module_name = Some(lit_str.value());
                                    }
                                }
                                Some("from_address") => {
                                    if let syn::Lit::Str(lit_str) = &nv.lit {
                                        from_address = Some(lit_str.value());
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
    }

    let from_event = from_event
        .expect("Please provide a from_event attribute, like from_event = \"SomeEvent\",");
    let from_event: syn::Path =
        syn::parse_str(&from_event).expect("from_event must be a path to a struct");

    let message =
        message.expect("Please provide a message attribute, like message = \"SomeMessage\"");
    let message: syn::Path = syn::parse_str(&message).expect("message must be a path");

    let plural_message = plural_message
        .expect("Please provide a plural_message attribute, like plural_message = \"SomeMessage\"");
    let plural_message: syn::Path =
        syn::parse_str(&plural_message).expect("plural_message must be a path");

    let module_name = module_name
        .expect("Please provide a module_name attribute, like module_name = \"map_approvals\"");

    impl_conversion_traits(
        &input,
        &from_event,
        &message,
        &plural_message,
        module_name,
        from_address,
    )
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
