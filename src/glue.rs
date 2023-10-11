use anyhow::Error;
use quote::{format_ident, quote};
use syn::{Attribute, Data, Meta, Path};

/// A struct that holds the configuration for a map module
#[derive(Debug)]
pub struct MapConfig {
    pub from_event: syn::Path,
    pub from_function: Option<syn::Path>,
    pub message: syn::Path,
    pub plural_message: syn::Path,
    pub module_name: String,
    pub use_glue: bool,
    pub from_address: Option<String>,
    /// The fields we can't map directly from the event, but need to glue together
    pub glue_fields: Vec<GlueConfig>,
    /// The fields we are omitting from the event or whatever
    pub skipped_fields: Vec<String>,
}

impl MapConfig {
    pub fn from_derive_input(input: &syn::DeriveInput) -> Self {
        let attributes: &Vec<Attribute> = &input.attrs;

        let from_event = get_attribute_value(attributes, "map_config", "from_event")
            .expect("Please provide a from_event attribute, like from_event = \"SomeEvent\",");
        let from_event = syn::parse_str(&from_event).expect(
            "Failed to parse from_event, please make sure this is a valid path to a struct",
        );

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
            .expect(
                "Please provide a plural_message attribute, like plural_message = \"SomeMessage\"",
            );
        let plural_message = syn::parse_str(&plural_message).expect(
            "Failed to parse plural_message, please make sure this is a valid path to a struct",
        );

        let use_glue = get_glue(input);

        let module_name = get_attribute_value(attributes, "map_config", "module_name")
            .expect("Please provide a module_name attribute, like module_name = \"map_approvals\"");

        let from_address = get_attribute_value(attributes, "map_config", "from_address");

        let glue_fields = GlueConfig::struct_fields_glue(input);
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
}

#[derive(Debug)]
pub struct GlueConfig {
    /// the name of the field we are gluing from
    glue_from: String,
    /// the name of the field we are gluing to
    glue_to: String,
    /// The thing we are grabbing from
    glue_type: GlueType,
}

impl GlueConfig {
    /// the way glue should work,
    /// is we attach it to a field
    /// this is also the glue_to value,
    /// the glue_from value is also defaulted to the field name
    /// and the glue_type is the argument to the "glue" attribute
    /// Simple example:
    /// #[glue(event)]
    /// OR
    /// #[glue(function)]
    ///
    /// more complex example:
    /// Note that right now we don't support use like this
    /// #[glue(event, glue_from = "owner")]
    /// #[glue(function, glue_from = "owner")]
    pub fn from_attributes(input: &Vec<Attribute>, field_name: &String) -> Self {
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

        let glue_from =
            get_attribute_value(input, "glue", "glue_from").unwrap_or(field_name.clone());
        let glue_type = GlueType::try_from(glue_type.as_str()).expect("Failed to parse glue_type");
        GlueConfig {
            glue_from,
            glue_to: field_name.clone(),
            glue_type,
        }
    }

    pub fn struct_fields_glue(input: &syn::DeriveInput) -> Vec<Self> {
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
            let glue_config = GlueConfig::from_attributes(
                &field.attrs,
                &field.ident.as_ref().unwrap().to_string(),
            );

            fields.push(glue_config);
        }

        fields
    }

    /// Return conversion from a field, into an IR field
    pub fn intermediate_field(&self) -> proc_macro2::TokenStream {
        let glue_type = format_ident!("{}", self.glue_type.to_str());
        let glue_from = format_ident!("{}", self.glue_from);
        let field = format_ident!("{}", self.glue_to);
        quote! {
            let #field: ::substreams_helpers_traits::StringW = #glue_type.#glue_from.into();
        }
    }

    pub fn field_conversion(&self) -> proc_macro2::TokenStream {
        let field = format_ident!("{}", self.glue_to);
        quote! {
            #field: #field.into()
        }
    }
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

/// Probably will be deprecated soon
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

/// This function implements all of the type conversion required to make maps possible
pub fn impl_from_trait(
    function_path: &Option<Path>,
    event_path: &Path,
    message: &Path,
    plural_message: &Path,
    module_name: &str,
    from_address: Option<String>,
    intermediate_fields: &Vec<proc_macro2::TokenStream>,
    field_conversions: &Vec<proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
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
}
