pub mod graphql {
    use crate::parser::*;
    use quote::{format_ident, quote};

    pub fn generate_update_functions(
        table_name: &String,
        fields: &Vec<Field>,
    ) -> proc_macro2::TokenStream {
        let update_functions: Vec<proc_macro2::TokenStream> = fields
        .iter()
        .filter_map(|field| {
            if field.is_id() || field.derived_from() {
                None
            } else {
                let table_name = format!("{}", table_name);
                let argument_name = format_ident!("r#{}", field.snake_case_name());
                let field_type = field.rust_type();
                let update_function_name = format_ident!("update_{}", field.snake_case_name());

                let set_statement = generate_set_statement(field);

                Some(quote!(
                    pub fn #update_function_name(tables: &mut Tables, id: &String, #argument_name: #field_type) {
                        let row = tables.update_row(#table_name, id);
                        #set_statement
                    }
                ))
            }
        })
        .collect();

        quote! {
            #(#update_functions)*
        }
    }

    pub fn generate_helper_functions(input: &str) -> proc_macro2::TokenStream {
        let type_def = parse_graphql_type(input).unwrap();
        let TypeDefinition { name, fields } = type_def;

        let module_name = format_ident!("{}Helpers", &name);

        let table_name = format!("{}", &name);
        let create_function_params = generate_create_params(&fields);
        let create_function_set_statements = generate_create_set_statements(&fields);
        let create_function = quote!(
            pub fn create(tables: &mut Tables, id: &String, #create_function_params) {
                let row = tables.create_row(#table_name, id);
                #create_function_set_statements
            }
        );

        let update_functions = generate_update_functions(&table_name, &fields);

        let module_definition = quote!(
            pub mod #module_name {
                use substreams_entity_change::tables::Tables;

                #create_function

                #update_functions
            }
        );

        quote!(
            #module_definition
        )
    }

    fn generate_create_params(fields: &Vec<Field>) -> proc_macro2::TokenStream {
        let field_types: Vec<proc_macro2::TokenStream> = fields
            .iter()
            .filter_map(|field| {
                if field.is_id() || !field.required() || field.derived_from() {
                    None
                } else {
                    let field_type = field.rust_type();
                    let field_name = format_ident!("r#{}", field.snake_case_name());
                    Some(quote!(#field_name: #field_type))
                }
            })
            .collect();

        quote! {
            #(#field_types),*
        }
    }

    fn generate_set_statement(field: &Field) -> proc_macro2::TokenStream {
        let key = format!("{}", &field.name());
        let value = format_ident!("r#{}", &field.snake_case_name());
        match field.field_type() {
            GraphQlType::BigInt { .. } => {
                quote!(
                    row.set_bigint(#key, #value);
                )
            }
            _ => {
                quote!(
                    row.set(#key, #value);
                )
            }
        }
    }

    fn generate_create_set_statements(fields: &Vec<Field>) -> proc_macro2::TokenStream {
        let set_statements: Vec<proc_macro2::TokenStream> = fields
            .iter()
            .filter_map(|field| {
                if field.is_id() || !field.required() || field.derived_from() {
                    None
                } else {
                    Some(generate_set_statement(&field))
                }
            })
            .collect();

        quote! {
            #(#set_statements)*
        }
    }
}

pub mod map_helpers {
    use proc_macro2::{Ident, TokenStream};
    use quote::{format_ident, quote};

    pub fn generate_map_module(
        module_name: Ident,
        event: Ident,
        output_type: Ident,
        repeated_field: Ident,
        address: Option<String>,
    ) -> TokenStream {
        let substreams_block = format_ident!("{}", "::substreams_ethereum::pb::eth::v2::Block");
        let substreams_error = format_ident!("{}", "::substreams::errors::Error");
        let address = match address {
            Some(a) => quote!(
                Some(#a)
            ),
            None => quote!(None),
        };

        quote!(
            #[::substreams::handlers::map]
            fn #module_name(block: #substreams_block) -> Result<#output_type, #substreams_error> {
                let events = #event::from_block(block, #address);

                Ok(#output_type { #repeated_field: events })
            }
        )
    }
}
