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
                    pub fn #update_function_name(tables: &mut Tables, id: &String, #argument_name: &#field_type) {
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

    let unit_struct_name = format_ident!("{}", &name);
    let unit_struct = quote!(
        pub struct #unit_struct_name;
    );

    let table_name = format!("{}", &name);
    let create_function_params = generate_create_params(&fields);
    let create_function_set_statements = generate_create_set_statements(&fields);
    let create_function = quote!(
        pub fn create(tables: &mut Tables, id: &'static str, #create_function_params) {
            let row = tables.create_row(#table_name, id);
            #create_function_set_statements
        }
    );

    let update_functions = generate_update_functions(&table_name, &fields);

    let impl_block = quote!(
        impl #unit_struct_name {

            #create_function

            #update_functions
        }
    );

    quote!(
        #unit_struct
        #impl_block
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
                Some(quote!(#field_name: &#field_type))
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

// pub fn generate_enum_code(definition: &str) -> proc_macro2::TokenStream {
//     let (_, type_def) = type_def(definition).unwrap();

//     let TypeDefinition { name, fields } = type_def;

//     let enum_name = format_ident!("{}", &name);

//     let field_names: Vec<proc_macro2::TokenStream> = fields
//         .clone()
//         .into_iter()
//         .filter_map(|field| {
//             if field.derived_from {
//                 None
//             } else {
//                 let upper_camel_case = format_ident!("{}", upper_camel_case(&field.name));
//                 Some(quote!(#upper_camel_case,))
//             }
//         })
//         .collect();

//     let enum_declaration = quote!(
//         enum #enum_name {
//           #(#field_names)*
//         }
//     );

//     let field_type_match: Vec<proc_macro2::TokenStream> = fields
//         .clone()
//         .into_iter()
//         .filter_map(|field| {
//             if field.derived_from {
//                 None
//             } else {
//                 let field_type = format_ident!("{}", upper_camel_case(&field.name));
//                 let field_type_string = format!("{}", &field.type_name);
//                 Some(quote!(
//                     #enum_name::#field_type => #field_type_string,
//                 ))
//             }
//         })
//         .collect();

//     let field_key_match: Vec<proc_macro2::TokenStream> = fields
//         .into_iter()
//         .filter_map(|field| {
//             if field.derived_from {
//                 None
//             } else {
//                 let field_type = format_ident!("{}", upper_camel_case(&field.name));
//                 let field_name_string = format!("{}", &field.name);
//                 Some(quote!(
//                     #enum_name::#field_type => #field_name_string,
//                 ))
//             }
//         })
//         .collect();

//     let enum_impl = quote!(
//         impl #enum_name {
//             pub fn type_name(&self) -> &'static str {
//                 match self {
//                     #(#field_type_match)*
//                 }
//             }

//             pub fn key(&self) -> &'static str {
//                 match self {
//                     #(#field_key_match)*
//                 }
//             }

//             //pub fn create_entry(tables: &mut Tables, id: &'static str, #generate_required_fields_params(&fields)) {
//                 //tables.create_row(#enum_name::Table.key(), id)
//             //}
//         }
//     );

//     quote! {
//         #enum_declaration
//         #enum_impl
//     }
// }

// fn upper_camel_case(input: &str) -> String {
//     let first_char = input
//         .chars()
//         .next()
//         .unwrap()
//         .to_uppercase()
//         .collect::<String>();
//     format!("{}{}", first_char, &input[1..])
// }
