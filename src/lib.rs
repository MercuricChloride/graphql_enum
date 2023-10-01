extern crate proc_macro;
use nom::{
    bytes::complete::{tag, take_until, take_while},
    character::{
        complete::{alpha1, alphanumeric1, char, multispace0, multispace1},
        is_alphabetic,
    },
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, LitStr};

#[proc_macro]
pub fn graphql_to_enum(input: TokenStream) -> TokenStream {
    let input_str = parse_macro_input!(input as LitStr).value();
    let generated_code = generate_enum_code(&input_str);
    TokenStream::from(generated_code)
}

#[proc_macro]
pub fn helpers_from_graphql(input: TokenStream) -> TokenStream {
    let input_str = parse_macro_input!(input as LitStr).value();
    let generated_code = generate_helper_functions(&input_str);
    TokenStream::from(generated_code)
}

// Define the data structures
#[derive(Debug, Clone)]
struct Field {
    name: String,
    type_name: String,
    required: bool,
    derived_from: bool,
}

#[derive(Debug, Clone)]
struct TypeDef {
    name: String,
    fields: Vec<Field>,
}

fn field_name(input: &str) -> IResult<&str, &str> {
    map(alpha1::<&str, _>, |name| name)(input.trim())
}

fn field_type(input: &str) -> IResult<&str, &str> {
    map(
        tuple((char(':'), multispace0, alpha1)),
        |(_, _, type_name)| type_name,
    )(input.trim())
}

fn is_required(input: &str) -> IResult<&str, bool> {
    map(opt(tag("!")), |opt_tag| opt_tag.is_some())(input.trim())
}

fn is_derived_from(input: &str) -> IResult<&str, bool> {
    let parser = preceded(tag("@derivedFrom("), terminated(take_until(")"), char(')')));

    map(opt(parser), |derived_from| derived_from.is_some())(input.trim())
}

// Parse a field
fn field(input: &str) -> IResult<&str, Field> {
    map(
        tuple((field_name, field_type, is_required, is_derived_from)),
        |(name, type_name, required, derived_from)| Field {
            name: name.to_string(),
            type_name: type_name.to_string(),
            required,
            derived_from,
        },
    )(input.trim())
}

// Parse a type definition
fn type_def(input: &str) -> IResult<&str, TypeDef> {
    map(
        tuple((
            graph_ql_type_name,
            multispace0,
            many0(field),
            multispace0,
            char('}'),
            multispace0,
        )),
        |(name, _, fields, _, _, _)| TypeDef {
            name: name.to_string(),
            fields,
        },
    )(input.trim())
}

fn graph_ql_type_name(input: &str) -> IResult<&str, String> {
    map(
        tuple((
            multispace0::<&str, _>,
            tag("type"),
            multispace1,
            alpha1,
            multispace0,
            tag("@entity"),
            multispace0,
            char('{'),
        )),
        |(_, _, _, name, _, _, _, _)| name.to_string(),
    )(input.trim())
}

fn upper_camel_case(input: &str) -> String {
    let first_char = input
        .chars()
        .next()
        .unwrap()
        .to_uppercase()
        .collect::<String>();
    format!("{}{}", first_char, &input[1..])
}

fn camel_to_snake_case(input: &str) -> String {
    let mut result = String::new();
    for (i, c) in input.chars().enumerate() {
        if c.is_uppercase() {
            if i != 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

#[test]
fn test_camel_to_snake() {
    assert_eq!(camel_to_snake_case("camelCase"), "camel_case");
    assert_eq!(camel_to_snake_case("CamelCase"), "camel_case");
    assert_eq!(camel_to_snake_case("camelcase"), "camelcase");
    assert_eq!(camel_to_snake_case("camel_case"), "camel_case");
    assert_eq!(camel_to_snake_case("Camel_Case"), "camel__case");
}

fn field_type_to_ident(field_type: &str) -> Ident {
    match field_type {
        "Int" => format_ident!("i32"),
        "Float" => format_ident!("f32"),
        "Boolean" => format_ident!("bool"),
        _ => format_ident!("String"),
    }
}

fn generate_create_params(fields: &Vec<Field>) -> proc_macro2::TokenStream {
    let field_types: Vec<proc_macro2::TokenStream> = fields
        .iter()
        .filter_map(|field| {
            if field.type_name == "ID" || !field.required || field.derived_from {
                None
            } else {
                let field_type = field_type_to_ident(&field.type_name);
                let snake_name = camel_to_snake_case(&field.name);
                let field_name = format_ident!("{}", snake_name);
                Some(quote!(#field_name: &#field_type))
            }
        })
        .collect();

    quote! {
        #(#field_types),*
    }
}

fn generate_set_statement(field: &Field) -> proc_macro2::TokenStream {
    let key = format!("{}", &field.name);
    let value = format_ident!("{}", camel_to_snake_case(&field.name));
    match field.type_name.as_str() {
        "BigInt" => {
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
            if field.type_name == "ID" || !field.required || field.derived_from {
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

fn generate_update_functions(table_name: &String, fields: &Vec<Field>) -> proc_macro2::TokenStream {
    let update_functions: Vec<proc_macro2::TokenStream> = fields
        .iter()
        .filter_map(|field| {
            if field.type_name == "ID" || field.derived_from {
                None
            } else {
                let field_type = field_type_to_ident(&field.type_name);
                let table_name = format!("{}", table_name);
                let snake_name = camel_to_snake_case(&field.name);
                let argument_name = format_ident!("{}", snake_name);
                let update_function_name = format_ident!("update_{}", snake_name);

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

fn generate_helper_functions(input: &str) -> proc_macro2::TokenStream {
    let (_, type_def) = type_def(input).unwrap();
    let TypeDef { name, fields } = type_def;

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

fn generate_enum_code(definition: &str) -> proc_macro2::TokenStream {
    let (_, type_def) = type_def(definition).unwrap();

    let TypeDef { name, fields } = type_def;

    let enum_name = format_ident!("{}", &name);

    let field_names: Vec<proc_macro2::TokenStream> = fields
        .clone()
        .into_iter()
        .filter_map(|field| {
            if field.derived_from {
                None
            } else {
                let upper_camel_case = format_ident!("{}", upper_camel_case(&field.name));
                Some(quote!(#upper_camel_case,))
            }
        })
        .collect();

    let enum_declaration = quote!(
        enum #enum_name {
          #(#field_names)*
        }
    );

    let field_type_match: Vec<proc_macro2::TokenStream> = fields
        .clone()
        .into_iter()
        .filter_map(|field| {
            if field.derived_from {
                None
            } else {
                let field_type = format_ident!("{}", upper_camel_case(&field.name));
                let field_type_string = format!("{}", &field.type_name);
                Some(quote!(
                    #enum_name::#field_type => #field_type_string,
                ))
            }
        })
        .collect();

    let field_key_match: Vec<proc_macro2::TokenStream> = fields
        .into_iter()
        .filter_map(|field| {
            if field.derived_from {
                None
            } else {
                let field_type = format_ident!("{}", upper_camel_case(&field.name));
                let field_name_string = format!("{}", &field.name);
                Some(quote!(
                    #enum_name::#field_type => #field_name_string,
                ))
            }
        })
        .collect();

    let enum_impl = quote!(
        impl #enum_name {
            pub fn type_name(&self) -> &'static str {
                match self {
                    #(#field_type_match)*
                }
            }

            pub fn key(&self) -> &'static str {
                match self {
                    #(#field_key_match)*
                }
            }

            //pub fn create_entry(tables: &mut Tables, id: &'static str, #generate_required_fields_params(&fields)) {
                //tables.create_row(#enum_name::Table.key(), id)
            //}
        }
    );

    quote! {
        #enum_declaration
        #enum_impl
    }
}

#[test]
fn test_parser() {
    let input = r#"
type ProtocolCollateral @entity {
  id: ID!
  collateralToken: Token
  derivedShit: Fart! @derivedFrom(sdflkasdflkjasdflk)
}"#;

    // println!(
    //     "Graphql to rust enum\n\n\n\n{}",
    //     parse_graphql_to_rust_enum(input.trim())
    // );
}
