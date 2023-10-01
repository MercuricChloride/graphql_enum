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

fn generate_enum_code(definition: &str) -> proc_macro2::TokenStream {
    let (_, type_def) = type_def(definition).unwrap();

    let TypeDef { name, fields } = type_def;

    let fields = fields
        .into_iter()
        .map(|field| Field {
            name: upper_camel_case(&field.name),
            type_name: field.type_name,
            required: field.required,
            derived_from: field.derived_from,
        })
        .collect::<Vec<Field>>();

    let enum_name = format_ident!("{}", &name);

    let field_names: Vec<proc_macro2::TokenStream> = fields
        .clone()
        .into_iter()
        .filter_map(|field| {
            if field.derived_from {
                None
            } else {
                let upper_camel_case = format_ident!("{}", &field.name);
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
        .into_iter()
        .filter_map(|field| {
            if field.derived_from {
                None
            } else {
                let field_type = format_ident!("{}", &field.name);
                let field_type_string = format!("{}", &field.type_name);
                Some(quote!(
                    #enum_name::#field_type => #field_type_string,
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

    println!(
        "Graphql to rust enum\n\n\n\n{}",
        parse_graphql_to_rust_enum(input.trim())
    );
}
