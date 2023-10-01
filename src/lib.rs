extern crate proc_macro;
use codegen::generate_helper_functions;
use proc_macro::TokenStream;
use syn::{parse_macro_input, LitStr};

mod codegen;
mod parser;

// #[proc_macro]
// pub fn graphql_to_enum(input: TokenStream) -> TokenStream {
//     let input_str = parse_macro_input!(input as LitStr).value();
//     let generated_code = generate_enum_code(&input_str);
//     TokenStream::from(generated_code)
// }

#[proc_macro]
pub fn helpers_from_graphql(input: TokenStream) -> TokenStream {
    let input_str = parse_macro_input!(input as LitStr).value();
    let generated_code = generate_helper_functions(&input_str);
    TokenStream::from(generated_code)
}

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
