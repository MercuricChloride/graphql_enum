use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, char, multispace0, multispace1},
    combinator::{map, opt},
    multi::many0,
    sequence::{preceded, terminated, tuple},
    IResult,
};
use proc_macro2::Ident;
use quote::format_ident;

use anyhow::Error;

/// A graphql type definition
#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: String,
    pub fields: Vec<Field>,
}

/// A graphql Field
#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    field_type: GraphQlType,
}

impl Field {
    pub fn snake_case_name(&self) -> String {
        camel_to_snake_case(&self.name)
    }

    pub fn field_type(&self) -> &GraphQlType {
        &self.field_type
    }

    pub fn is_id(&self) -> bool {
        match self.field_type {
            GraphQlType::Id => true,
            _ => false,
        }
    }

    pub fn required(&self) -> bool {
        match self.field_type {
            GraphQlType::Id
            | GraphQlType::String { required: true, .. }
            | GraphQlType::Bytes { required: true, .. }
            | GraphQlType::Boolean { required: true, .. }
            | GraphQlType::BigInt { required: true, .. }
            | GraphQlType::Int { required: true, .. }
            | GraphQlType::Array { required: true, .. } => true,
            _ => false,
        }
    }

    pub fn derived_from(&self) -> bool {
        match self.field_type {
            GraphQlType::String {
                derived_from: true, ..
            }
            | GraphQlType::Bytes {
                derived_from: true, ..
            }
            | GraphQlType::Boolean {
                derived_from: true, ..
            }
            | GraphQlType::BigInt {
                derived_from: true, ..
            }
            | GraphQlType::Int {
                derived_from: true, ..
            }
            | GraphQlType::Array {
                derived_from: true, ..
            } => true,
            _ => false,
        }
    }
    pub fn rust_type(&self) -> Ident {
        self.field_type.rust_type()
    }
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

/// An enum that models graphql types and if they are required
#[derive(Debug, Clone)]
pub enum GraphQlType {
    Id,
    String {
        required: bool,
        derived_from: bool,
    },
    Bytes {
        required: bool,
        derived_from: bool,
    },
    Boolean {
        required: bool,
        derived_from: bool,
    },
    BigInt {
        required: bool,
        derived_from: bool,
    },
    Int {
        required: bool,
        derived_from: bool,
    },
    Array {
        required: bool,
        derived_from: bool,
        internal_type: Box<GraphQlType>,
    },
}

impl GraphQlType {
    pub fn new(
        required: bool,
        derived_from: bool,
        type_name: &str,
        internal_type: Option<Box<GraphQlType>>,
    ) -> Self {
        match type_name {
            "ID" => GraphQlType::Id,
            "String" => GraphQlType::String {
                required,
                derived_from,
            },
            "Bytes" => GraphQlType::Bytes {
                required,
                derived_from,
            },
            "Boolean" => GraphQlType::Boolean {
                required,
                derived_from,
            },
            "BigInt" => GraphQlType::BigInt {
                required,
                derived_from,
            },
            "Int" => GraphQlType::Int {
                required,
                derived_from,
            },
            "Array" => GraphQlType::Array {
                required,
                derived_from,
                internal_type: internal_type.unwrap(),
            },
            _ => panic!("Unknown GraphQl type {}", type_name),
        }
    }

    pub fn rust_type(&self) -> Ident {
        match self {
            GraphQlType::Id => format_ident!("String"),
            GraphQlType::String { .. } => {
                format_ident!("String")
            }
            GraphQlType::Bytes { .. } => {
                format_ident!("Vec<u8>")
            }
            GraphQlType::Boolean { .. } => {
                format_ident!("bool")
            }
            GraphQlType::BigInt { .. } => {
                format_ident!("String")
            }
            GraphQlType::Int { .. } => {
                format_ident!("i32")
            }
            GraphQlType::Array { internal_type, .. } => {
                format_ident!("Vec<{}>", internal_type.rust_type())
            }
        }
    }
}

// Parse a type definition
pub fn parse_graphql_type(input: &str) -> Result<TypeDefinition, Error> {
    let result = map(
        tuple((
            graph_ql_type_name,
            multispace0,
            many0(field),
            multispace0,
            char('}'),
            multispace0,
        )),
        |(name, _, fields, _, _, _)| TypeDefinition {
            name: name.to_string(),
            fields,
        },
    )(input.trim());

    match result {
        Ok((remaining, type_definition)) => {
            if remaining.len() > 0 {
                let message = stringify!("Couldn't fully parse graphql type definition. Go yell at @blind_nabler to fix this!: {}", remaining);
                return Err(Error::msg(message));
            }
            Ok(type_definition)
        }
        Err(err) => {
            let message = format!("Failed to parse graphql type definition. Go yell at @blind_nabler to fix this!: {}", err);
            Err(Error::msg(message))
        }
    }
}

// Parse a field
fn field(input: &str) -> IResult<&str, Field> {
    map(
        tuple((field_name, alt((array_type, field_type)))),
        |(name, field_type)| Field {
            name: name.to_string(),
            field_type,
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

fn field_name(input: &str) -> IResult<&str, &str> {
    map(alpha1::<&str, _>, |name| name)(input.trim())
}

fn field_type(input: &str) -> IResult<&str, GraphQlType> {
    // TODO Add support for arrays
    map(
        tuple((char(':'), multispace0, alpha1, is_required, is_derived_from)),
        |(_, _, type_name, required, derived_from)| {
            GraphQlType::new(required, derived_from, type_name, None)
        },
    )(input.trim())
}

fn array_type(input: &str) -> IResult<&str, GraphQlType> {
    map(
        tuple((
            char(':'),
            multispace0,
            tag("["),
            alpha1,
            is_required,
            tag("]"),
            is_required,
            is_derived_from,
        )),
        |(_, _, _, inner_type_name, inner_required, _, outer_required, derived_from)| {
            let inner_type = GraphQlType::new(inner_required, derived_from, inner_type_name, None);
            let array_type = GraphQlType::new(
                outer_required,
                derived_from,
                "Array",
                Some(Box::new(inner_type)),
            );
            array_type
        },
    )(input.trim())
}

fn is_required(input: &str) -> IResult<&str, bool> {
    map(opt(tag("!")), |opt_tag| opt_tag.is_some())(input.trim())
}

fn is_derived_from(input: &str) -> IResult<&str, bool> {
    let parser = preceded(tag("@derivedFrom("), terminated(take_until(")"), char(')')));

    map(opt(parser), |derived_from| derived_from.is_some())(input.trim())
}

#[test]
fn test_type_def() {
    let input = r#"
type SimpleType @entity {
    id: ID!
    name: String!
    bytes: Bytes!
    boolean: Boolean!
    bigInt: BigInt!
    int: Int!
    array: [String!]!
}"#;

    let type_def = parse_graphql_type(input).unwrap();

    assert_eq!(type_def.name, "SimpleType");
    assert_eq!(type_def.fields.len(), 7);
}
