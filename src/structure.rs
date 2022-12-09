use either_n::Either2;
use proc_macro2::Ident;
use syn::Attribute;

/// The data type the field is under
pub enum Structure<'a> {
    Struct {
        struct_name: &'a Ident,
        struct_attrs: &'a Vec<Attribute>,
    },
    EnumVariant {
        enum_name: &'a Ident,
        enum_attrs: &'a Vec<Attribute>,
        variant_name: &'a Ident,
        variant_attrs: &'a Vec<Attribute>,
    },
}

impl<'a> Structure<'a> {
    /// Returns attributes on the declaration of the fields AND if a enum on the enum item
    pub fn all_attributes(&'a self) -> impl Iterator<Item = &'a Attribute> {
        match self {
            Structure::Struct { struct_attrs, .. } => Either2::One(struct_attrs.iter()),
            Structure::EnumVariant {
                enum_attrs,
                variant_attrs,
                ..
            } => Either2::Two(enum_attrs.iter().chain(variant_attrs.iter())),
        }
    }

    /// Returns attributes on the declaration of the fields
    pub fn field_attributes(&'a self) -> impl Iterator<Item = &'a Attribute> {
        match self {
            Structure::Struct {
                struct_attrs: attrs,
                ..
            }
            | Structure::EnumVariant {
                variant_attrs: attrs,
                ..
            } => attrs.iter(),
        }
    }

    /// Struct name or just the variant
    pub fn own_name(&self) -> String {
        match self {
            Structure::Struct {
                struct_name: name, ..
            }
            | Structure::EnumVariant {
                variant_name: name, ..
            } => name.to_string(),
        }
    }

    /// Struct name or enum and variant name concatenated with double colon
    pub fn full_name(&self) -> String {
        match self {
            Structure::Struct { struct_name, .. } => struct_name.to_string(),
            Structure::EnumVariant {
                enum_name,
                variant_name,
                ..
            } => format!("{}::{}", enum_name, variant_name),
        }
    }
}
