use std::iter;

use either_n::{Either2, Either3};
use proc_macro2::{Ident, TokenStream};
use quote::format_ident;
use quote::quote;
use syn::{parse_quote, Attribute, Expr, Type};

/// Represents the fields in a structure. This could be the fields on a struct
/// or the fields in the variant of a enum
pub enum Fields<'a> {
    /// `...{ X: String }`
    Named {
        on_structure: Structure<'a>,
        fields: Vec<NamedField<'a>>,
    },
    /// `...(String)`
    Unnamed {
        on_structure: Structure<'a>,
        fields: Vec<UnnamedField<'a>>,
    },
    /// No fields `...`
    Unit { on_structure: Structure<'a> },
}

impl<'a> Fields<'a> {
    pub fn get_structure(&'a self) -> &Structure<'a> {
        match self {
            Fields::Named { on_structure, .. }
            | Fields::Unnamed { on_structure, .. }
            | Fields::Unit { on_structure, .. } => on_structure,
        }
    }

    pub fn get_field_attributes(&'a self) -> impl Iterator<Item = &'a Attribute> {
        self.get_structure().field_attributes()
    }

    /// Get a (exact size) iterator
    pub fn fields_iterator<'b>(
        &'b mut self,
    ) -> impl Iterator<Item = NamedOrUnnamedField<'b, 'a>> + ExactSizeIterator {
        match self {
            Fields::Named { fields, .. } => Either3::One(
                fields
                    .iter_mut()
                    .map(|field| NamedOrUnnamedField::Named(field)),
            ),
            Fields::Unnamed { fields, .. } => Either3::Two(
                fields
                    .iter_mut()
                    .map(|field| NamedOrUnnamedField::Unnamed(field)),
            ),
            Fields::Unit { .. } => Either3::Three(iter::empty()),
        }
    }
}

/// Converts to syn::Fields to syn_helpers::Fields
pub(crate) fn fields_to_structure_fields<'a>(
    fields: &'a syn::Fields,
    structure: Structure<'a>,
) -> Fields<'a> {
    match fields {
        syn::Fields::Named(named_fields) => {
            let named_fields = named_fields
                .named
                .iter()
                .enumerate()
                .map(|(idx, field)| NamedField {
                    attrs: &field.attrs,
                    name: field.ident.as_ref().unwrap(),
                    ty: &field.ty,
                    is_used: false,
                    idx,
                })
                .collect::<Vec<_>>();
            Fields::Named {
                fields: named_fields,
                on_structure: structure,
            }
        }
        syn::Fields::Unnamed(unnamed_fields) => {
            let fields = unnamed_fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(idx, field)| UnnamedField {
                    idx,
                    attrs: &field.attrs,
                    ty: &field.ty,
                    is_used: false,
                })
                .collect::<Vec<_>>();
            Fields::Unnamed {
                fields,
                on_structure: structure,
            }
        }
        syn::Fields::Unit => Fields::Unit {
            on_structure: structure,
        },
    }
}

/// A *Field* is declaration of some data under a object
pub trait Field {
    /// Get a expression which refers to this field. Note that reference takes the form `_` + index of field, this is
    /// to prevent possible clashes with parameter names
    fn get_reference(&mut self) -> Expr;

    /// Same as `Field::get_reference` but a alternate reference for pair building
    fn get_other_reference(&mut self) -> Expr;

    /// Get a pattern for reading this field
    fn get_pattern(&self) -> TokenStream;

    /// For pair building
    fn get_other_pattern(&self) -> TokenStream;

    fn get_attributes(&self) -> &Vec<Attribute>;
}

pub enum NamedOrUnnamedField<'a, 'b> {
    Named(&'a mut NamedField<'b>),
    Unnamed(&'a mut UnnamedField<'b>),
}

impl Field for NamedOrUnnamedField<'_, '_> {
    fn get_reference(&mut self) -> Expr {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_reference(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_reference(),
        }
    }

    fn get_other_reference(&mut self) -> Expr {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_other_reference(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_other_reference(),
        }
    }

    fn get_pattern(&self) -> TokenStream {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_pattern(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_pattern(),
        }
    }

    fn get_other_pattern(&self) -> TokenStream {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_other_pattern(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_other_pattern(),
        }
    }

    fn get_attributes(&self) -> &Vec<Attribute> {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_attributes(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_attributes(),
        }
    }
}

pub struct NamedField<'a> {
    pub name: &'a Ident,
    pub attrs: &'a Vec<Attribute>,
    pub ty: &'a Type,
    idx: usize,
    is_used: bool,
}

impl Field for NamedField<'_> {
    fn get_reference(&mut self) -> Expr {
        self.is_used = true;
        let idx = format_ident!("_{}", self.idx);
        parse_quote!(#idx)
    }

    fn get_other_reference(&mut self) -> Expr {
        self.is_used = true;
        let ident = format_ident!("_{}_other", self.idx);
        parse_quote!(#ident)
    }

    fn get_pattern(&self) -> TokenStream {
        let ident = self.name;
        if self.is_used {
            let reference_name = format_ident!("_{}", self.idx);
            quote!(#ident: #reference_name)
        } else {
            quote!(#ident: _)
        }
    }

    fn get_other_pattern(&self) -> TokenStream {
        let ident = self.name;
        if self.is_used {
            let other_ident = format_ident!("_{}_other", self.idx);
            quote!(#ident: #other_ident)
        } else {
            quote!(#ident: _ )
        }
    }

    fn get_attributes(&self) -> &Vec<Attribute> {
        self.attrs
    }
}

pub struct UnnamedField<'a> {
    pub idx: usize,
    pub attrs: &'a Vec<Attribute>,
    pub ty: &'a Type,
    is_used: bool,
}

impl Field for UnnamedField<'_> {
    fn get_reference(&mut self) -> Expr {
        self.is_used = true;
        let idx = format_ident!("_{}", self.idx);
        parse_quote!(#idx)
    }

    fn get_other_reference(&mut self) -> Expr {
        self.is_used = true;
        let idx = format_ident!("_{}_other", self.idx);
        parse_quote!(#idx)
    }

    fn get_pattern(&self) -> TokenStream {
        if self.is_used {
            let idx = format_ident!("_{}", self.idx);
            quote!(#idx)
        } else {
            quote!(_)
        }
    }

    fn get_other_pattern(&self) -> TokenStream {
        if self.is_used {
            let idx = format_ident!("_{}_other", self.idx);
            quote!(#idx)
        } else {
            quote!(_)
        }
    }

    fn get_attributes(&self) -> &Vec<Attribute> {
        self.attrs
    }
}

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
