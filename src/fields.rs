use std::iter;

use either_n::Either3;
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use quote::{format_ident, ToTokens};
use syn::{Attribute, Expr, Type};

use crate::structure::Structure;

/// Represents the fields in a structure. This could be the fields on a struct
/// or the fields in the variant of a enum
pub enum Fields<'a> {
    /// `**name { X: String }`
    Named {
        on_structure: Structure<'a>,
        fields: Vec<NamedField<'a>>,
    },
    /// `**name(String)`
    Unnamed {
        on_structure: Structure<'a>,
        fields: Vec<UnnamedField<'a>>,
    },
    /// No fields `*name*`
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

    pub fn fields_iterator<'b>(
        &'b mut self,
    ) -> impl Iterator<Item = NamedOrUnnamedField<'b, 'a>> + ExactSizeIterator {
        match self {
            Fields::Named { fields, .. } => {
                Either3::One(fields.iter_mut().map(NamedOrUnnamedField::Named))
            }
            Fields::Unnamed { fields, .. } => {
                Either3::Two(fields.iter_mut().map(NamedOrUnnamedField::Unnamed))
            }
            Fields::Unit { .. } => Either3::Three(iter::empty()),
        }
    }

    pub fn to_pattern(&self, path: &syn::Path, variant: crate::Variant) -> TokenStream {
        match self {
            Fields::Named {
                on_structure: _,
                fields,
            } => {
                let fields_pattern = fields.iter().map(|field| field.get_pattern(variant));

                quote!(#path { #(#fields_pattern),* })
            }
            Fields::Unnamed {
                on_structure: _,
                fields,
            } => {
                let fields_pattern = fields.iter().map(|field| field.get_pattern(variant));

                quote!(#path(#(#fields_pattern),*))
            }
            Fields::Unit { .. } => quote!(#path),
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
                    used_for_trait: false,
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
                    used_for_trait: false,
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
    /// to prevent possible clashes with parameter names.
    /// Use [Field::get_reference_with_config] to for different options
    fn get_reference(&mut self) -> Expr {
        Self::get_reference_with_config(self, Variant::Primary, true)
    }

    /// For pairs
    fn get_reference_pair(&mut self) -> (Expr, Expr) {
        (
            Self::get_reference_with_config(self, Variant::Primary, true),
            Self::get_reference_with_config(self, Variant::Secondary, true),
        )
    }

    fn get_reference_with_config(&mut self, variant: Variant, used_for_trait: bool) -> Expr;

    /// Get a pattern for reading this field
    fn get_pattern(&self, variant: Variant) -> TokenStream;

    fn get_type_that_needs_constraint(&self) -> Option<Type>;

    fn get_attributes(&self) -> &[Attribute];
}

pub enum NamedOrUnnamedField<'a, 'b> {
    Named(&'a mut NamedField<'b>),
    Unnamed(&'a mut UnnamedField<'b>),
}

impl Field for NamedOrUnnamedField<'_, '_> {
    fn get_reference_with_config(&mut self, variant: Variant, used_for_trait: bool) -> Expr {
        match self {
            NamedOrUnnamedField::Named(named) => {
                named.get_reference_with_config(variant, used_for_trait)
            }
            NamedOrUnnamedField::Unnamed(unnamed) => {
                unnamed.get_reference_with_config(variant, used_for_trait)
            }
        }
    }

    fn get_pattern(&self, variant: Variant) -> TokenStream {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_pattern(variant),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_pattern(variant),
        }
    }

    fn get_attributes(&self) -> &[Attribute] {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_attributes(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_attributes(),
        }
    }

    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_type_that_needs_constraint(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_type_that_needs_constraint(),
        }
    }
}

pub struct NamedField<'a> {
    pub name: &'a Ident,
    pub attrs: &'a Vec<Attribute>,
    pub ty: &'a Type,
    idx: usize,
    is_used: bool,
    used_for_trait: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Variant {
    Primary,
    Secondary,
}

impl Variant {
    fn get_ident(self, idx: usize) -> Ident {
        match self {
            Variant::Primary => format_ident!("_{}", idx),
            Variant::Secondary => format_ident!("_{}_other", idx),
        }
    }
}

impl Field for NamedField<'_> {
    fn get_reference_with_config(&mut self, variant: Variant, used_for_trait: bool) -> Expr {
        self.is_used = true;
        self.used_for_trait |= used_for_trait;
        syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: variant.get_ident(self.idx).into(),
        }
        .into()
    }

    fn get_pattern(&self, variant: Variant) -> TokenStream {
        let ident = self.name;
        if self.is_used {
            let reference_name = variant.get_ident(self.idx);
            quote!(#ident: #reference_name)
        } else {
            quote!(#ident: _)
        }
    }

    fn get_attributes(&self) -> &[Attribute] {
        self.attrs
    }

    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        self.used_for_trait.then(|| self.ty.clone())
    }
}

pub struct UnnamedField<'a> {
    pub idx: usize,
    pub attrs: &'a Vec<Attribute>,
    pub ty: &'a Type,
    is_used: bool,
    used_for_trait: bool,
}

impl Field for UnnamedField<'_> {
    fn get_reference_with_config(&mut self, variant: Variant, used_for_trait: bool) -> Expr {
        self.is_used = true;
        self.used_for_trait |= used_for_trait;
        syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: variant.get_ident(self.idx).into(),
        }
        .into()
    }

    fn get_pattern(&self, variant: Variant) -> TokenStream {
        if self.is_used {
            variant.get_ident(self.idx).to_token_stream()
        } else {
            quote!(_)
        }
    }

    fn get_attributes(&self) -> &[Attribute] {
        self.attrs
    }

    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        self.used_for_trait.then(|| self.ty.clone())
    }
}
