use std::{error::Error, iter};

use either_n::Either3;
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{parse_quote, Attribute, Expr, ExprUnary, Path, Token, Type};

use crate::{HasAttributes, TypeOfSelf};

/// Represents the fields in a structure. This could be the fields on a struct
/// or the fields in the variant of a enum
pub enum Fields {
    /// `*name* { X: String }`
    Named(Vec<NamedField>, Vec<Attribute>),
    /// `*name*(String)`
    Unnamed(Vec<UnnamedField>, Vec<Attribute>),
    /// No fields `*name*`
    Unit(Vec<Attribute>),
}

impl Fields {
    pub fn get_field_attributes(&self) -> &[Attribute] {
        match self {
            Fields::Named(_, attributes)
            | Fields::Unnamed(_, attributes)
            | Fields::Unit(attributes) => attributes,
        }
    }

    pub fn fields_iterator(&self) -> impl ExactSizeIterator<Item = NamedOrUnnamedField<'_>> {
        match self {
            Fields::Named(fields, ..) => {
                Either3::One(fields.iter().map(NamedOrUnnamedField::Named))
            }
            Fields::Unnamed(fields, ..) => {
                Either3::Two(fields.iter().map(NamedOrUnnamedField::Unnamed))
            }
            Fields::Unit(..) => Either3::Three(iter::empty()),
        }
    }

    pub fn fields_iterator_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = NamedOrUnnamedFieldMut<'_>> {
        match self {
            Fields::Named(fields, ..) => {
                Either3::One(fields.iter_mut().map(NamedOrUnnamedFieldMut::Named))
            }
            Fields::Unnamed(fields, ..) => {
                Either3::Two(fields.iter_mut().map(NamedOrUnnamedFieldMut::Unnamed))
            }
            Fields::Unit(..) => Either3::Three(iter::empty()),
        }
    }

    pub fn to_pattern(&self, path: Path, type_of_self: TypeOfSelf) -> TokenStream {
        Self::to_pattern_with_config(self, path, type_of_self, "")
    }

    pub fn to_pattern_with_config(
        &self,
        path: Path,
        type_of_self: TypeOfSelf,
        name_postfix: &'static str,
    ) -> TokenStream {
        match self {
            Fields::Named(fields, ..) => {
                let fields_pattern = fields
                    .iter()
                    .map(|field| field.get_pattern_with_config(type_of_self, name_postfix));
                quote!(#path { #(#fields_pattern),* })
            }
            Fields::Unnamed(fields, ..) => {
                let fields_pattern = fields
                    .iter()
                    .map(|field| field.get_pattern_with_config(type_of_self, name_postfix));
                quote!(#path(#(#fields_pattern),*))
            }
            Fields::Unit(..) => quote!(#path),
        }
    }

    pub fn to_constructor<'b>(
        &'b self,
        generator: impl Fn(NamedOrUnnamedField<'b>) -> Result<Expr, Box<dyn Error>>,
        constructor: Path,
    ) -> Result<Expr, Box<dyn Error>> {
        match self {
            Fields::Named(fields, ..) => {
                let arguments = fields
                    .iter()
                    .map(|field| {
                        generator(NamedOrUnnamedField::Named(field)).map(|value| {
                            let field_name = field.name.clone();
                            quote! { #field_name: #value }
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(parse_quote! { #constructor { #(#arguments),* } })
            }
            Fields::Unnamed(fields, ..) => {
                let arguments = fields
                    .iter()
                    .map(|field| generator(NamedOrUnnamedField::Unnamed(field)))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(parse_quote! { #constructor (#(#arguments),*) })
            }
            Fields::Unit(..) => Ok(parse_quote! { #constructor }),
        }
    }

    /// Will just return `None` if member is not of matching form. Also will return `None` if cannot find the field
    pub fn get_field_by_member(&self, member: syn::Member) -> Option<NamedOrUnnamedField> {
        match self {
            Fields::Named(named, _) => {
                if let syn::Member::Named(ident) = member {
                    named
                        .iter()
                        .find(|named| named.name == ident)
                        .map(NamedOrUnnamedField::Named)
                } else {
                    None
                }
            }
            Fields::Unnamed(unnamed, _) => {
                if let syn::Member::Unnamed(idx) = member {
                    unnamed
                        .get(idx.index as usize)
                        .map(NamedOrUnnamedField::Unnamed)
                } else {
                    None
                }
            }
            Fields::Unit(_) => None,
        }
    }

    /// Will just return `None` if member is not of matching form. Also will return `None` if cannot find the field
    pub fn get_field_by_member_mut(
        &mut self,
        member: syn::Member,
    ) -> Option<NamedOrUnnamedFieldMut> {
        match self {
            Fields::Named(named, _) => {
                if let syn::Member::Named(ident) = member {
                    named
                        .iter_mut()
                        .find(|named| named.name == ident)
                        .map(NamedOrUnnamedFieldMut::Named)
                } else {
                    None
                }
            }
            Fields::Unnamed(unnamed, _) => {
                if let syn::Member::Unnamed(idx) = member {
                    unnamed
                        .get_mut(idx.index as usize)
                        .map(NamedOrUnnamedFieldMut::Unnamed)
                } else {
                    None
                }
            }
            Fields::Unit(_) => None,
        }
    }
}

/// Converts to syn::Fields to syn_helpers::Fields
pub(crate) fn syn_fields_to_fields(fields: syn::Fields, attributes: Vec<syn::Attribute>) -> Fields {
    match fields {
        syn::Fields::Named(named_fields) => {
            let named_fields = named_fields
                .named
                .into_iter()
                .enumerate()
                .map(|(idx, field)| NamedField {
                    attrs: field.attrs,
                    name: field.ident.unwrap(),
                    ty: field.ty,
                    is_used: false,
                    used_for_trait: false,
                    idx,
                })
                .collect::<Vec<_>>();

            Fields::Named(named_fields, attributes)
        }
        syn::Fields::Unnamed(unnamed_fields) => {
            let fields = unnamed_fields
                .unnamed
                .into_iter()
                .enumerate()
                .map(|(idx, field)| UnnamedField {
                    idx,
                    attrs: field.attrs,
                    ty: field.ty,
                    is_used: false,
                    used_for_trait: false,
                })
                .collect::<Vec<_>>();

            Fields::Unnamed(fields, attributes)
        }
        syn::Fields::Unit => Fields::Unit(attributes),
    }
}

/// A *Field* is declaration of some data under a object
pub trait Field: HasAttributes {
    /// Returns true if this field matches the same type
    #[cfg(feature = "field_is_type")]
    fn is_type(&self, ty: &Type) -> bool {
        self.get_type() == ty
    }

    fn get_type(&self) -> &Type;

    /// Get a pattern for reading this field
    fn get_pattern(&self, type_of_self: TypeOfSelf) -> TokenStream {
        Self::get_pattern_with_config(self, type_of_self, "")
    }

    /// Get pattern with configuration for the ownership (with [TypeOfSelf] and whether to prefix the identifier)
    fn get_pattern_with_config(
        &self,
        type_of_self: TypeOfSelf,
        name_postfix: &'static str,
    ) -> TokenStream;

    /// Returns the fields type **if** it used for the trait (only used internally)
    fn get_type_that_needs_constraint(&self) -> Option<Type>;
}

/// Getting a reference to the field is recorded
pub trait FieldMut: Field {
    /// Get a expression which refers to this field. Note that reference takes the form `_` + index of field, this is
    /// to prevent possible clashes with parameter names.
    /// Use [FieldMut::get_reference_with_config] to for different options
    fn get_reference(&mut self) -> Expr {
        Self::get_reference_with_config(self, true, "")
    }

    fn get_reference_with_config(
        &mut self,
        used_for_trait: bool,
        name_postfix: &'static str,
    ) -> Expr;
}

/// Either [NamedField] or [UnnamedField]
pub enum NamedOrUnnamedField<'a> {
    Named(&'a NamedField),
    Unnamed(&'a UnnamedField),
}

/// Either [NamedField] or [UnnamedField]
pub enum NamedOrUnnamedFieldMut<'a> {
    Named(&'a mut NamedField),
    Unnamed(&'a mut UnnamedField),
}

impl HasAttributes for NamedOrUnnamedField<'_> {
    fn get_attributes(&self) -> &[Attribute] {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_attributes(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_attributes(),
        }
    }
}

impl Field for NamedOrUnnamedField<'_> {
    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_type_that_needs_constraint(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_type_that_needs_constraint(),
        }
    }

    fn get_pattern_with_config(
        &self,
        type_of_self: TypeOfSelf,
        name_postfix: &'static str,
    ) -> TokenStream {
        match self {
            NamedOrUnnamedField::Named(named) => {
                named.get_pattern_with_config(type_of_self, name_postfix)
            }
            NamedOrUnnamedField::Unnamed(unnamed) => {
                unnamed.get_pattern_with_config(type_of_self, name_postfix)
            }
        }
    }

    fn get_type(&self) -> &Type {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_type(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_type(),
        }
    }
}

impl HasAttributes for NamedOrUnnamedFieldMut<'_> {
    fn get_attributes(&self) -> &[Attribute] {
        match self {
            NamedOrUnnamedFieldMut::Named(named) => named.get_attributes(),
            NamedOrUnnamedFieldMut::Unnamed(unnamed) => unnamed.get_attributes(),
        }
    }
}

impl Field for NamedOrUnnamedFieldMut<'_> {
    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        match self {
            NamedOrUnnamedFieldMut::Named(named) => named.get_type_that_needs_constraint(),
            NamedOrUnnamedFieldMut::Unnamed(unnamed) => unnamed.get_type_that_needs_constraint(),
        }
    }

    fn get_pattern_with_config(
        &self,
        type_of_self: TypeOfSelf,
        name_postfix: &'static str,
    ) -> TokenStream {
        match self {
            NamedOrUnnamedFieldMut::Named(named) => {
                named.get_pattern_with_config(type_of_self, name_postfix)
            }
            NamedOrUnnamedFieldMut::Unnamed(unnamed) => {
                unnamed.get_pattern_with_config(type_of_self, name_postfix)
            }
        }
    }

    fn get_type(&self) -> &Type {
        match self {
            NamedOrUnnamedFieldMut::Named(named) => named.get_type(),
            NamedOrUnnamedFieldMut::Unnamed(unnamed) => unnamed.get_type(),
        }
    }
}

impl FieldMut for NamedOrUnnamedFieldMut<'_> {
    fn get_reference_with_config(
        &mut self,
        used_for_trait: bool,
        name_postfix: &'static str,
    ) -> Expr {
        match self {
            NamedOrUnnamedFieldMut::Named(named) => {
                named.get_reference_with_config(used_for_trait, name_postfix)
            }
            NamedOrUnnamedFieldMut::Unnamed(unnamed) => {
                unnamed.get_reference_with_config(used_for_trait, name_postfix)
            }
        }
    }
}

pub struct NamedField {
    pub name: Ident,
    pub attrs: Vec<Attribute>,
    pub ty: Type,
    idx: usize,
    is_used: bool,
    used_for_trait: bool,
}

impl HasAttributes for NamedField {
    fn get_attributes(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl Field for NamedField {
    fn get_pattern_with_config(
        &self,
        type_of_self: TypeOfSelf,
        name_postfix: &'static str,
    ) -> TokenStream {
        let Self {
            name, is_used, idx, ..
        } = self;
        if *is_used {
            let annotations = type_of_self.as_matcher_tokens();
            let reference_name = format_ident!("_{}{}", idx, name_postfix);
            quote!(#name: #annotations #reference_name)
        } else {
            quote!(#name: _)
        }
    }

    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        self.used_for_trait.then(|| self.ty.clone())
    }

    fn get_type(&self) -> &Type {
        &self.ty
    }
}

impl FieldMut for NamedField {
    fn get_reference_with_config(
        &mut self,
        used_for_trait: bool,
        name_postfix: &'static str,
    ) -> Expr {
        self.is_used = true;
        self.used_for_trait |= used_for_trait;
        let path: Expr = syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: format_ident!("_{}{}", self.idx, name_postfix).into(),
        }
        .into();
        if matches!(self.ty, Type::Reference(..)) {
            Expr::Unary(ExprUnary {
                attrs: Vec::default(),
                op: syn::UnOp::Deref(Token![*](Span::call_site())),
                expr: Box::new(path),
            })
        } else {
            path
        }
    }
}

pub struct UnnamedField {
    pub idx: usize,
    pub attrs: Vec<Attribute>,
    pub ty: Type,
    is_used: bool,
    used_for_trait: bool,
}

impl HasAttributes for UnnamedField {
    fn get_attributes(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl Field for UnnamedField {
    fn get_pattern_with_config(
        &self,
        type_of_self: TypeOfSelf,
        name_postfix: &'static str,
    ) -> TokenStream {
        let Self { is_used, idx, .. } = self;
        if *is_used {
            let mut ts = type_of_self.as_matcher_tokens();
            ts.extend(format_ident!("_{}{}", idx, name_postfix).to_token_stream());
            ts
        } else {
            quote!(_)
        }
    }

    fn get_type_that_needs_constraint(&self) -> Option<Type> {
        self.used_for_trait.then(|| self.ty.clone())
    }

    fn get_type(&self) -> &Type {
        &self.ty
    }
}

impl FieldMut for UnnamedField {
    fn get_reference_with_config(
        &mut self,
        used_for_trait: bool,
        name_postfix: &'static str,
    ) -> Expr {
        self.is_used = true;
        self.used_for_trait |= used_for_trait;
        let path: Expr = syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: format_ident!("_{}{}", self.idx, name_postfix).into(),
        }
        .into();
        if matches!(self.ty, Type::Reference(..)) {
            Expr::Unary(ExprUnary {
                attrs: Vec::default(),
                op: syn::UnOp::Deref(Token![*](Span::call_site())),
                expr: Box::new(path),
            })
        } else {
            path
        }
    }
}
