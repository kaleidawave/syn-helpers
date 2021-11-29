use std::{error, iter};

use either_n::{Either2, Either3};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse_quote, Arm, Attribute, Data, DeriveInput, Expr, Fields as SynFields, FnArg, GenericParam,
    LitStr, Path, Stmt, Type,
};

pub enum BuildPair {
    Pair {
        statements_if_enums_do_not_match: Vec<Stmt>,
    },
    NoPairing,
}

impl BuildPair {
    pub(super) fn pair(&self) -> bool {
        matches!(self, Self::Pair { .. })
    }
}

pub struct Trait {
    pub name: Path,
    pub generic_parameters: Vec<GenericParam>,
    pub methods: Vec<TraitMethod>,
}

pub struct TraitMethod {
    pub method_name: Ident,
    pub method_parameters: Vec<FnArg>,
    pub return_type: Option<Type>,
    pub build_pair: BuildPair,
}

pub struct PrefixAndPostfix<T> {
    pub prefix: Vec<T>,
    pub postfix: Vec<T>,
}

impl<T> Default for PrefixAndPostfix<T> {
    fn default() -> Self {
        Self {
            prefix: Default::default(),
            postfix: Default::default(),
        }
    }
}

pub fn visit_all_variants_of_data<'a>(
    data: &'a DeriveInput,
    implementing_trait: Trait,
    structure_handler: impl Fn(
        String,
        &'a DeriveInput,
    ) -> Result<PrefixAndPostfix<Stmt>, Box<dyn error::Error>>,
    field_handler: impl for<'b> Fn(
        String,
        &'b mut Fields<'a>,
    ) -> Result<Vec<Stmt>, Box<dyn error::Error>>,
) -> TokenStream {
    let data_name = &data.ident;

    let Trait {
        name: trait_name,
        generic_parameters: trait_generic_parameters,
        methods,
    } = implementing_trait;

    let generics = if !trait_generic_parameters.is_empty() {
        quote!(<#(#trait_generic_parameters),*>)
    } else {
        quote!()
    };

    match &data.data {
        Data::Struct(r#struct) => {
            let methods = methods
                .into_iter()
                .flat_map(|method| {
                    let TraitMethod {
                        method_name,
                        method_parameters,
                        return_type,
                        build_pair,
                    } = method;

                    let structure_result = structure_handler(method_name.to_string(), data);

                    let PrefixAndPostfix { prefix, postfix } = match structure_result {
                        Ok(structure) => structure,
                        Err(err) => {
                            let error_as_string = LitStr::new(
                                &err.to_string(),
                                Span::call_site(),
                            );
                            return quote!( compile_error!(#error_as_string) )
                        },
                    };

                    let mut fields = fields_to_data_fields(
                        &r#struct.fields,
                        Structure::Struct {
                            struct_name: data_name,
                            struct_attributes: &data.attrs,
                        },
                    );

                    let handler_result = field_handler(method_name.to_string(), &mut fields);
                    let body = match handler_result {
                        Ok(body) => body,
                        Err(err) => {
                            let error_as_string = LitStr::new(
                                &err.to_string(),
                                Span::call_site(),
                            );
                            return quote!( compile_error!(#error_as_string) )
                        },
                    };

                    let locals: Vec<Stmt> = match &fields {
                        Fields::Named { fields, .. } => {
                            let fields_pattern = fields.iter().map(NamedField::get_pattern);

                            let first = parse_quote! { let Self { #(#fields_pattern),* } = self; };
                            if build_pair.pair() {
                                let other_fields_pattern = fields.iter().map(NamedField::get_other_pattern);
                                let second =
                                    parse_quote! { let Self { #(#other_fields_pattern),* } = other; };

                                vec![first, second]
                            } else {
                                vec![first]
                            }
                        }
                        Fields::Unnamed { fields, .. } => {
                            let fields_pattern = fields.iter().map(UnnamedField::get_pattern);
                            let first = parse_quote! { let Self ( #(#fields_pattern),* ) = self; };
                            if build_pair.pair() {
                                let other_fields_pattern = fields.iter().map(UnnamedField::get_other_pattern);
                                let second =
                                    parse_quote! { let Self ( #(#other_fields_pattern),* ) = other; };
                                vec![first, second]
                            } else {
                                vec![first]
                            }
                        }
                        Fields::Unit { .. } => vec![],
                    };

                    let return_type = return_type.map(|ty| quote!( -> #ty ));

                    quote! {
                        fn #method_name(#(#method_parameters),*) #return_type {
                            #(#prefix)*
                            #(#locals)*
                            #(#body)*
                            #(#postfix)*
                        }
                    }
                })
                .collect::<TokenStream>();
            quote! {
                impl#generics #trait_name#generics for #data_name {
                    #methods
                }
            }
        }
        Data::Enum(r#enum) => {
            let methods = methods
                .into_iter()
                .flat_map(|method| {
                    let TraitMethod {
                        method_name,
                        method_parameters,
                        return_type,
                        build_pair,
                    } = method;

                    let structure_result = structure_handler(method_name.to_string(), data);

                    let PrefixAndPostfix { prefix, postfix } = match structure_result {
                        Ok(structure) => structure,
                        Err(err) => {
                            let error_as_string = LitStr::new(&err.to_string(), Span::call_site());
                            return quote!(compile_error!(#error_as_string));
                        }
                    };

                    let branches = r#enum.variants.iter().map(|variant| {
                        let variant_ident = &variant.ident;
                        let mut fields = fields_to_data_fields(
                            &variant.fields,
                            Structure::EnumVariant {
                                enum_name: data_name,
                                enum_attributes: &data.attrs,
                                variant_name: variant_ident,
                                variant_attributes: &variant.attrs,
                            },
                        );

                        let handler_result = field_handler(method_name.to_string(), &mut fields);

                        let body = match handler_result {
                            Ok(body) => body,
                            Err(err) => {
                                let error_as_string =
                                    LitStr::new(&err.to_string(), Span::call_site());
                                return quote!(compile_error!(#error_as_string));
                            }
                        };

                        match fields {
                            Fields::Named { fields, .. } => {
                                let fields_pattern = fields.iter().map(NamedField::get_pattern);
                                if build_pair.pair() {
                                    let other_fields_pattern =
                                        fields.iter().map(NamedField::get_other_pattern);
                                    quote! {
                                        (
                                            Self::#variant_ident { #(#fields_pattern),* },
                                            Self::#variant_ident { #(#other_fields_pattern),* }
                                        ) => {
                                            #(#body)*
                                        }
                                    }
                                } else {
                                    quote! {
                                        Self::#variant_ident { #(#fields_pattern),* } => {
                                            #(#body)*
                                        }
                                    }
                                }
                            }
                            Fields::Unnamed { fields, .. } => {
                                let fields_pattern = fields.iter().map(UnnamedField::get_pattern);
                                if build_pair.pair() {
                                    let other_fields_pattern =
                                        fields.iter().map(UnnamedField::get_other_pattern);
                                    quote! {
                                        (
                                            Self::#variant_ident ( #(#fields_pattern),* ),
                                            Self::#variant_ident ( #(#other_fields_pattern),* )
                                        ) => {
                                            #(#body)*
                                        }
                                    }
                                } else {
                                    quote! {
                                        Self::#variant_ident( #(#fields_pattern),* ) => {
                                            #(#body)*
                                        }
                                    }
                                }
                            }
                            Fields::Unit { .. } => quote! {
                                Self::#variant_ident => {
                                    #(#body)*
                                }
                            },
                        }
                    });

                    let matching_on = if build_pair.pair() {
                        quote! { (self, other) }
                    } else {
                        quote!(self)
                    };

                    let exhaustive_arm: Option<Arm> = if let BuildPair::Pair {
                        ref statements_if_enums_do_not_match,
                    } = build_pair
                    {
                        Some(parse_quote! { _ => {
                            #(#statements_if_enums_do_not_match)*
                        }})
                    } else {
                        None
                    };

                    let return_type = return_type.map(|ty| quote!( -> #ty ));

                    quote! {
                        fn #method_name(#(#method_parameters),*) #return_type {
                            #(#prefix)*
                            match #matching_on {
                                #(#branches),*,
                                #exhaustive_arm
                            }
                            #(#postfix)*
                        }
                    }
                })
                .collect::<TokenStream>();
            quote! {
                impl #generics #trait_name #generics for #data_name {
                    #methods
                }
            }
        }
        Data::Union(_) => todo!(),
    }
}

pub enum Fields<'a> {
    Named {
        on_structure: Structure<'a>,
        fields: Vec<NamedField<'a>>,
    },
    Unnamed {
        on_structure: Structure<'a>,
        fields: Vec<UnnamedField<'a>>,
    },
    Unit {
        on_structure: Structure<'a>,
    },
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

    pub fn fields_iterator<'b>(&'b mut self) -> impl Iterator<Item = NamedOrUnnamedField<'b, 'a>> {
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

fn fields_to_data_fields<'a>(fields: &'a SynFields, structure: Structure<'a>) -> Fields<'a> {
    match fields {
        SynFields::Named(named_fields) => {
            let named_fields = named_fields
                .named
                .iter()
                .map(|field| NamedField {
                    attrs: &field.attrs,
                    name: field.ident.as_ref().unwrap(),
                    ty: &field.ty,
                    is_used: false,
                })
                .collect::<Vec<_>>();
            Fields::Named {
                fields: named_fields,
                on_structure: structure,
            }
        }
        SynFields::Unnamed(unnamed_fields) => {
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
        SynFields::Unit => Fields::Unit {
            on_structure: structure,
        },
    }
}

pub trait Field {
    fn get_reference(&mut self) -> Expr;

    /// For pair building
    fn get_other_reference(&mut self) -> Expr;

    fn get_pattern(&self) -> TokenStream;

    /// For pair building
    fn get_other_pattern(&self) -> TokenStream;

    fn get_attrs(&self) -> &Vec<Attribute>;
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

    fn get_attrs(&self) -> &Vec<Attribute> {
        match self {
            NamedOrUnnamedField::Named(named) => named.get_attrs(),
            NamedOrUnnamedField::Unnamed(unnamed) => unnamed.get_attrs(),
        }
    }
}

pub struct NamedField<'a> {
    pub name: &'a Ident,
    pub attrs: &'a Vec<Attribute>,
    pub ty: &'a Type,
    is_used: bool,
}

impl Field for NamedField<'_> {
    fn get_reference(&mut self) -> Expr {
        let ident = self.name;
        self.is_used = true;
        parse_quote!(#ident)
    }

    fn get_other_reference(&mut self) -> Expr {
        self.is_used = true;
        let ident = format_ident!("other_{}", self.name);
        parse_quote!(#ident)
    }

    fn get_pattern(&self) -> TokenStream {
        let ident = self.name;
        if self.is_used {
            quote!(#ident)
        } else {
            quote!(#ident: _)
        }
    }

    fn get_other_pattern(&self) -> TokenStream {
        let ident = self.name;
        if self.is_used {
            let other_ident = format_ident!("other_{}", self.name);
            quote!(#ident: #other_ident)
        } else {
            quote!(#ident: _ )
        }
    }

    fn get_attrs(&self) -> &Vec<Attribute> {
        &self.attrs
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
        let idx = format_ident!("_other{}", self.idx);
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
            let idx = format_ident!("_other{}", self.idx);
            quote!(#idx)
        } else {
            quote!(_)
        }
    }

    fn get_attrs(&self) -> &Vec<Attribute> {
        &self.attrs
    }
}

/// The data type the field is on
pub enum Structure<'a> {
    Struct {
        struct_name: &'a Ident,
        struct_attributes: &'a Vec<Attribute>,
    },
    EnumVariant {
        enum_name: &'a Ident,
        enum_attributes: &'a Vec<Attribute>,
        variant_name: &'a Ident,
        variant_attributes: &'a Vec<Attribute>,
    },
}

impl<'a> Structure<'a> {
    pub fn all_attributes(&'a self) -> impl Iterator<Item = &'a Attribute> {
        match self {
            Structure::Struct {
                struct_attributes, ..
            } => Either2::One(struct_attributes.iter()),
            Structure::EnumVariant {
                enum_attributes,
                variant_attributes,
                ..
            } => Either2::Two(enum_attributes.iter().chain(variant_attributes.iter())),
        }
    }

    pub fn field_attributes(&'a self) -> impl Iterator<Item = &'a Attribute> {
        match self {
            Structure::Struct {
                struct_attributes: attributes,
                ..
            }
            | Structure::EnumVariant {
                variant_attributes: attributes,
                ..
            } => attributes.iter(),
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
