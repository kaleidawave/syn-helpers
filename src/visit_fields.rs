use std::error;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse_quote, Arm, Data, DeriveInput, GenericParam, LitStr, Stmt};

use crate::{
    fields_to_structure_fields, BuildPair, Field, Fields, NamedField, PrefixAndPostfix, Structure,
    Trait, TraitMethod, UnnamedField,
};

/// Generates implementation for a trait over a structure
pub fn build_implementation_over_structure<'a>(
    structure: &'a DeriveInput,
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
    let structure_name = &structure.ident;
    let structure_generics = &structure.generics;

    let Trait {
        name: trait_name,
        generic_parameters: trait_generic_parameters,
        methods,
    } = implementing_trait;

    let trait_generics = if !trait_generic_parameters.is_empty() {
        Some(quote!(<#(#trait_generic_parameters),*>))
    } else {
        None
    };

    let implementation_generics =
        if !trait_generic_parameters.is_empty() || !structure_generics.params.is_empty() {
            let parameters_chained = trait_generic_parameters
                .iter()
                .chain(structure_generics.params.iter());
            Some(quote!(<#(#parameters_chained),*>))
        } else {
            None
        };

    let structure_generics = if !structure_generics.params.is_empty() {
        // Remove constraints for the arguments of the type on the rhs of the impl
        let references = structure_generics
            .params
            .iter()
            .map(|parameter| match parameter {
                GenericParam::Type(ty) => {
                    let ident = &ty.ident;
                    quote!(#ident)
                }
                GenericParam::Lifetime(lt) => {
                    let lt = &lt.lifetime;
                    quote!(#lt)
                }
                GenericParam::Const(cg) => {
                    let ident = &cg.ident;
                    quote!(#ident)
                }
            });
        Some(quote!(<#(#references),*>))
    } else {
        None
    };

    match &structure.data {
        Data::Struct(r#struct) => {
            let methods = methods
                .into_iter()
                .flat_map(|method| {
                    let TraitMethod {
                        method_name,
                        method_parameters,
                        method_generics,
                        return_type,
                        build_pair,
                    } = method;

                    let structure_result = structure_handler(method_name.to_string(), structure);

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

                    let mut fields = fields_to_structure_fields(
                        &r#struct.fields,
                        Structure::Struct {
                            struct_name: structure_name,
                            struct_attrs: &structure.attrs,
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
                            if build_pair.is_pair() {
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
                            if build_pair.is_pair() {
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

                    // If there are generic parameter, wrap in them in '<' '>'
                    let chevroned_generic_params = if !method_generics.is_empty() {
                        Some(quote!(<#(#method_generics),*>))
                    } else {
                        None
                    };

                    quote! {
                        fn #method_name#chevroned_generic_params(#(#method_parameters),*) #return_type {
                            #(#prefix)*
                            #(#locals)*
                            #(#body)*
                            #(#postfix)*
                        }
                    }
                })
                .collect::<TokenStream>();
            quote! {
                impl #implementation_generics #trait_name #trait_generics for #structure_name #structure_generics {
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
                        method_generics,
                        return_type,
                        build_pair,
                    } = method;

                    let structure_result = structure_handler(method_name.to_string(), structure);

                    let PrefixAndPostfix { prefix, postfix } = match structure_result {
                        Ok(structure) => structure,
                        Err(err) => {
                            let error_as_string = LitStr::new(&err.to_string(), Span::call_site());
                            return quote!(compile_error!(#error_as_string));
                        }
                    };

                    let branches = r#enum.variants.iter().map(|variant| {
                        let variant_ident = &variant.ident;
                        let mut fields = fields_to_structure_fields(
                            &variant.fields,
                            Structure::EnumVariant {
                                enum_name: structure_name,
                                enum_attrs: &structure.attrs,
                                variant_name: variant_ident,
                                variant_attrs: &variant.attrs,
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
                                if build_pair.is_pair() {
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
                                if build_pair.is_pair() {
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
                            Fields::Unit { .. } => {
                                if build_pair.is_pair() {
                                    quote! {
                                        (Self::#variant_ident, Self::#variant_ident) => {
                                            #(#body)*
                                        }
                                    }
                                } else {
                                    quote! {
                                        Self::#variant_ident => {
                                            #(#body)*
                                        }
                                    }
                                }
                            },
                        }
                    });

                    let (exhaustive_arm, matching_on): (Option<Arm>, TokenStream) = if let BuildPair::Pair {
                        ref statements_if_enums_do_not_match,
                        ref other_item_name
                    } = build_pair
                    {
                        (Some(parse_quote! { _ => {
                            #(#statements_if_enums_do_not_match)*
                        }}), quote! { (self, #other_item_name) })
                    } else {
                        (None, quote!(self))
                    };

                    let return_type_tokens = return_type.map(|ty| quote!( -> #ty ));

                    // If there are generic parameter, wrap in them in '<' '>'
                    let chevroned_generic_params = if !method_generics.is_empty() {
                        Some(quote!(<#(#method_generics),*>))
                    } else {
                        None
                    };

                    quote! {
                        fn #method_name#chevroned_generic_params(#(#method_parameters),*) #return_type_tokens {
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
                impl #implementation_generics #trait_name #trait_generics for #structure_name #structure_generics {
                    #methods
                }
            }
        }
        Data::Union(_) => todo!(),
    }
}
