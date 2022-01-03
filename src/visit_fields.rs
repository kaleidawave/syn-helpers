use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use std::error;
use syn::{parse_quote, Arm, Data, DeriveInput, GenericParam, LitStr, Stmt};

use crate::{
    fields_to_structure_fields, generic_param_to_generic_argument_token_stream,
    generic_parameters_have_same_name, BuildPair, Field, Fields, NamedField, PrefixAndPostfix,
    Structure, Trait, TraitMethod, UnnamedField,
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

    // Cloning bc going to modify them in this scope for clashing reasons
    let mut structure_generics = structure.generics.clone();

    let Trait {
        name: trait_name,
        generic_parameters: trait_generic_parameters,
        methods,
    } = implementing_trait;

    let mut conflict_resolution_corrections_index = 0;

    let generics_on_structure = if !structure_generics.params.is_empty() {
        // This modifies `structure_generics`s generics in-place, not great but as owned under this function it should be okay
        let mut structure_identifiers = Vec::<TokenStream>::new();
        for structure_generic_parameter in structure_generics.params.iter_mut() {
            if trait_generic_parameters
                .iter()
                .any(|trait_generic_parameter| {
                    generic_parameters_have_same_name(
                        trait_generic_parameter,
                        structure_generic_parameter,
                    )
                })
            {
                let ident = Ident::new(
                    &format!("Gp{}", conflict_resolution_corrections_index),
                    Span::call_site(),
                );
                conflict_resolution_corrections_index += 1;
                match structure_generic_parameter {
                    GenericParam::Type(gtp) => gtp.ident = ident.clone(),
                    GenericParam::Lifetime(glp) => glp.lifetime.ident = ident.clone(),
                    GenericParam::Const(gcp) => gcp.ident = ident.clone(),
                }
            }
            structure_identifiers.push(generic_param_to_generic_argument_token_stream(
                structure_generic_parameter,
            ));
        }
        Some(quote!(<#(#structure_identifiers),*>))
    } else {
        None
    };

    let generics_on_trait = if !trait_generic_parameters.is_empty() {
        // Removes bounds
        let trait_generic_arguments =
            trait_generic_parameters
                .iter()
                .map(|trait_generic_parameter| {
                    generic_param_to_generic_argument_token_stream(trait_generic_parameter)
                });
        Some(quote!(<#(#trait_generic_arguments),*>))
    } else {
        None
    };

    // Combination of structure and trait generics, retains bounds
    let generics_on_impl =
        if !trait_generic_parameters.is_empty() || !structure_generics.params.is_empty() {
            let mut references = trait_generic_parameters
                .iter()
                .chain(structure_generics.params.iter())
                .collect::<Vec<_>>();
            references.sort_unstable_by_key(|generic| match generic {
                GenericParam::Lifetime(_) => 0,
                GenericParam::Type(_) => 1,
                GenericParam::Const(_) => 2,
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
                            let error_as_string = LitStr::new(&err.to_string(), Span::call_site());
                            return quote!( compile_error!(#error_as_string); )
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
                            return quote!( compile_error!(#error_as_string); )
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
                impl #generics_on_impl #trait_name #generics_on_trait for #structure_name #generics_on_structure {
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
                            return quote!( compile_error!(#error_as_string); );
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
                                return quote!( compile_error!(#error_as_string); );
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
                impl #generics_on_impl #trait_name #generics_on_trait for #structure_name #generics_on_structure {
                    #methods
                }
            }
        }
        Data::Union(_) => {
            quote!( compile_error!("syn-helpers does not support derives on unions"); )
        }
    }
}
