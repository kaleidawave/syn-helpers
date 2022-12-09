use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::HashMap, error};
use syn::{parse_quote, visit_mut::VisitMut, Arm, Data, DeriveInput, GenericParam, LitStr, Stmt};

use crate::{
    fields_to_structure_fields, generic_param_to_generic_argument_token_stream,
    generic_parameters_have_same_name, BuildPair, Field, Fields, PrefixAndPostfix, Structure,
    Trait, TraitMethod,
};

pub type StructureHandlerResult = Result<PrefixAndPostfix<Stmt>, Box<dyn error::Error>>;
pub type FieldHandlerResult = Result<Vec<Stmt>, Box<dyn error::Error>>;

/// Generates implementation for a trait over a structure
/// - Generated impl blocks
/// - Handles patterns for read and unread fields
pub fn build_implementation_over_structure<'a>(
    structure: &'a DeriveInput,
    implementing_trait: Trait,
    structure_handler: impl Fn(String, &'a DeriveInput) -> StructureHandlerResult,
    field_handler: impl for<'b> Fn(String, &'b mut Fields<'a>) -> FieldHandlerResult,
) -> TokenStream {
    let structure_name = &structure.ident;

    // Cloning as modify clashes
    let mut structure_generics = structure.generics.clone();

    let Trait {
        name: trait_name,
        generic_parameters: trait_generic_parameters,
        methods: trait_methods,
    } = implementing_trait;

    let mut conflicts_map = HashMap::new();

    let trait_with_arguments: TokenStream = if !trait_generic_parameters.is_empty() {
        // Rename clashing trait names
        if structure_generics.lt_token.is_some() {
            for structure_generic_parameter in structure_generics.params.iter_mut() {
                let collision = trait_generic_parameters
                    .iter()
                    .any(|trait_generic_parameter| {
                        generic_parameters_have_same_name(
                            trait_generic_parameter,
                            structure_generic_parameter,
                        )
                    });

                if collision {
                    // Just hope nothing called `_gp...`...
                    let new_ident =
                        Ident::new(&format!("_gp{}", conflicts_map.len()), Span::call_site());

                    let ident: &mut Ident = match structure_generic_parameter {
                        GenericParam::Type(gtp) => &mut gtp.ident,
                        GenericParam::Lifetime(glp) => &mut glp.lifetime.ident,
                        GenericParam::Const(gcp) => &mut gcp.ident,
                    };
                    conflicts_map.insert(ident.clone(), new_ident.clone());
                    *ident = new_ident;
                }
            }
        }

        // Removes bounds off parameters thus becoming arguments
        let trait_generic_arguments =
            trait_generic_parameters
                .iter()
                .map(|trait_generic_parameter| {
                    generic_param_to_generic_argument_token_stream(trait_generic_parameter)
                });

        quote!(#trait_name<#(#trait_generic_arguments),*>)
    } else {
        trait_name.to_token_stream()
    };

    // Combination of structure and trait generics, retains bounds
    let generics_for_impl =
        if !trait_generic_parameters.is_empty() || !structure_generics.params.is_empty() {
            let mut references = trait_generic_parameters
                .iter()
                .chain(structure_generics.params.iter())
                .collect::<Vec<_>>();

            // This is the order in which the AST is defined
            references.sort_unstable_by_key(|generic| match generic {
                GenericParam::Lifetime(_) => 0,
                GenericParam::Type(_) => 1,
                GenericParam::Const(_) => 2,
            });

            Some(quote!(<#(#references),*>))
        } else {
            None
        };

    // Could be `HashSet` but Rust tolerates duplicate where clause (when that rarely occurs)
    // Vec ensures consistent order which makes tests easy
    let mut where_clauses = Vec::new();

    match &structure.data {
        Data::Struct(r#struct) => {
            let methods = trait_methods
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

                    let body = match field_handler(method_name.to_string(), &mut fields) {
                        Ok(body) => body,
                        Err(err) => {
                            let error_as_string = LitStr::new(
                                &err.to_string(),
                                Span::call_site(),
                            );
                            return quote!( compile_error!(#error_as_string); )
                        },
                    };

                    where_clauses.extend(fields.fields_iterator().flat_map(|field| field.get_type_that_needs_constraint()));

                    let self_path = parse_quote!(Self);
                    let matcher = fields.to_pattern(&self_path, crate::Variant::Primary);
                    let locals = if build_pair.is_pair() {
                        let alternative_matcher = fields.to_pattern(&self_path, crate::Variant::Secondary);

                        quote! {
                            let #matcher = self;
                            let #alternative_matcher = other
                        }
                    } else {
                        quote!(let #matcher = self)
                    };

                    // If there are generic parameter, wrap in them in '<' '>'
                    let chevroned_generic_params = if !method_generics.is_empty() {
                        Some(quote!(<#(#method_generics),*>))
                    } else {
                        None
                    };

                    let return_type = return_type.iter();

                    quote! {
                        fn #method_name#chevroned_generic_params(#(#method_parameters),*) #(-> #return_type)* {
                            #(#prefix)*
                            #locals;
                            #(#body)*
                            #(#postfix)*
                        }
                    }
                })
                .collect::<TokenStream>();

            let where_clause: Option<_> = if !where_clauses.is_empty() {
                if !conflicts_map.is_empty() {
                    for element in where_clauses.iter_mut() {
                        RenameGenerics(&conflicts_map).visit_type_mut(element)
                    }
                }
                Some(quote!(where #( #where_clauses: #trait_with_arguments ),* ))
            } else {
                None
            };

            quote! {
                impl #generics_for_impl #trait_with_arguments for #structure_name #structure_generics #where_clause {
                    #methods
                }
            }
        }
        Data::Enum(r#enum) => {
            let methods = trait_methods
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

                        where_clauses.extend(fields.fields_iterator().flat_map(|field| field.get_type_that_needs_constraint()));

                        let variant_pattern = parse_quote!(Self::#variant_ident);
                        let matcher = fields.to_pattern(&variant_pattern, crate::Variant::Primary);

                        if build_pair.is_pair() {
                            let alternative_matcher = fields.to_pattern(&variant_pattern, crate::Variant::Secondary);

                            quote! ( (#matcher, #alternative_matcher) => { #(#body)* } )                           
                        } else {
                            quote! { #matcher => { #(#body)* } }
                        }
                    });

                    let (exhaustive_arm, matching_on): (Option<Arm>, TokenStream) = if let BuildPair::Pair {
                        ref statements_if_enums_do_not_match,
                        ref other_item_name
                    } = build_pair
                    {
                        let arm = Some(parse_quote! { _ => {
                            #(#statements_if_enums_do_not_match)*
                        }});
                        (arm, quote! { (self, #other_item_name) })
                    } else {
                        (None, quote!(self))
                    };

                    // If there are generic parameter, wrap in them in `<..>`
                    let chevroned_generic_params = if !method_generics.is_empty() {
                        Some(quote!(<#(#method_generics),*>))
                    } else {
                        None
                    };

                    let return_type = return_type.iter();

                    quote! {
                        fn #method_name#chevroned_generic_params(#(#method_parameters),*) #(-> #return_type)* {
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

            let where_clause: Option<_> = if !where_clauses.is_empty() {
                if !conflicts_map.is_empty() {
                    for element in where_clauses.iter_mut() {
                        RenameGenerics(&conflicts_map).visit_type_mut(element)
                    }
                }
                Some(quote!(where #( #where_clauses : #trait_with_arguments ),*))
            } else {
                None
            };

            quote! {
                impl#generics_for_impl #trait_with_arguments for #structure_name #structure_generics #where_clause {
                    #methods
                }
            }
        }
        Data::Union(_) => {
            quote!( compile_error!("syn-helpers does not support derives on unions"); )
        }
    }
}

struct RenameGenerics<'a>(pub &'a HashMap<Ident, Ident>);

impl<'a> VisitMut for RenameGenerics<'a> {
    fn visit_type_reference_mut(&mut self, i: &mut syn::TypeReference) {
        if let Some(ref mut lifetime) = i.lifetime {
            if let Some(rewrite) = self.0.get(&lifetime.ident).cloned() {
                lifetime.ident = rewrite;
            }
        }
        self.visit_type_mut(&mut i.elem);
    }

    fn visit_type_path_mut(&mut self, i: &mut syn::TypePath) {
        if let Some(current_ident) = i.path.get_ident() {
            if let Some(rewrite) = self.0.get(current_ident).cloned() {
                i.path = rewrite.into();
            }
        }
    }
}
