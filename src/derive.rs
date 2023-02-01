use std::{collections::HashMap, iter::FromIterator};

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    punctuated::Punctuated, visit_mut::VisitMut, Data, GenericParam, Ident, Path, PathSegment,
};

use crate::{
    dyn_error_to_compile_error_tokens, generic_helpers,
    generic_param_to_generic_argument_token_stream, generic_parameters_have_same_name, model::Item,
    syn_fields_to_fields, EnumStructure, EnumVariant, Field, StructStructure, Structure, Trait,
    TraitItem,
};

/// Creates an impl block for the respective trait over the item
///
/// Handles a few complex things
/// - Collisions with the names of generics on the trait and the structure
/// - Creating where clauses for referenced items
pub fn derive_trait(item: syn::DeriveInput, r#trait: Trait) -> TokenStream {
    let syn::DeriveInput {
        generics: mut structure_generics,
        attrs,
        data,
        ident: structure_name,
        ..
    } = item;

    let Trait {
        name: trait_name,
        generic_parameters: trait_generic_parameters,
        items,
    } = r#trait;

    let mut generic_conflicts_map = HashMap::new();

    let trait_with_arguments: TokenStream = if let Some(trait_generic_parameters) =
        trait_generic_parameters.as_ref()
    {
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
                    let new_ident = Ident::new(
                        &format!("_gp{}", generic_conflicts_map.len()),
                        Span::call_site(),
                    );

                    let ident: &mut Ident = match structure_generic_parameter {
                        GenericParam::Type(gtp) => &mut gtp.ident,
                        GenericParam::Lifetime(glp) => &mut glp.lifetime.ident,
                        GenericParam::Const(gcp) => &mut gcp.ident,
                    };

                    generic_conflicts_map.insert(ident.clone(), new_ident.clone());
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
        quote::ToTokens::to_token_stream(&trait_name)
    };

    // Combination of structure and trait generics, retains bounds
    let generics_for_impl =
        if trait_generic_parameters.is_some() || !structure_generics.params.is_empty() {
            let mut references = trait_generic_parameters
                .iter()
                .flatten()
                .chain(structure_generics.params.iter())
                .collect::<Vec<_>>();

            // This is the order in which the AST is defined
            references.sort_unstable_by_key(|generic| match generic {
                GenericParam::Lifetime(_) => 0,
                GenericParam::Type(_) => 1,
                GenericParam::Const(_) => 2,
            });

            let token_stream = quote!(<#(#references),*>);

            // Clear bounds for when reprinted
            structure_generics
                .type_params_mut()
                .for_each(|tp| tp.bounds = Default::default());

            Some(token_stream)
        } else {
            None
        };

    // Could be `HashSet` but Rust tolerates duplicate where clause (when that rarely occurs)
    // Vec ensures consistent order which makes tests easy
    let mut where_clauses = Vec::new();

    let mut structure = match data {
        Data::Struct(r#struct) => {
            let fields = syn_fields_to_fields(r#struct.fields, attrs);
            Structure::Struct(StructStructure {
                fields,
                name: structure_name.clone(),
            })
        }
        Data::Enum(r#enum) => Structure::Enum(EnumStructure {
            name: structure_name.clone(),
            attrs,
            variants: r#enum
                .variants
                .into_iter()
                .enumerate()
                .map(|(idx, variant)| EnumVariant {
                    full_path: Path {
                        leading_colon: None,
                        segments: Punctuated::from_iter([
                            PathSegment::from(structure_name.clone()),
                            PathSegment::from(variant.ident),
                        ]),
                    },
                    idx,
                    fields: syn_fields_to_fields(variant.fields, variant.attrs),
                })
                .collect(),
        }),
        Data::Union(_) => {
            return quote!( compile_error!("syn-helpers does not support derives on unions"); );
        }
    };

    let impl_items = items
        .into_iter()
        .flat_map(|item| {
            match item {
                TraitItem::Method {
                    name,
                    generic_parameters,
                    self_type,
                    other_parameters,
                    return_type,
                    handler,
                } => {
                    let result = handler(Item { structure: &mut structure, self_type });
                    let stmts = match result {
                        Ok(stmts) => stmts,
                        Err(err) => {
                            return dyn_error_to_compile_error_tokens(err);
                        },
                    };
                    let chevroned_generic_params = if let Some(generic_parameters) = generic_parameters {
                        quote! { <#(#generic_parameters),*> }
                    } else {
                        TokenStream::default()
                    };
                    let return_type = return_type.iter();
                    let self_parameter = self_type.as_parameter_tokens();
                    quote! {
                        fn #name #chevroned_generic_params(#self_parameter #(,#other_parameters)*) #(-> #return_type)* {
                            #(#stmts)*
                        }
                    }
                }
                TraitItem::AssociatedFunction {
                    name,
                    generic_parameters,
                    parameters,
                    return_type,
                    handler,
                } => {
                    let result = handler(&mut structure);
                    let stmts = match result {
                        Ok(stmts) => stmts,
                        Err(err) => {
                            return dyn_error_to_compile_error_tokens(err);
                        },
                    };
                    let chevroned_generic_params = if let Some(generic_parameters) = generic_parameters {
                        quote! { <#(#generic_parameters),*> }
                    } else {
                        TokenStream::default()
                    };
                    let return_type = return_type.iter();
                    quote! {
                        fn #name #chevroned_generic_params(#(#parameters),*) #(-> #return_type)* {
                            #(#stmts)*
                        }
                    }
                }
            }
        }).collect::<TokenStream>();

    {
        let iter = structure
            .all_fields()
            .flat_map(|field| field.get_type_that_needs_constraint())
            .map(|mut ty| {
                generic_helpers::RenameGenerics(&generic_conflicts_map).visit_type_mut(&mut ty);
                ty
            })
            .filter(|ty| generic_helpers::ReferencesAGeneric::has_generic(ty, &structure_generics));

        where_clauses.extend(iter);
    }

    let where_clause: Option<_> = if !where_clauses.is_empty() {
        Some(quote!(where #( #where_clauses: #trait_with_arguments ),* ))
    } else {
        None
    };

    quote! {
        #[automatically_derived]
        impl #generics_for_impl #trait_with_arguments for #structure_name #structure_generics #where_clause {
            #impl_items
        }
    }
}
