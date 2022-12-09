mod fields;
mod lists_and_arguments;
mod structure;
mod visit_fields;

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{ConstParam, FnArg, GenericParam, LifetimeDef, Path, Stmt, Type, TypeParam};

pub use fields::*;
pub use lists_and_arguments::*;
pub use structure::*;
pub use visit_fields::*;

/// Whether the method takes two instances of self
/// Used for comparisons (e.g. PartialEq)
#[derive(Default)]
pub enum BuildPair {
    Pair {
        statements_if_enums_do_not_match: Vec<Stmt>,
        other_item_name: Ident,
    },
    #[default]
    NoPairing,
}

impl BuildPair {
    pub(crate) fn is_pair(&self) -> bool {
        matches!(self, Self::Pair { .. })
    }
}

/// A representation of trait to be implemented
pub struct Trait {
    pub name: Path,
    pub generic_parameters: Vec<GenericParam>,
    pub methods: Vec<TraitMethod>,
}

/// A method under a trait
pub struct TraitMethod {
    pub method_name: Ident,
    pub method_generics: Vec<GenericParam>,
    pub method_parameters: Vec<FnArg>,
    pub return_type: Option<Type>,
    /// Whether this method is in takes two of self, for use in comparison
    pub build_pair: BuildPair,
}

/// A prefix (before) and postfix (after) pairing
pub struct PrefixAndPostfix<T> {
    pub prefix: Vec<T>,
    pub postfix: Vec<T>,
}

// `#[derive(Default)]` generates bounds on T :(
impl<T> Default for PrefixAndPostfix<T> {
    fn default() -> Self {
        Self {
            prefix: Default::default(),
            postfix: Default::default(),
        }
    }
}

/// Returns true if same variant and same name, ignores bounds
pub(crate) fn generic_parameters_have_same_name(
    generic_parameter1: &GenericParam,
    generic_parameter2: &GenericParam,
) -> bool {
    match (generic_parameter1, generic_parameter2) {
        (GenericParam::Type(gtp1), GenericParam::Type(gtp2)) => gtp1.ident == gtp2.ident,
        (GenericParam::Lifetime(glp1), GenericParam::Lifetime(glp2)) => {
            glp1.lifetime.ident == glp2.lifetime.ident
        }
        (GenericParam::Const(gcp1), GenericParam::Const(gcp2)) => gcp1.ident == gcp2.ident,
        _ => false,
    }
}

/// Removes the bounds and uses the parameter as a reference
pub(crate) fn generic_param_to_generic_argument_token_stream(
    trait_generic_parameter: &GenericParam,
) -> TokenStream {
    match trait_generic_parameter {
        GenericParam::Const(ConstParam { ident, .. })
        | GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
        GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
    }
}
