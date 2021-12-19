mod fields_and_structures;
mod lists_and_arguments;
mod visit_fields;

pub use fields_and_structures::*;
pub use lists_and_arguments::*;
use proc_macro2::Ident;
use syn::{FnArg, GenericParam, Path, Stmt, Type};
pub use visit_fields::*;

pub use inflector::cases::snakecase::to_snake_case as str_to_snake_case;

pub enum BuildPair {
    Pair {
        statements_if_enums_do_not_match: Vec<Stmt>,
        other_item_name: Ident,
    },
    NoPairing,
}

impl Default for BuildPair {
    fn default() -> Self {
        BuildPair::NoPairing
    }
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
    pub method_parameters: Vec<FnArg>,
    pub method_generics: Vec<GenericParam>,
    pub return_type: Option<Type>,
    /// Whether this method is in takes two of self, for use in comparison
    pub build_pair: BuildPair,
}

/// A prefix (before) and postfix (after) pairing
pub struct PrefixAndPostfix<T> {
    pub prefix: Vec<T>,
    pub postfix: Vec<T>,
}

// `#[derive(Default)]` would require
impl<T> Default for PrefixAndPostfix<T> {
    fn default() -> Self {
        Self {
            prefix: Default::default(),
            postfix: Default::default(),
        }
    }
}
