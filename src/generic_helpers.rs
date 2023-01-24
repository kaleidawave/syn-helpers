use std::collections::HashMap;

use syn::{self, visit::Visit, visit_mut::VisitMut, Ident};

pub(crate) struct RenameGenerics<'a>(pub &'a HashMap<Ident, Ident>);

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

pub(crate) struct ReferencesAGeneric<'a> {
    pub(crate) found: bool,
    pub(crate) generics_on_structures: &'a syn::Generics,
}

impl<'a> ReferencesAGeneric<'a> {
    pub(crate) fn has_generic(ty: &syn::Type, generics_on_structures: &'a syn::Generics) -> bool {
        let mut state = ReferencesAGeneric {
            found: false,
            generics_on_structures,
        };
        state.visit_type(ty);
        state.found
    }
}

impl<'a, 'b> Visit<'b> for ReferencesAGeneric<'a> {
    fn visit_type_reference(&mut self, i: &'b syn::TypeReference) {
        if !self.found {
            self.visit_type(&i.elem);
        }
    }

    fn visit_type_path(&mut self, i: &'b syn::TypePath) {
        if self.found {
            return;
        }
        if let Some(path) = i.path.get_ident() {
            self.found = self
                .generics_on_structures
                .params
                .iter()
                .filter_map(|param| {
                    if let syn::GenericParam::Type(ty) = param {
                        Some(ty)
                    } else {
                        None
                    }
                })
                .any(|ty| &ty.ident == path);
        }
    }
}
