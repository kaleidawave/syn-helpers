use std::fmt::Debug;

use proc_macro2::Ident;
use syn::{
    parse::{self, Parse},
    punctuated::Punctuated,
    Token,
};

/// A comma separated list of `T`
pub struct CommaSeparatedList<T>(pub Punctuated<T, Token![,]>);

impl<T: Parse> Parse for CommaSeparatedList<T> {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        input.parse_terminated(T::parse, Token![,]).map(Self)
    }
}

impl<T> IntoIterator for CommaSeparatedList<T> {
    type Item = T;
    type IntoIter = syn::punctuated::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// A argument with a name and a value. '=' separated, e.g. `name=AST`
#[derive(Debug)]
pub struct Argument<T>(pub String, pub T);

impl<T: Parse> Parse for Argument<T> {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?.to_string();
        input.parse::<Token![=]>()?;
        Ok(Self(ident, input.parse()?))
    }
}

pub type CommaSeparatedArgumentList<T> = CommaSeparatedList<Argument<T>>;
