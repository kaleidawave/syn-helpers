use proc_macro2::Span;
use quote::quote;
use syn::{parse_quote, DeriveInput, GenericParam, Ident, Stmt};
use syn_helpers::{
    derive_trait, Constructable, FieldMut, HasAttributes, NamedOrUnnamedFieldMut, Trait, TraitItem,
    TypeOfSelf,
};

macro_rules! token_stream_eq {
    ($a:expr, $b:expr) => {
        assert_eq!($a.to_string(), $b.to_string())
    };
}

fn example_trait(generic_parameters: Option<Vec<GenericParam>>) -> Trait {
    Trait {
        name: parse_quote!(::my_crate::MyTrait),
        generic_parameters,
        items: vec![TraitItem::new_method(
            Ident::new("method_one", Span::call_site()),
            None,
            TypeOfSelf::Reference,
            Vec::default(),
            None,
            |mut item| {
                item.map_constructable(|mut constructable| {
                    Ok(constructable
                        .get_fields_mut()
                        .fields_iterator_mut()
                        .flat_map(|mut field: NamedOrUnnamedFieldMut| -> Option<Stmt> {
                            if field
                                .get_attributes()
                                .iter()
                                .any(|attr| attr.path().is_ident("ignore"))
                            {
                                None
                            } else {
                                let reference = field.get_reference();
                                Some(parse_quote!(do_thing(#reference);))
                            }
                        })
                        .collect())
                })
            },
        )],
    }
}

#[test]
fn derives_fields_on_struct() {
    let r#struct: DeriveInput = parse_quote! {
        struct X {
            a: String,
            b: i32
        }
    };

    let stream = derive_trait(r#struct, example_trait(None));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl ::my_crate::MyTrait for X {
                fn method_one(&self) {
                    let X { a: ref _0, b: ref _1 } = self;
                    do_thing(_0);
                    do_thing(_1);
                }
            }
        }
    )
}

#[test]
fn derives_fields_on_enum() {
    let r#enum: DeriveInput = parse_quote! {
        enum X {
            A(i32, bool),
            B {
                items: std::collections::HashSet<u8>
            }
        }
    };

    let stream = derive_trait(r#enum, example_trait(None));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl ::my_crate::MyTrait for X {
                fn method_one(&self) {
                    match self {
                        X::A(ref _0, ref _1) => {
                            do_thing(_0);
                            do_thing(_1);
                        }
                        X::B { items: ref _0 } => {
                            do_thing(_0);
                        }
                    }
                }
            }
        }
    )
}

#[test]
fn derives_fields_on_input_with_generics() {
    let r#enum: DeriveInput = parse_quote! {
        struct MyReferenceType<'a, T>(&'a T);
    };
    let stream = derive_trait(r#enum, example_trait(None));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl<'a, T> ::my_crate::MyTrait for MyReferenceType<'a, T>
            where &'a T: ::my_crate::MyTrait {
                fn method_one(&self) {
                    let MyReferenceType(ref _0) = self;
                    do_thing(*_0);
                }
            }
        }
    )
}

#[test]
fn derives_fields_on_trait_with_generic_collision() {
    let r#enum: DeriveInput = parse_quote! {
        struct MyReferenceType<'a, T>(&'a T);
    };

    let trait_generic_parameters = vec![GenericParam::Type(
        Ident::new("T", Span::call_site()).into(),
    )];

    let stream = derive_trait(r#enum, example_trait(Some(trait_generic_parameters)));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl<'a, T, _gp0> ::my_crate::MyTrait<T> for MyReferenceType<'a, _gp0>
            where &'a _gp0: ::my_crate::MyTrait<T> {
                fn method_one(&self) {
                    let MyReferenceType(ref _0) = self;
                    do_thing(*_0);
                }
            }
        }
    )
}

#[test]
fn derive_add_where_clause() {
    let r#struct: DeriveInput = parse_quote! {
        struct MyStruct<T> {
            item: T,
            a: A,
            #[ignore]
            b: B,
        }
    };

    let stream = derive_trait(r#struct, example_trait(None));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl<T> ::my_crate::MyTrait for MyStruct<T>
            where T: ::my_crate::MyTrait {
                fn method_one(&self) {
                    let MyStruct { item: ref _0, a: ref _1, b : _ } = self;
                    do_thing(_0);
                    do_thing(_1);
                }
            }
        }
    )
}

#[test]
fn derive_add_where_clause_to_associated_item() {
    let r#struct: DeriveInput = parse_quote! {
        struct MyStruct<T: OtherTrait> {
            item: T::X,
            a: A,
            #[ignore]
            b: B,
        }
    };

    let stream = derive_trait(r#struct, example_trait(None));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl<T: OtherTrait> ::my_crate::MyTrait for MyStruct<T>
            where T::X : ::my_crate::MyTrait {
                fn method_one(&self) {
                    let MyStruct { item: ref _0, a: ref _1, b : _ } = self;
                    do_thing(_0);
                    do_thing(_1);
                }
            }
        }
    )
}

#[test]
fn derive_add_where_clause_to_parameterized_item() {
    let r#struct: DeriveInput = parse_quote! {
        struct MyStruct<T: OtherTrait> {
            item: Box<T>,
        }
    };

    let stream = derive_trait(r#struct, example_trait(None));

    token_stream_eq!(
        stream,
        quote! {
            #[automatically_derived]
            impl<T: OtherTrait> ::my_crate::MyTrait for MyStruct<T>
            where Box<T> : ::my_crate::MyTrait {
                fn method_one(&self) {
                    let MyStruct { item: ref _0 } = self;
                    do_thing(_0);
                }
            }
        }
    )
}
