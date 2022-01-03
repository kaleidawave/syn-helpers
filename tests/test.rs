use proc_macro2::Span;
use quote::quote;
use syn::{parse_quote, DeriveInput, Ident};
use syn_helpers::{build_implementation_over_structure, BuildPair, Field, Trait, TraitMethod};

#[test]
fn derives_fields_on_struct() {
    let struct1: DeriveInput = parse_quote! {
        struct X {
            a: String,
            b: i32
        }
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait1),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_parameters: vec![parse_quote!(&self)],
            method_generics: vec![],
            return_type: None,
            build_pair: BuildPair::default(),
        }],
        generic_parameters: vec![],
    };

    let stream = build_implementation_over_structure(
        &struct1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, fields| {
            let fields_iterator = fields.fields_iterator();
            assert_eq!(fields_iterator.len(), 2);
            Ok(fields_iterator
                .map(|mut field| {
                    let reference = field.get_reference();
                    parse_quote! {
                        do_thing(&#reference);
                    }
                })
                .collect())
        },
    );

    assert_eq!(
        stream.to_string(),
        quote! {
            impl MyTrait1 for X {
                fn method_one(&self) {
                    let Self { a: _0, b: _1 } = self;
                    do_thing(&_0);
                    do_thing(&_1);
                }
            }
        }
        .to_string()
    )
}

#[test]
fn derives_fields_on_enum() {
    let enum1: DeriveInput = parse_quote! {
        enum X {
            A(i32, bool),
            B {
                items: std::collections::HashSet<u8>
            }
        }
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait1),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_parameters: vec![parse_quote!(&mut self), parse_quote!(a: i32)],
            method_generics: vec![],
            return_type: None,
            build_pair: BuildPair::default(),
        }],
        generic_parameters: vec![],
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, fields| {
            let fields_iterator = fields.fields_iterator();
            Ok(fields_iterator
                .map(|mut field| {
                    let reference = field.get_reference();
                    parse_quote! {
                        do_thing(&mut #reference);
                    }
                })
                .collect())
        },
    );

    assert_eq!(
        stream.to_string(),
        quote! {
            impl MyTrait1 for X {
                fn method_one(&mut self, a: i32) {
                    match self {
                        Self::A(_0, _1) => {
                            do_thing(&mut _0);
                            do_thing(&mut _1);
                        },
                        Self::B { items: _0 } => {
                            do_thing(&mut _0);
                        },
                    }
                }
            }
        }
        .to_string()
    )
}

#[test]
fn derives_fields_on_input_with_generics() {
    let enum1: DeriveInput = parse_quote! {
        struct MyReferenceType<'a, T>(&'a T);
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait1),
        methods: vec![],
        generic_parameters: vec![],
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, _| Ok(Default::default()),
    );

    assert_eq!(
        stream.to_string(),
        quote! {
            impl<'a, T> MyTrait1 for MyReferenceType<'a, T> { }
        }
        .to_string()
    )
}

#[test]
fn derives_fields_on_trait_with_generic_collision() {
    let enum1: DeriveInput = parse_quote! {
        struct MyReferenceType<'a, T>(&'a T);
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait1),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_parameters: vec![parse_quote!(item: T)],
            build_pair: Default::default(),
            method_generics: vec![],
            return_type: None,
        }],
        generic_parameters: vec![parse_quote!(T)],
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, _| Ok(Default::default()),
    );

    // T is used on trait and structure so derive uses Gp0 in place of the T on `MyReferenceType`
    assert_eq!(
        stream.to_string(),
        quote! {
            impl<'a, T, Gp0> MyTrait1<T> for MyReferenceType<'a, Gp0> {
                fn method_one(item: T) { let Self (_) = self ; }
            }
        }
        .to_string()
    )
}
