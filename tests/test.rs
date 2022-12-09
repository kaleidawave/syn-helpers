// Uses whitespace removal a lot because `quote` and macro output disagree on whitespace...

use proc_macro2::Span;
use quote::quote;
use syn::{parse_quote, DeriveInput, Ident};
use syn_helpers::{
    build_implementation_over_structure, BuildPair, Field, Fields, Trait, TraitMethod,
};

macro_rules! token_stream_eq {
    ($a:expr, $b:expr) => {
        assert_eq!($a.to_string(), $b.to_string())
    };
}

#[test]
fn derives_fields_on_struct() {
    let struct1: DeriveInput = parse_quote! {
        struct X {
            a: String,
            b: i32
        }
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_parameters: vec![parse_quote!(&self)],
            method_generics: Vec::new(),
            return_type: None,
            build_pair: BuildPair::default(),
        }],
        generic_parameters: Vec::new(),
    };

    let stream = build_implementation_over_structure(
        &struct1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, fields: &'_ mut Fields| {
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

    token_stream_eq!(
        stream,
        quote! {
            impl MyTrait for X
            where String : MyTrait , i32 : MyTrait {
                fn method_one(&self) {
                    let Self { a: _0, b: _1 } = self;
                    do_thing(&_0);
                    do_thing(&_1);
                }
            }
        }
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
        name: parse_quote!(MyTrait),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_parameters: vec![parse_quote!(&mut self), parse_quote!(a: i32)],
            method_generics: Vec::new(),
            return_type: None,
            build_pair: BuildPair::default(),
        }],
        generic_parameters: Vec::new(),
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, fields: &mut Fields| {
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

    token_stream_eq!(
        stream,
        quote! {
            impl MyTrait for X
            where i32: MyTrait, bool: MyTrait, std::collections::HashSet<u8>: MyTrait {
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
    )
}

#[test]
fn derives_fields_on_input_with_generics() {
    let enum1: DeriveInput = parse_quote! {
        struct MyReferenceType<'a, T>(&'a T);
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait),
        methods: Vec::new(),
        generic_parameters: Vec::new(),
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, _: &mut Fields| Ok(Default::default()),
    );

    token_stream_eq!(
        stream,
        quote! {
            impl<'a, T> MyTrait for MyReferenceType<'a, T> { }
        }
    )
}

#[test]
fn derives_fields_on_trait_with_generic_collision() {
    let enum1: DeriveInput = parse_quote! {
        struct MyReferenceType<'a, T>(&'a T);
    };

    // Trait has T here and in method
    let trait1 = Trait {
        name: parse_quote!(MyTrait),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_parameters: vec![parse_quote!(item: T)],
            build_pair: Default::default(),
            method_generics: Vec::new(),
            return_type: None,
        }],
        generic_parameters: vec![parse_quote!(T)],
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, _: &mut Fields| Ok(Default::default()),
    );

    // T is used on trait and structure so derive uses _gp0 in place of the T on `MyReferenceType`
    token_stream_eq!(
        stream,
        quote! {
            impl<'a, T, _gp0> MyTrait<T> for MyReferenceType<'a, _gp0> {
                fn method_one(item: T) { let Self( _ ) = self ; }
            }
        }
    )
}

#[test]
fn derive_add_where_clause() {
    let enum1: DeriveInput = parse_quote! {
        struct MyStruct<T> {
            #[read]
            item: T,
            #[read]
            a: A,
            b: B,
        }
    };

    let trait1 = Trait {
        name: parse_quote!(MyTrait),
        generic_parameters: Vec::new(),
        methods: vec![TraitMethod {
            method_name: Ident::new("method_one", Span::call_site()),
            method_generics: Vec::new(),
            method_parameters: Vec::new(),
            return_type: None,
            build_pair: Default::default(),
        }],
    };

    let stream = build_implementation_over_structure(
        &enum1,
        trait1,
        |_, _| Ok(Default::default()),
        |_, fields: &mut Fields| {
            for mut field in fields.fields_iterator() {
                if field
                    .get_attributes()
                    .iter()
                    .any(|attr| attr.path.is_ident("read"))
                {
                    let _reference = field.get_reference();
                }
            }
            Ok(Default::default())
        },
    );

    token_stream_eq!(
        stream,
        quote! {
        impl<T> MyTrait for MyStruct<T>
        where T: MyTrait, A: MyTrait {
            fn method_one() {
                let Self { item: _0, a: _1, b: _ } = self;
            }
        } }
    )
}
