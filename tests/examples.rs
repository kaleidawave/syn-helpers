use proc_macro2::Span;
use quote::quote;
use syn::{parse_quote, DeriveInput, Expr, Ident, Stmt};
use syn_helpers::{
    derive_trait, Constructable, FieldMut, NamedOrUnnamedFieldMut, Structure, Trait, TraitItem,
    TypeOfSelf,
};

macro_rules! token_stream_eq {
    ($a:expr, $b:expr) => {
        assert_eq!($a.to_string(), $b.to_string())
    };
}

#[test]
fn binary_serialize_enum() {
    let input: DeriveInput = parse_quote! {
        enum X {
            A { a: String },
            B { b: i32 }
        }
    };

    let stream = derive_trait(
        input,
        Trait {
            name: Ident::new("BinarySerializable", Span::call_site()).into(),
            generic_parameters: None,
            items: vec![
                TraitItem::new_method(
                    Ident::new("serialize", Span::call_site()),
                    None,
                    TypeOfSelf::Reference,
                    vec![parse_quote!(buf: &mut Vec<u8>)],
                    None,
                    |mut item| {
                        item.map_constructable(|mut constructable| {
                            let iterator = constructable
                                .as_enum_variant()
                                .map(|variant| {
                                    let idx = variant.idx;
                                    parse_quote!(buf.push(#idx);)
                                })
                                .into_iter()
                                .chain(constructable.get_fields_mut().fields_iterator_mut().map(
                                    |mut field: NamedOrUnnamedFieldMut| -> Stmt {
                                        let reference = field.get_reference();
                                        parse_quote!(crate::BinarySerializable::serialize(#reference, buf);)
                                    },
                                ));
                            Ok(iterator.collect())
                        })
                    },
                ),
                TraitItem::new_associated_function(
                    Ident::new("deserialize", Span::call_site()),
                    Some(vec![parse_quote!(I: Iterator<Item = u8>)]),
                    vec![parse_quote!(iter: &mut I)],
                    Some(parse_quote!(Self)),
                    |structure| {
                        let deserialize_call: Expr =
                            parse_quote!(BinarySerializable::deserialize(iter));
                        match structure {
                            Structure::Enum(r#enum) => {
                                let indexer: Stmt =
                                    parse_quote!(let indexer = iter.next().unwrap(););

                                let bad_case = parse_quote!(
                                    unreachable!("invalid discriminant when deserializing enum");
                                );

                                Ok(std::iter::once(indexer)
                                    .chain(r#enum.get_variants().iter().map(|variant| {
                                        let idx = variant.idx as u8;
                                        let constructor = variant
                                            .build_constructor(|_| Ok(deserialize_call.clone()))
                                            .unwrap();

                                        parse_quote!(if indexer == #idx {
                                            return #constructor;
                                        })
                                    }))
                                    .chain(std::iter::once(bad_case))
                                    .collect())
                            }
                            Structure::Struct(r#struct) => r#struct
                                .build_constructor(|_| Ok(deserialize_call.clone()))
                                .map(|expr| vec![Stmt::Expr(expr)]),
                        }
                    },
                ),
            ],
        },
    );

    #[rustfmt::skip]
    let expected = quote!(
        #[automatically_derived]
        impl BinarySerializable for X {
            fn serialize(&self, buf: &mut Vec<u8>) {
                match self {
                    X::A { a: ref _0 } => {
                        buf.push(0usize);
                        _0.serialize(buf);
                    }
                    X::B { b: ref _0 } => {
                        buf.push(1usize);
                        _0.serialize(buf);
                    }
                }
            }
            fn deserialize<I: Iterator<Item = u8> >(iter: &mut I) -> Self {
                let indexer = iter.next().unwrap();
                if indexer == 0usize {
                    return X::A {
                        a: BinarySerializable::deserialize(iter)
                    };
                }
                if indexer == 1usize {
                    return X::B {
                        b: BinarySerializable::deserialize(iter)
                    };
                }
            }
        }
    );

    token_stream_eq!(stream, expected)
}
