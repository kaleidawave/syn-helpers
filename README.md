# Syn-Helpers

[![crates.io badge](https://img.shields.io/crates/v/syn-helpers)](https://crates.io/crates/syn-helpers)
[![docs.rs badge](https://img.shields.io/docsrs/syn-helpers)](https://docs.rs/syn-helpers/latest/syn_helpers/)

Framework for building derive proc macros over structures (`struct` and `enum`).

Handles:
- Getting expressions referencing fields
- Building patterns for enums
- Using the same logic for deriving over a enum or struct
- Error handling and generating `compile_error` output
- Generics on trait and structure (including conflict rectification)

This crate extends (and re-exports) the excellent [syn](https://crates.io/crates/syn) and [quote](https://crates.io/crates/quote) crates.

## Example

Evaluate `do_thing` function on every field (expect those with the `#[ignore]` attribute)

```rust
use syn_helpers::{
    syn::{parse_quote, DeriveInput, GenericParam, Ident, Stmt}, proc_macro2::Span, quote,
    derive_trait, FieldMut, HasAttributes, Trait, TraitItem, TypeOfSelf, Constructable,
};

let my_trait = Trait {
    name: parse_quote!(::my_crate::MyTrait),
    generic_parameters: None,
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
                    .flat_map(|mut field| -> Option<Stmt> {
                        if field
                            .get_attributes()
                            .iter()
                            .any(|attr| attr.path.is_ident("ignore"))
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
};

let r#struct: DeriveInput = parse_quote! {
    struct X {
        a: String,
        b: i32
    }
};

let stream = derive_trait(r#struct, my_trait);

assert_eq!(
    stream.to_string(),
    quote! {
        #[automatically_derived]
        impl ::my_crate::MyTrait for X {
            fn method_one(&self) {
                let X { a: ref _0, b: ref _1 } = self;
                do_thing(_0);
                do_thing(_1);
            }
        }
    }.to_string()
)
```

### Used in
- [self-rust-tokenize](https://github.com/kaleidawave/self-rust-tokenize)
- [derive-partial-eq-extras](https://github.com/kaleidawave/derive-partial-eq-extras)
- [derive-debug-extras](https://github.com/kaleidawave/derive-debug-extras)
- [Ezno's parser](https://github.com/kaleidawave/ezno/blob/main/parser/visitable-derive/macro.rs)

Design is a *work in progress*
