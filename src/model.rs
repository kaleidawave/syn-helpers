use std::error::Error;

use quote::quote;
use syn::{parse_quote, Stmt};

use crate::{ConstructableStructure, EnumStructure, Structure, TypeOfSelf};

pub struct Item<'a> {
    pub structure: &'a mut Structure,
    pub self_type: TypeOfSelf,
}

impl Item<'_> {
    /// Evaluates every constructable form of item and asks to produce a vector of statements to operate on item
    pub fn map_constructable(
        &mut self,
        cb: impl for<'a> Fn(ConstructableStructure<'a>) -> Result<Vec<Stmt>, Box<dyn Error>>,
    ) -> Result<Vec<Stmt>, Box<dyn Error>> {
        let type_of_self = self.self_type;
        match self.structure {
            Structure::Enum(EnumStructure {
                attrs: ref enum_attrs,
                variants,
                ..
            }) => {
                let branches = variants
                    .iter_mut()
                    .map(|variant| {
                        let result = cb(ConstructableStructure::EnumVariant(variant, enum_attrs));
                        match result {
                            Ok(lines) => {
                                let pattern = variant
                                    .fields
                                    .to_pattern(variant.full_path.clone(), type_of_self);
                                Ok(quote!( #pattern => {
                                    #(#lines)*
                                }))
                            }
                            Err(err) => Err(err),
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(vec![parse_quote!(match self {
                    #(#branches)*
                })])
            }
            Structure::Struct(r#struct) => {
                let result = cb(ConstructableStructure::Struct(r#struct));
                match result {
                    Ok(mut lines) => {
                        let pattern = r#struct
                            .fields
                            .to_pattern(r#struct.name.clone().into(), self.self_type);
                        let destructuring_stmt = parse_quote! {
                            let #pattern = self;
                        };
                        lines.insert(0, destructuring_stmt);
                        Ok(lines)
                    }
                    Err(err) => Err(err),
                }
            }
        }
    }

    pub fn evaluate_pattern() {}
}
