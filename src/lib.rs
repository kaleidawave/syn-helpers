#![doc = include_str!("../README.md")]

mod derive;
mod fields;
mod generic_helpers;
mod lists_and_arguments;
mod model;

use std::error::Error;

use either_n::Either2;
use proc_macro2::{Ident, Span, TokenStream};
use syn::{Attribute, ConstParam, Expr, FnArg, GenericParam, Path, Stmt, Type, TypeParam};

pub use proc_macro2;
pub use quote::{format_ident, quote, ToTokens};
pub use syn;

pub use derive::*;
pub use fields::*;
pub use lists_and_arguments::*;
pub use model::Item;

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
        | GenericParam::Type(TypeParam { ident, .. }) => ident.to_token_stream(),
        GenericParam::Lifetime(lifetime) => lifetime.to_token_stream(),
    }
}

pub fn dyn_error_to_compile_error_tokens(err: Box<dyn Error>) -> TokenStream {
    let error_as_string = syn::LitStr::new(&err.to_string(), Span::call_site());
    quote!(compile_error!(#error_as_string);)
}

/// A declaration for a Rust [trait](https://doc.rust-lang.org/rust-by-example/trait.html)
pub struct Trait {
    pub name: Path,
    pub generic_parameters: Option<Vec<GenericParam>>,
    pub items: Vec<TraitItem>,
}

/// Statements returned from the handler
type HandlerResult = Result<Vec<Stmt>, Box<dyn Error>>;

/// A item under a trait
pub enum TraitItem {
    Method {
        name: Ident,
        generic_parameters: Option<Vec<GenericParam>>,
        self_type: TypeOfSelf,
        other_parameters: Vec<FnArg>,
        return_type: Option<Type>,
        handler: Box<dyn for<'a> Fn(Item<'a>) -> HandlerResult>,
    },
    AssociatedFunction {
        name: Ident,
        generic_parameters: Option<Vec<GenericParam>>,
        parameters: Vec<FnArg>,
        return_type: Option<Type>,
        handler: Box<dyn for<'a> Fn(&'a mut Structure) -> HandlerResult>,
    },
}

/// What ownership the method requires
#[derive(Clone, Copy)]
pub enum TypeOfSelf {
    /// `&self`
    Reference,
    /// `&mut self`
    MutableReference,
    /// `self`
    Owned,
}

impl TypeOfSelf {
    fn as_parameter_tokens(&self) -> TokenStream {
        match self {
            TypeOfSelf::Reference => quote!(&self),
            TypeOfSelf::MutableReference => quote!(&mut self),
            TypeOfSelf::Owned => quote!(self),
        }
    }

    fn as_matcher_tokens(&self) -> TokenStream {
        match self {
            TypeOfSelf::Reference => quote!(ref),
            TypeOfSelf::MutableReference => quote!(ref mut),
            TypeOfSelf::Owned => TokenStream::default(),
        }
    }
}

impl TraitItem {
    /// Create a new method (something that takes `self`, `&self` or `&mut self`)
    pub fn new_method(
        name: Ident,
        generic_parameters: Option<Vec<GenericParam>>,
        self_type: TypeOfSelf,
        other_parameters: Vec<FnArg>,
        return_type: Option<Type>,
        handler: impl for<'a> Fn(Item<'a>) -> HandlerResult + 'static,
    ) -> Self {
        Self::Method {
            name,
            generic_parameters,
            self_type,
            other_parameters,
            return_type,
            handler: Box::new(handler),
        }
    }

    /// Create a new associated function (doesn't take any reference of self)
    pub fn new_associated_function(
        name: Ident,
        generic_parameters: Option<Vec<GenericParam>>,
        parameters: Vec<FnArg>,
        return_type: Option<Type>,
        handler: impl for<'a> Fn(&'a mut Structure) -> HandlerResult + 'static,
    ) -> Self {
        Self::AssociatedFunction {
            name,
            generic_parameters,
            parameters,
            return_type,
            handler: Box::new(handler),
        }
    }
}

/// An [Enum](https://doc.rust-lang.org/rust-by-example/custom_types/enum.html)
pub struct EnumStructure {
    name: Ident,
    attrs: Vec<Attribute>,
    variants: Vec<EnumVariant>,
}

impl EnumStructure {
    pub fn get_variants(&self) -> &[EnumVariant] {
        &self.variants
    }

    pub fn get_variants_mut(&mut self) -> &mut [EnumVariant] {
        self.variants.as_mut_slice()
    }
}

impl HasAttributes for EnumStructure {
    fn get_attributes(&self) -> &[Attribute] {
        &self.attrs
    }
}

/// A member of an [EnumStructure]
pub struct EnumVariant {
    full_path: Path,
    pub idx: usize,
    fields: Fields,
}

/// A [Struct](https://doc.rust-lang.org/rust-by-example/custom_types/structs.html)
pub struct StructStructure {
    name: Ident,
    fields: Fields,
}

/// A Rust structure which can be *created* (either a struct or enum variant)
pub trait Constructable {
    /// Get the path required to construct the expression
    fn get_constructor_path(&self) -> Path;

    /// Builds a constructor expression by evaluating a expression generator for each field
    fn build_constructor(
        &self,
        generator: impl Fn(NamedOrUnnamedField) -> Result<Expr, Box<dyn Error>>,
    ) -> Result<Expr, Box<dyn std::error::Error>>;

    fn get_fields(&self) -> &Fields;
    fn get_fields_mut(&mut self) -> &mut Fields;
}

impl Constructable for StructStructure {
    fn build_constructor(
        &self,
        generator: impl Fn(NamedOrUnnamedField) -> Result<Expr, Box<dyn Error>>,
    ) -> Result<Expr, Box<dyn std::error::Error>> {
        self.fields
            .to_constructor(generator, self.name.clone().into())
    }

    fn get_fields(&self) -> &Fields {
        &self.fields
    }

    fn get_fields_mut(&mut self) -> &mut Fields {
        &mut self.fields
    }

    fn get_constructor_path(&self) -> Path {
        self.name.clone().into()
    }
}

impl Constructable for EnumVariant {
    fn build_constructor(
        &self,
        generator: impl Fn(NamedOrUnnamedField) -> Result<Expr, Box<dyn Error>>,
    ) -> Result<Expr, Box<dyn std::error::Error>> {
        self.fields
            .to_constructor(generator, self.full_path.clone())
    }

    fn get_fields(&self) -> &Fields {
        &self.fields
    }

    fn get_fields_mut(&mut self) -> &mut Fields {
        &mut self.fields
    }

    fn get_constructor_path(&self) -> Path {
        self.full_path.clone()
    }
}

pub trait HasAttributes {
    fn get_attributes(&self) -> &[Attribute];
}

/// Either a [StructStructure] or [EnumStructure]
pub enum Structure {
    Struct(StructStructure),
    Enum(EnumStructure),
}

/// Either a [StructStructure] or [EnumVariant] (with it's parents attributes)
pub enum ConstructableStructure<'a> {
    Struct(&'a mut StructStructure),
    EnumVariant(&'a mut EnumVariant, &'a [Attribute]),
}

impl Structure {
    /// Iterator over all the fields
    fn all_fields(&self) -> impl Iterator<Item = NamedOrUnnamedField<'_>> {
        match self {
            Structure::Struct(r#struct) => Either2::One(r#struct.get_fields().fields_iterator()),
            Structure::Enum(r#enum) => Either2::Two(
                r#enum
                    .variants
                    .iter()
                    .flat_map(|variant| variant.get_fields().fields_iterator()),
            ),
        }
    }

    /// The top attributes
    pub fn get_attributes(&self) -> &[Attribute] {
        match self {
            // attributes have been moved to fields here
            Structure::Struct(r#struct) => r#struct.fields.get_field_attributes(),
            Structure::Enum(r#enum) => r#enum.attrs.as_slice(),
        }
    }

    /// The declaration name
    pub fn get_name(&self) -> &Ident {
        match self {
            Structure::Struct(r#struct) => &r#struct.name,
            Structure::Enum(r#enum) => &r#enum.name,
        }
    }
}

impl Constructable for ConstructableStructure<'_> {
    fn build_constructor(
        &self,
        generator: impl Fn(NamedOrUnnamedField) -> Result<Expr, Box<dyn Error>>,
    ) -> Result<Expr, Box<dyn std::error::Error>> {
        match self {
            ConstructableStructure::Struct(r#struct) => r#struct.build_constructor(generator),
            ConstructableStructure::EnumVariant(enum_variant, _) => {
                enum_variant.build_constructor(generator)
            }
        }
    }

    fn get_fields(&self) -> &Fields {
        match self {
            ConstructableStructure::Struct(r#struct) => r#struct.get_fields(),
            ConstructableStructure::EnumVariant(enum_variant, _) => enum_variant.get_fields(),
        }
    }

    fn get_fields_mut(&mut self) -> &mut Fields {
        match self {
            ConstructableStructure::Struct(r#struct) => r#struct.get_fields_mut(),
            ConstructableStructure::EnumVariant(enum_variant, _) => enum_variant.get_fields_mut(),
        }
    }

    fn get_constructor_path(&self) -> Path {
        match self {
            ConstructableStructure::Struct(r#struct) => r#struct.get_constructor_path(),
            ConstructableStructure::EnumVariant(enum_variant, _) => {
                enum_variant.get_constructor_path()
            }
        }
    }
}

impl<'a> ConstructableStructure<'a> {
    pub fn as_enum_variant(&'a self) -> Option<&'a EnumVariant> {
        if let Self::EnumVariant(variant, _) = self {
            Some(&**variant)
        } else {
            None
        }
    }

    pub fn all_attributes<'b: 'a>(&'b self) -> impl Iterator<Item = &'b Attribute> + '_ {
        match self {
            ConstructableStructure::Struct(r#struct) => {
                Either2::One(r#struct.get_fields().get_field_attributes().iter())
            }
            ConstructableStructure::EnumVariant(r#enum, parent_attrs) => Either2::Two(
                parent_attrs
                    .iter()
                    .chain(r#enum.get_fields().get_field_attributes().iter()),
            ),
        }
    }
}

/// Prints a path out as its source representation
pub fn path_to_string(path: Path) -> String {
    let mut buf = String::new();
    if path.leading_colon.is_some() {
        buf.push_str("::");
    }
    for (idx, segment) in path.segments.iter().enumerate() {
        buf.push_str(&segment.ident.to_string());
        if !segment.arguments.is_empty() {
            todo!()
        }
        if idx != path.segments.len() - 1 {
            buf.push_str("::");
        }
    }
    buf
}
