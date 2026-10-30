pub mod emit;
#[cfg(feature = "emit-string")]
pub mod emit_string;

pub use proc_macro2::{Delimiter, TokenStream, TokenTree};

#[derive(Debug)]
pub enum Generics {
	Unprocessed(TokenStream),
	Processed(Vec<(String, Constraint)>)
}

impl Generics {
	pub fn names_and_constraints(&mut self) -> &mut [(String, Constraint)] {
		fn parse(stream: TokenStream) -> Vec<(String, Constraint)> {
			let mut inner = stream.into_iter();
			let mut vec = Vec::new();
			while let Some(initial) = inner.next() {
				let proc_macro2::TokenTree::Ident(initial) = initial else {
					panic!();
				};
				let initial = initial.to_string();
				let mut constraint = TokenStream::new();
				if let Some(proc_macro2::TokenTree::Punct(punct)) = inner.next() {
					if punct.as_char() == ':' {
						let mut depth: u8 = 0;
						while let Some(token) = inner.next() {
							if let proc_macro2::TokenTree::Punct(ref punct) = token {
								depth = match punct.as_char() {
									'<' => depth + 1,
									'>' => depth.saturating_sub(1),
									',' if depth == 0 => {
										break;
									},
									_ => depth
								};
							}
							constraint.extend(std::iter::once(token));
						}
					} else {
						assert_eq!(punct.as_char(), ',');
					}
				}
				vec.push((initial, Constraint(constraint)));
			}
			vec
		}

		if let Generics::Unprocessed(stream) = self {
			*self = Generics::Processed(parse(std::mem::take(stream)));
		}
		match *self {
			Generics::Unprocessed(_) => unreachable!(),
			Generics::Processed(ref mut owned) => owned
		}
	}
}

#[derive(Debug)]
pub struct Name {
	pub name: String,
	pub generics: Option<Generics>,
}

/// Anything that can have a `derive` on it
/// TODO function + rename to deriveInput
#[derive(Debug)]
pub enum Item {
	Enum(Enum),
	Struct(Struct),
}

#[derive(Debug)]
pub struct Enum {
	pub attributes: Attributes,
	pub publicity: Publicity,
	pub name: Name,
	pub variants: Vec<Variant>,
}

#[derive(Debug, Default)]
pub enum Publicity {
	#[default]
	Private,
	Public(Option<TokenStream>),
}

impl Publicity {
	pub fn to_tokens(&self) -> TokenStream {
		match self {
			Self::Public(Some(_stream)) => todo!(),
			Self::Public(None) => todo!(),
			Self::Private => TokenStream::new()
		}
	}

	pub fn to_string(&self) -> String {
		match self {
			Self::Public(Some(stream)) => format!("pub({stream}) "),
			Self::Public(None) => "pub ".to_owned(),
			Self::Private => String::new()
		}
	}
}

#[derive(Debug)]
pub struct Type(pub TokenStream);

impl Type {
	pub fn to_tokens(&self) -> TokenStream {
		self.0.clone()
	}

	pub fn to_string(&self) -> String {
		self.0.to_string()
	}
}

pub fn add_constraint<N>(field: &Field<N>, _constraint: &str, generics: &mut Generics) {
	fn matches(token: &TokenTree, m: &[(String, Constraint)], f: &mut u32) {
		if let TokenTree::Ident(ident) = token {
			let name = ident.to_string();
			if let Some(position) = m.iter().position(|(name1, _)| name == *name1) {
				*f |= 0b1 << position;
			}
		}
		// TODO group etc
	}

	let generics = generics.names_and_constraints();
	if generics.is_empty() {
		return;
	}
	let mut f: u32 = 0;
	for token in field.r#type.0.clone() {
		matches(&token, generics, &mut f);
	}
	for (position, (_, _constraints)) in generics.iter().enumerate() {
		if f & (0b1 << position) != 0 {
			// TODO and not already constrained
			// constraints.push("+")
			// constraints.push(constraint)
		}
	}
}

#[derive(Debug)]
pub struct Constraint(pub TokenStream);

#[derive(Debug)]
pub struct Variant(pub Struct);

#[derive(Debug)]
pub struct Struct {
	pub attributes: Attributes,
	pub publicity: Publicity,
	pub name: Name,
	pub fields: Fields,
}

#[derive(Debug)]
pub enum Fields {
	Unit,
	Unnamed(Vec<Field<()>>),
	Named(Vec<Field<String>>),
}

#[derive(Debug)]
pub struct Field<N> {
	pub attributes: Attributes,
	pub publicity: Publicity,
	pub name: N,
	pub r#type: Type,
}

pub type Attributes = Vec<Attribute>;

/// TODO unsafe
#[derive(Debug)]
pub struct Attribute {
	pub path: String,
	pub value: Option<(bool, TokenStream)>,
}

impl Attribute {
	pub fn name_eq(&self, is: &str) -> bool {
		self.path == is
		// let inner = self.0.clone();
		// if let Some(TokenTree::Ident(ident)) = inner.into_iter().next() {
		// 	ident == is
		// } else {
		// 	// TODO
		// 	false
		// }
	}
}

impl std::str::FromStr for Item {
	type Err = proc_macro2::LexError;

	fn from_str(source: &str) -> Result<Self, Self::Err> {
		match str::parse::<TokenStream>(source) {
			Ok(stream) => Ok(parse_item(stream).unwrap()),
			Err(err) => Err(err),
		}
	}
}

pub fn parse_item(stream: TokenStream) -> Result<Item, ()> {
	let mut input = stream.into_iter().peekable();
	let attributes = parse_attributes(&mut input);
	let publicity = parse_publicity(&mut input);

	let first = input.peek();
	if let Some(TokenTree::Ident(ident)) = first {
		if ident == "enum" {
			Ok(Item::Enum(parse_enum(&mut input, Some((attributes, publicity)))))
		} else if ident == "struct" {
			Ok(Item::Struct(parse_struct(&mut input, Some((attributes, publicity)))))
		} else {
			Err(())
		}
	} else {
		Err(())
	}
}

impl std::str::FromStr for Enum {
	type Err = proc_macro2::LexError;

	fn from_str(source: &str) -> Result<Self, Self::Err> {
		let input = str::parse::<TokenStream>(source).unwrap();
		let mut input = input.into_iter().peekable();
		Ok(parse_enum(&mut input, None))
	}
}

impl std::str::FromStr for Struct {
	type Err = proc_macro2::LexError;

	fn from_str(source: &str) -> Result<Self, Self::Err> {
		let input = str::parse::<TokenStream>(source).unwrap();
		let mut input = input.into_iter().peekable();
		Ok(parse_struct(&mut input, None))
	}
}

pub enum EitherField<'a> {
	Unnamed(&'a Field<()>),
	Named(&'a Field<String>),
}

impl<'a> EitherField<'a> {
	pub fn name(&self) -> Option<&str> {
		match self {
			EitherField::Unnamed(_field) => None,
			EitherField::Named(field) => Some(&field.name),
		}
	}

	pub fn r#type(&self) -> &Type {
		match self {
			EitherField::Unnamed(field) => &field.r#type,
			EitherField::Named(field) => &field.r#type,
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub enum FieldsRef<'a> {
	Unit,
	Unnamed(&'a [Field<()>]),
	Named(&'a [Field<String>]),
}

impl<'a> From<&'a Fields> for FieldsRef<'a> {
	fn from(fields: &'a Fields) -> Self {
		match fields {
			Fields::Unit => FieldsRef::Unit,
			Fields::Unnamed(fields) => FieldsRef::Unnamed(fields),
			Fields::Named(fields) => FieldsRef::Named(fields),
		}
	}
}

impl<'a> IntoIterator for FieldsRef<'a> {
	type Item = EitherField<'a>;
	type IntoIter = FieldsRefIter<'a>;

	fn into_iter(self) -> Self::IntoIter {
		match self {
			FieldsRef::Unit => FieldsRefIter::Unit,
			FieldsRef::Unnamed(field) => FieldsRefIter::Unnamed(field.iter()),
			FieldsRef::Named(field) => FieldsRefIter::Named(field.iter()),
		}
	}
}

pub enum FieldsRefIter<'a> {
	Unit,
	Unnamed(std::slice::Iter<'a, Field<()>>),
	Named(std::slice::Iter<'a, Field<String>>),
}

impl<'a> Iterator for FieldsRefIter<'a> {
	type Item = EitherField<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			FieldsRefIter::Unit => None,
			FieldsRefIter::Unnamed(iter) => iter.next().map(EitherField::Unnamed),
			FieldsRefIter::Named(iter) => iter.next().map(EitherField::Named),
		}
	}
}

type TokenIterator = std::iter::Peekable<<TokenStream as IntoIterator>::IntoIter>;

type ParentDecorators = (Attributes, Publicity);

fn parse_enum(input: &mut TokenIterator, parent: Option<ParentDecorators>) -> Enum {
	let (attributes, publicity): ParentDecorators = if let Some(parent) = parent {
		let _ = input.next();
		parent
	} else {
		let attributes = parse_attributes(input);
		let publicity = parse_publicity(input);
		let _ = input.next(); // TODO assert is
		(attributes, publicity)
	};
	let name = parse_name(input);
	let variants = if let Some(TokenTree::Group(group)) = input.next() {
		let item = group.stream();
		let mut input = item.into_iter().peekable();
		let mut variants: Vec<Variant> = Vec::new();
		while input.peek().is_some() {
			let attributes = parse_attributes(&mut input);
			let name = parse_name(&mut input);
			let fields = parse_fields(&mut input);
			if !matches!(fields, Fields::Unit) {
				let next = input.next();
				if let Some(TokenTree::Punct(punct)) = next {
					assert_eq!(punct.as_char(), ',');
				} else if next.is_some() {
					panic!("{next:?}");
				}
			}
			variants.push(Variant(Struct {
				publicity: Publicity::default(),
				name,
				fields,
				attributes,
			}));
		}
		variants
	} else {
		panic!();
	};
	Enum { publicity, name, variants, attributes }
}

fn parse_struct(input: &mut TokenIterator, parent: Option<ParentDecorators>) -> Struct {
	let (attributes, publicity): ParentDecorators = if let Some(parent) = parent {
		let _ = input.next();
		parent
	} else {
		let attributes = parse_attributes(input);
		let publicity = parse_publicity(input);
		let _ = input.next(); // TODO assert is
		(attributes, publicity)
	};
	let name = parse_name(input);
	let fields = parse_fields(input);
	Struct { publicity, name, fields, attributes }
}

fn parse_fields(input: &mut TokenIterator) -> Fields {
	let next = input.next();
	if let Some(TokenTree::Group(group)) = next {
		let fields = match group.delimiter() {
			Delimiter::Parenthesis => {
				let mut fields = Vec::new();
				let item = group.stream();
				let mut input = item.into_iter().peekable();
				while input.peek().is_some() {
					let attributes = parse_attributes(&mut input);
					let publicity = parse_publicity(&mut input);
					let r#type = parse_type(&mut input);
					fields.push(Field { attributes, publicity, name: (), r#type });
					let next = input.next();
					if let Some(TokenTree::Punct(punct)) = next {
						assert_eq!(punct.as_char(), ',');
					} else if next.is_some() {
						panic!("{next:?}");
					}
				}
				Fields::Unnamed(fields)
			}
			Delimiter::Brace => {
				let mut fields = Vec::new();
				let item = group.stream();
				let mut input = item.into_iter().peekable();
				loop {
					let attributes = parse_attributes(&mut input);
					let publicity = parse_publicity(&mut input);
					let Some(name) = input.next() else {
						break;
					};
					let name = if let TokenTree::Ident(ident) = name {
						ident.to_string()
					} else {
						panic!("{name:?}");
					};
					let next = input.next();
					if let Some(TokenTree::Punct(punct)) = next {
						assert_eq!(punct.as_char(), ':');
					} else {
						panic!("{next:?}");
					}

					let r#type = parse_type(&mut input);
					fields.push(Field { publicity, name, r#type, attributes });
					let next = input.next();
					if let Some(TokenTree::Punct(punct)) = next {
						assert_eq!(punct.as_char(), ',');
					} else if next.is_some() {
						panic!("{next:?}");
					}
				}
				Fields::Named(fields)
			}
			delimiter => panic!("{delimiter:?}"),
		};
		fields
	} else if let Some(TokenTree::Punct(punct)) = next {
		// ; is for struct. and , is for enum
		std::assert_matches!(punct.as_char(), ',' | ';');
		Fields::Unit
	} else if next.is_none() {
		Fields::Unit
	} else {
		panic!("{next:?}");
	}
}

fn parse_publicity(input: &mut TokenIterator) -> Publicity {
	if let Some(token) = input.peek()
		&& let TokenTree::Ident(ident) = token
		&& ident == "pub"
	{
		let _ = input.next();

		if let Some(TokenTree::Group(group)) = input.peek() {
			let stream = group.stream();
			let _ = input.next();
			Publicity::Public(Some(stream))
		} else {
			Publicity::Public(None)
		}
	} else {
		Publicity::Private
	}
}

fn parse_name(input: &mut TokenIterator) -> Name {
	let next = input.next().unwrap();
	let name = if let TokenTree::Ident(ident) = next {
		ident.to_string()
	} else {
		panic!("{next:?}");
	};
	if let Some(TokenTree::Punct(punct)) = input.peek()
		&& punct.as_char() == '<'
	{
		let _ = input.next();
		let mut generics = TokenStream::new();
		let mut height = 1;
		while let Some(token) = input.next() {
			if let TokenTree::Punct(ref punct) = token {
				match punct.as_char() {
					'>' => {
						height -= 1;
						if height == 0 {
							break;
						}
					}
					'<' => {
						height += 1;
					}
					_ => {}
				}
			}
			generics.extend(std::iter::once(token));
		}
		Name { name, generics: Some(Generics::Unprocessed(generics)) }
	} else {
		Name { name, generics: None }
	}
}

fn parse_type(input: &mut TokenIterator) -> Type {
	let mut height = 0_usize;
	let mut stream = TokenStream::new();
	while let Some(token) = input.peek() {
		if let TokenTree::Punct(punct) = token {
			match punct.as_char() {
				'>' => {
					height = height.saturating_sub(1);
				}
				'<' => {
					height += 1;
				}
				',' if height == 0 => {
					break;
				}
				_ => {}
			}
		}
		let token = input.next().unwrap();
		stream.extend(std::iter::once(token));
	}
	Type(stream)
}

fn parse_attributes(input: &mut TokenIterator) -> Attributes {
	let mut attributes = Vec::new();
	while let Some(TokenTree::Punct(punct)) = input.peek()
		&& punct.as_char() == '#'
	{
		let _ = input.next();

		if let Some(TokenTree::Group(group)) = input.next()
			&& group.delimiter() == Delimiter::Bracket
		{
			let mut input = group.stream().into_iter().peekable();

			let path = {
				let next = input.next();
				let mut path = if let Some(TokenTree::Ident(ident)) = next {
					ident.to_string()
				} else {
					panic!();
				};
				while let Some(TokenTree::Punct(punct)) = input.peek()
					&& punct.as_char() == ':'
				{
					// ::
					let _ = (input.next(), input.next());
					let next = input.next();
					if let Some(TokenTree::Ident(ident)) = next {
						use std::fmt::Write;

						let _ = write!(&mut path, "::{ident}");
					} else {
						panic!();
					}
				}
				path
			};

			let value = if let Some(next) = input.next() {
				match next {
					TokenTree::Punct(punct) if punct.as_char() == '=' => {
						let rest: TokenStream = input.collect();
						Some((true, rest))
					}
					TokenTree::Group(group) => Some((false, group.stream())),
					tt => {
						panic!("{tt:?}");
					}
				}
			} else {
				None
			};

			attributes.push(Attribute { path, value });
		} else {
			panic!();
		}
	}
	attributes
}

/// TODO should this accept Item (not function) or TokenStream
pub fn map_fields(
	_item: Item,
	_cb: impl for<'a> FnMut(EitherField<'a>) -> TokenStream,
) -> TokenStream {
	todo!();
}

/// TODO should this accept Item (not function) or TokenStream
pub fn iter_fields(
	_item: Item,
	_cb: impl for<'a> FnMut(EitherField<'a>) -> TokenStream,
) -> TokenStream {
	todo!();
}

/// TODO attributes
#[derive(Debug)]
pub enum Structure<'a> {
	Single {
		name: &'a str,
		attributes: &'a [Attribute],
		fields: FieldsRef<'a>
	},
	Parent {
		parent: &'a str,
		name: &'a str,
		attributes: (&'a [Attribute], &'a [Attribute]),
		fields: FieldsRef<'a>
	}
}

impl<'a> Structure<'a> {
	// TODO cfg emit_string
	pub fn full_name(&self) -> String {
		match self {
			Self::Single { name, .. } => name.to_owned().to_string(),
			Self::Parent { parent, name, .. } => format!("{parent}::{name}")
		}
	}

	pub fn name(&self) -> String {
		match self {
			Self::Single { name, .. } | Self::Parent { name, .. } => name.to_owned().to_string(),
		}
	}

	pub fn fields(&self) -> FieldsRef<'a> {
		match self {
			Structure::Single { fields, .. } | Structure::Parent { fields, .. } => *fields
		}
	}
}
