pub mod emit;

use proc_macro2::{Delimiter, TokenStream, TokenTree};

#[derive(Debug)]
pub struct Name {
	pub name: String,
	pub generics: Option<TokenStream>,
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

#[derive(Debug)]
pub struct Type(pub TokenStream);

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
	UnnamedFields(Vec<Field<()>>),
	NamedFields(Vec<Field<String>>),
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
		let input = str::parse::<TokenStream>(source).unwrap();
		let mut input = input.into_iter().peekable();

		let attributes = parse_attributes(&mut input);
		let publicity = parse_publicity(&mut input);

		let first = input.next();
		if let Some(TokenTree::Ident(ref ident)) = first {
			if ident == "enum" {
				let name = parse_name(&mut input);
				let variants = if let Some(TokenTree::Group(group)) = input.next() {
					let item = group.stream();
					let mut input = item.into_iter().peekable();
					let mut variants: Vec<Variant> = Vec::new();
					while input.peek().is_some() {
						let attributes = parse_attributes(&mut input);
						let name = parse_name(&mut input);
						let fields = parse_fields(&mut input);
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
				Ok(Item::Enum(Enum { publicity, name, variants, attributes }))
			} else if ident == "struct" {
				let name = parse_name(&mut input);
				let fields = parse_fields(&mut input);
				Ok(Item::Struct(Struct { publicity, name, fields, attributes }))
			} else {
				panic!("{first:?}");
			}
		} else {
			panic!("{first:?}");
		}
	}
}

type TokenIterator = std::iter::Peekable<<TokenStream as IntoIterator>::IntoIter>;

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
				Fields::UnnamedFields(fields)
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
				Fields::NamedFields(fields)
			}
			delimiter => panic!("{delimiter:?}"),
		};
		let next = input.next();
		if let Some(TokenTree::Punct(punct)) = next {
			assert_eq!(punct.as_char(), ',');
		} else if next.is_some() {
			panic!("{next:?}");
		}
		fields
	} else if let Some(TokenTree::Punct(punct)) = next {
		assert_eq!(punct.as_char(), ',');
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
		Name { name, generics: Some(generics) }
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
