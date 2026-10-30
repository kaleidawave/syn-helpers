use crate::{EitherField, Enum, Fields, FieldsRef, Structure, Item};

use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

fn i(name: &str) -> TokenTree {
	TokenTree::Ident(Ident::new(name, Span::call_site()))
}

pub fn iter_self(
	item: &Item,
	mut cb: impl for<'a> FnMut(Structure<'a>) -> TokenStream,
) -> TokenStream {
	match item {
		Item::Enum(r#enum) => {
			enum_to_match(&r#enum, "self", cb)
		}
		Item::Struct(r#struct) => {
			let structure = Structure::Single { name: &r#struct.name.name, fields: FieldsRef::from(&r#struct.fields), attributes: &r#struct.attributes };
			let mut inner = TokenStream::new();
			inner.extend(std::iter::once(i("let")));
			emit_destructor(&structure, &mut inner);
			inner.extend([
				TokenTree::Punct(Punct::new('=', Spacing::Alone)),
				i("self"),
				TokenTree::Punct(Punct::new(';', Spacing::Alone))
			]);
			inner.extend(cb(structure));
			inner
		}
	}
}

pub fn compile_error(message: &str) -> TokenStream {
	let inner = TokenStream::from_iter([TokenTree::Literal(str::parse(message).unwrap())]);
	let inner = TokenTree::Group(Group::new(Delimiter::Parenthesis, inner));
	let inner = TokenStream::from_iter([i("compile_error"), inner]);
	let inner = Group::new(Delimiter::Bracket, inner);
	TokenStream::from_iter([
		TokenTree::Punct(Punct::new('#', Spacing::Alone)),
		TokenTree::Group(inner),
	])
}

fn attr(on: &mut TokenStream, name: &str, inner: TokenStream) {
	on.extend([
		TokenTree::Punct(Punct::new('#', Spacing::Alone)),
		TokenTree::Group(Group::new(
			Delimiter::Bracket,
			TokenStream::from_iter([
				i(name),
				TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)),
			]),
		)),
	]);
}

pub fn emit_destructor(structure: &Structure, group: &mut TokenStream) {
	emit_destructor_suffix(structure, group, "")
}

pub fn emit_destructor_suffix(structure: &Structure, group: &mut TokenStream, suffix: &str) {
	match structure {
		Structure::Single {
			name,
			attributes: _attributes,
			fields: _
		} => {
			group.extend(std::iter::once(i(name)));
		},
		Structure::Parent {
			parent,
			name,
			attributes: _,
			fields: _
		} => {
			group.extend([
				i(parent),
				TokenTree::Punct(Punct::new(':', Spacing::Joint)),
				TokenTree::Punct(Punct::new(':', Spacing::Alone)),
				i(name),
			]);
		}
	}

	match structure.fields() {
		FieldsRef::Unit => {}
		FieldsRef::Unnamed(ref fields) => {
			let mut inner = TokenStream::new();
			for idx in 0..fields.len() {
				if idx > 0 {
					inner.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
				}
				// TODO attribute
				// TODO could be a better name
				inner.extend([i(&format!("field_{idx}{suffix}"))]);
			}
			group.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
		}
		FieldsRef::Named(ref fields) => {
			let mut inner = TokenStream::new();
			for (idx, field) in fields.iter().enumerate() {
				if idx > 0 {
					inner.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
				}
				for attribute in &field.attributes {
					if attribute.name_eq("cfg") {
						attr(&mut inner, "cfg", attribute.value.as_ref().unwrap().1.clone());
					}
				}
				let name = &field.name;
				if suffix.is_empty() {
					inner.extend(std::iter::once(i(name)));
				} else {
					inner.extend(std::iter::once(i(&format!("{name}{suffix}"))));
				}
			}
			group.extend([TokenTree::Group(Group::new(Delimiter::Brace, inner))]);
		}
	}
}

/// TODO Structure::Variant
pub fn enum_to_match(r#enum: &Enum, matcher: &str, mut cb: impl for<'a> FnMut(Structure<'a>) -> TokenStream) -> TokenStream {
	let mut group = TokenStream::new();
	let mut seen = true;
	for variant in &r#enum.variants {
		if !std::mem::take(&mut seen) {
			group.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
		}

		for attribute in &variant.0.attributes {
			if attribute.name_eq("cfg") {
				attr(&mut group, "cfg", attribute.value.as_ref().unwrap().1.clone());
			}
		}

		let structure = Structure::Parent {
			parent: &r#enum.name.name,
			name: &variant.0.name.name,
			fields: FieldsRef::from(&variant.0.fields),
			attributes: (&r#enum.attributes, &variant.0.attributes)
		};

		emit_destructor(&structure, &mut group);

		let inner = cb(structure);
		let body = TokenTree::Group(Group::new(Delimiter::Brace, inner));
		group.extend([
			TokenTree::Punct(Punct::new('=', Spacing::Joint)),
			TokenTree::Punct(Punct::new('>', Spacing::Alone)),
			body
		]);
	}

	let group = TokenTree::Group(Group::new(Delimiter::Brace, group));
	TokenStream::from_iter([i("match"), i(matcher), group])
}

pub fn fields_to_structure<F: for<'a> FnMut(EitherField<'_>, &'a mut TokenStream)>(
	path: Vec<String>,
	fields: Fields,
	mut field_cb: F,
) -> TokenStream {
	map_fields(path, fields, |field, stream| {
		if let Some(name) = field.name() {
			stream.extend([i(name), TokenTree::Punct(Punct::new(':', Spacing::Alone))]);
		}
		field_cb(field, stream);
	})
}

fn map_fields<F: for<'a> FnMut(EitherField<'_>, &'a mut TokenStream)>(
	path: Vec<String>,
	fields: Fields,
	mut field_cb: F,
) -> TokenStream {
	let mut path_iter = path.into_iter();
	let mut stream = TokenStream::new();
	let ident = Ident::new(&path_iter.next().expect("no path"), Span::call_site());
	stream.extend(std::iter::once(ident));
	for part in path_iter {
		stream.extend([Punct::new(':', Spacing::Joint), Punct::new(':', Spacing::Alone)]);
		let ident = Ident::new(&part, Span::call_site());
		stream.extend(std::iter::once(ident));
	}
	match fields {
		Fields::Unit => {}
		Fields::Unnamed(fields) => {
			let mut inner = TokenStream::new();
			for (idx, field) in fields.iter().enumerate() {
				if idx > 0 {
					let punct = Punct::new(',', Spacing::Alone);
					stream.extend(std::iter::once(punct));
				}

				for attribute in &field.attributes {
					if attribute.name_eq("cfg") {
						attr(&mut inner, "cfg", attribute.value.as_ref().unwrap().1.clone());
					}
				}
				field_cb(EitherField::Unnamed(field), &mut stream);
			}
			stream.extend(std::iter::once(Group::new(Delimiter::Parenthesis, inner)));
		}
		Fields::Named(fields) => {
			let mut inner = TokenStream::new();
			for (idx, field) in fields.iter().enumerate() {
				if idx > 0 {
					let punct = Punct::new(',', Spacing::Alone);
					stream.extend(std::iter::once(punct));
				}

				for attribute in &field.attributes {
					if attribute.name_eq("cfg") {
						attr(&mut inner, "cfg", attribute.value.as_ref().unwrap().1.clone());
					}
				}
				field_cb(EitherField::Named(field), &mut stream);
			}
			stream.extend(std::iter::once(Group::new(Delimiter::Bracket, inner)));
		}
	}
	stream
}
