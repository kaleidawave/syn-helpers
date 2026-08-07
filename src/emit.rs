use crate::{EitherField, Enum, Fields};

use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

fn i(name: &str) -> TokenTree {
	TokenTree::Ident(Ident::new(name, Span::call_site()))
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

pub fn enum_to_match(r#enum: Enum) -> TokenStream {
	let mut group = TokenStream::new();
	let mut seen = true;
	for variant in r#enum.variants {
		if !std::mem::take(&mut seen) {
			group.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
		}

		for attribute in variant.0.attributes {
			if attribute.name_eq("cfg") {
				attr(&mut group, "cfg", attribute.value.as_ref().unwrap().1.clone());
			}
		}

		group.extend([
			i(&r#enum.name.name),
			TokenTree::Punct(Punct::new(':', Spacing::Joint)),
			TokenTree::Punct(Punct::new(':', Spacing::Alone)),
			i(&variant.0.name.name),
		]);

		match variant.0.fields {
			Fields::Unit => {}
			Fields::UnnamedFields(fields) => {
				let mut inner = TokenStream::new();
				for idx in 0..fields.len() {
					if idx > 0 {
						inner.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
					}
					// TODO could be a better name
					inner.extend([i(&format!("_field{idx}"))]);
				}
				group.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, inner))]);
			}
			Fields::NamedFields(fields) => {
				let mut inner = TokenStream::new();
				for (idx, field) in fields.iter().enumerate() {
					if idx > 0 {
						inner.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
					}
					for attribute in &field.attributes {
						if attribute.name_eq("cfg") {
							attr(&mut group, "cfg", attribute.value.as_ref().unwrap().1.clone());
						}
					}
					inner.extend([i(&field.name)]);
				}
				group.extend([TokenTree::Group(Group::new(Delimiter::Brace, inner))]);
			}
		}

		group.extend([
			TokenTree::Punct(Punct::new('=', Spacing::Joint)),
			TokenTree::Punct(Punct::new('>', Spacing::Alone)),
		]);

		group.extend([
			i("todo"),
			TokenTree::Punct(Punct::new('!', Spacing::Joint)),
			TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
		]);
	}

	let group = TokenTree::Group(Group::new(Delimiter::Brace, group));
	TokenStream::from_iter([i("match"), i("value"), group])
}

pub fn fields_to_constructor<F: for<'a> FnMut(EitherField<'_>, &'a mut TokenStream)>(
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
		Fields::UnnamedFields(fields) => {
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
		Fields::NamedFields(fields) => {
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
