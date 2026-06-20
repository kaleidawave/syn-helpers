// TODO CFG for each
use crate::{Enum, Fields};

pub fn enum_to_match_tokens(r#enum: Enum) -> proc_macro2::TokenStream {
	use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

	fn i(name: &str) -> TokenTree {
		TokenTree::Ident(Ident::new(name, Span::call_site()))
	}

	let mut group = TokenStream::new();
	let mut seen = true;
	for variant in r#enum.variants {
		if !std::mem::take(&mut seen) {
			group.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
		}

		for attribute in variant.0.attributes {
			if attribute.name_eq("cfg") {
				group.extend([
					TokenTree::Punct(Punct::new('#', Spacing::Alone)),
					TokenTree::Group(Group::new(
						Delimiter::Bracket,
						TokenStream::from_iter([
							i("cfg"),
							TokenTree::Group(Group::new(
								Delimiter::Parenthesis,
								attribute.value.as_ref().unwrap().1.clone(),
							)),
						]),
					)),
				]);
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
							group.extend([
								TokenTree::Punct(Punct::new('#', Spacing::Alone)),
								TokenTree::Group(Group::new(
									Delimiter::Bracket,
									TokenStream::from_iter([
										i("cfg"),
										TokenTree::Group(Group::new(
											Delimiter::Parenthesis,
											attribute.value.as_ref().unwrap().1.clone(),
										)),
									]),
								)),
							]);
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
			i("todo"),
			TokenTree::Punct(Punct::new('!', Spacing::Joint)),
			TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
		]);
	}

	let group = TokenTree::Group(Group::new(Delimiter::Brace, group));
	TokenStream::from_iter([i("match"), i("value"), group])
}

pub fn enum_to_match_string(r#enum: Enum) -> String {
	use std::fmt::Write;

	let mut buf = "match value {\n".to_owned();
	for (idx, variant) in r#enum.variants.iter().enumerate() {
		for attribute in &variant.0.attributes {
			if attribute.name_eq("cfg") {
				let _ = writeln!(
					&mut buf,
					"\t#[cfg({stream})]",
					stream = attribute.value.as_ref().unwrap().1
				);
			}
		}

		write!(
			&mut buf,
			"\t{enum_name}::{variant_name}",
			enum_name = r#enum.name.name,
			variant_name = variant.0.name.name
		)
		.unwrap();
		match &variant.0.fields {
			Fields::Unit => {}
			Fields::UnnamedFields(fields) => {
				let _ = write!(&mut buf, "(");
				for idx in 0..fields.len() {
					// TODO cfg?

					// TODO could be a better name
					if idx > 0 {
						let _ = write!(&mut buf, ", _field{idx}");
					} else {
						let _ = write!(&mut buf, "_field{idx}");
					}
				}
				let _ = write!(&mut buf, ")");
			}
			Fields::NamedFields(fields) => {
				let _ = write!(&mut buf, " {{ ");
				for (idx, field) in fields.iter().enumerate() {
					if idx > 0 {
						let _ = write!(&mut buf, ", ");
					}

					for attribute in &field.attributes {
						if attribute.name_eq("cfg") {
							let _ = write!(
								&mut buf,
								"#[cfg({stream})] ",
								stream = attribute.value.as_ref().unwrap().1
							);
						}
					}

					let _ = write!(&mut buf, "{name}", name = &field.name);
				}
				let _ = write!(&mut buf, " }}");
			}
		}

		let end = if idx + 1 == r#enum.variants.len() { "" } else { "," };

		let _ = writeln!(&mut buf, " => todo!(){end}");
	}
	let _ = write!(&mut buf, "}}");
	buf
}

#[cfg(test)]
mod tests {
	#[test]
	fn to_enum() {
		let item = str::parse::<crate::Item>("enum X { A, B, C }");
		let Ok(crate::Item::Enum(r#enum)) = item else {
			panic!("{item:?}");
		};
		let out = super::enum_to_match_string(r#enum);
		let expected = "
match value {
	X::A => todo!(),
	X::B => todo!(),
	X::C => todo!()
}
		"
		.trim();
		assert_eq!(&out, expected);
	}

	#[test]
	fn to_enum_cfg() {
		let item = str::parse::<crate::Item>(
			"enum X { #[cfg(windows)] Windows, B(usize), C { #[cfg(feature=\"x\")] a: Vec<String>, b: String } }",
		);
		let Ok(crate::Item::Enum(r#enum)) = item else {
			panic!("{item:?}");
		};
		let out = super::enum_to_match_string(r#enum);
		let expected = "
match value {
	#[cfg(windows)]
	X::Windows => todo!(),
	X::B(_field0) => todo!(),
	X::C { #[cfg(feature = \"x\")] a, b } => todo!()
}
		"
		.trim();
		assert_eq!(&out, expected);
	}
}
