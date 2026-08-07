use crate::{EitherField, EitherFields, Enum, Fields};

pub fn enum_to_match<F: for<'a> FnMut(EitherFields<'a>, &'a mut String)>(
	r#enum: Enum,
	mut cb: F,
) -> String {
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
			Fields::Unit => {
				buf.push_str(" => ");
				(cb)(EitherFields::Unit, &mut buf);
			}
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
				let _ = write!(&mut buf, ") => ");
				(cb)(EitherFields::Unnamed(fields), &mut buf);
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
				let _ = write!(&mut buf, " }} => ");
				(cb)(EitherFields::Named(fields), &mut buf);
			}
		}

		if idx + 1 != r#enum.variants.len() && !buf.ends_with("}") {
			buf.push(',');
		}
		let _ = writeln!(&mut buf);
	}
	let _ = write!(&mut buf, "}}");
	buf
}

pub fn fields_to_constructor<F: for<'a> FnMut(EitherField<'_>, &'a mut String)>(
	path: Vec<String>,
	fields: Fields,
	mut field_cb: F,
) -> String {
	map_fields(path, fields, |field, buf| {
		if let Some(name) = field.name() {
			buf.push('\t');
			buf.push_str(name);
			buf.push_str(": ");
		}
		field_cb(field, buf);
	})
}

pub fn fields_to_deconstructor(path: Vec<String>, fields: Fields) -> String {
	map_fields(path, fields, |field, buf| {
		if let Some(name) = field.name() {
			buf.push('\t');
			buf.push_str(name);
		} else {
			// TODO index?
			buf.push_str("_field");
		}
	})
}

fn map_fields<F: for<'a> FnMut(EitherField<'_>, &'a mut String)>(
	path: Vec<String>,
	fields: Fields,
	mut field_cb: F,
) -> String {
	use std::fmt::Write;

	let mut path_iter = path.into_iter();
	let mut buf = path_iter.next().expect("no path");
	for part in path_iter {
		buf.push_str("::");
		buf.push_str(&part);
	}
	match fields {
		Fields::Unit => {}
		Fields::UnnamedFields(fields) => {
			buf.push('(');
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
				field_cb(EitherField::Unnamed(field), &mut buf);
			}
			buf.push(')');
		}
		Fields::NamedFields(fields) => {
			buf.push_str(" {\n");
			for (idx, field) in fields.iter().enumerate() {
				if idx > 0 {
					let _ = writeln!(&mut buf, ",");
				}

				for attribute in &field.attributes {
					if attribute.name_eq("cfg") {
						let _ = writeln!(
							&mut buf,
							"\t#[cfg({stream})]",
							stream = attribute.value.as_ref().unwrap().1
						);
					}
				}

				field_cb(EitherField::Named(field), &mut buf);
			}
			let _ = writeln!(&mut buf);
			buf.push('}');
		}
	}
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
		let out = super::enum_to_match(r#enum, |_, buf| buf.push_str("todo!()"));
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
		let out = super::enum_to_match(r#enum, |_, buf| buf.push_str("todo!()"));
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

	#[test]
	fn to_constructable() {
		let item = str::parse::<crate::Item>(
			"struct C { #[cfg(feature=\"x\")] a: Vec<String>, b: String }",
		);
		let Ok(crate::Item::Struct(r#struct)) = item else {
			panic!("{item:?}");
		};
		let out =
			super::fields_to_constructor(vec![r#struct.name.name], r#struct.fields, |_, buf| {
				buf.push_str("Default::default()")
			});
		let expected = "
C {
	#[cfg(feature = \"x\")]
	a: Default::default(),
	b: Default::default()
}
		"
		.trim();
		assert_eq!(&out, expected);
	}

	#[test]
	fn to_destructor() {
		let item = str::parse::<crate::Item>(
			"struct C { #[cfg(feature=\"x\")] a: Vec<String>, b: String }",
		);
		let Ok(crate::Item::Struct(r#struct)) = item else {
			panic!("{item:?}");
		};
		let out = super::fields_to_deconstructor(vec![r#struct.name.name], r#struct.fields);
		let expected = "
C {
	#[cfg(feature = \"x\")]
	a,
	b
}
		"
		.trim();
		assert_eq!(&out, expected);
	}
}
