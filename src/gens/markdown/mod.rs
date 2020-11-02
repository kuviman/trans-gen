use super::*;

fn conv(name: &str) -> String {
    name.to_owned()
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "boolean".to_owned(),
        Schema::Int32 => "int32".to_owned(),
        Schema::Int64 => "int64".to_owned(),
        Schema::Float32 => "float32".to_owned(),
        Schema::Float64 => "float64".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Option(inner) => format!("Option<{}>", type_name(inner)),
        Schema::Struct(Struct { name, .. }) => name.camel_case(conv),
        Schema::OneOf { base_name, .. } => base_name.camel_case(conv),
        Schema::Vec(inner) => format!("[{}]", type_name(inner)),
        Schema::Map(key_type, value_type) => {
            format!("Map<{} -> {}>", type_name(key_type), type_name(value_type))
        }
        Schema::Enum { base_name, .. } => base_name.camel_case(conv),
    }
}

fn get_doc<'a>(documentation: &'a Documentation, language: &str, error_msg: &str) -> &'a str {
    if let Some(text) = documentation.get(language) {
        text
    } else {
        // panic!("{}", error_msg);
        "TODO: document"
    }
}

pub struct Generator {
    parts: Vec<String>,
}
impl crate::Generator for Generator {
    fn new(name: &str, version: &str) -> Self {
        Self { parts: Vec::new() }
    }
    fn result(self) -> GenResult {
        GenResult {
            files: vec![File {
                path: "doc.md".to_owned(),
                content: self.parts.join("\n"),
            }],
        }
    }
    fn add_only(&mut self, schema: &Schema) {
        let language = "en";
        match schema {
            Schema::Struct(Struct {
                documentation,
                name,
                fields,
                magic,
            }) => {
                let mut content = Writer::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(
                        content,
                        "{}",
                        get_doc(
                            documentation,
                            language,
                            &format!("{:?} not documented in {:?}", name, language)
                        )
                    )
                    .unwrap();
                    writeln!(content).unwrap();
                    writeln!(content, "Fields:").unwrap();
                    writeln!(content).unwrap();
                    for field in fields {
                        writeln!(
                            content,
                            "- `{}`: `{}` - {}",
                            field.name.snake_case(conv),
                            type_name(&field.schema),
                            get_doc(
                                &field.documentation,
                                language,
                                &format!(
                                    "{:?}::{:?} not documented in {:?}",
                                    name, field.name, language
                                )
                            ),
                        )
                        .unwrap();
                    }
                }
                self.parts.push(content.get());
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
            } => {
                let mut content = Writer::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", base_name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(
                        content,
                        "{}",
                        get_doc(
                            documentation,
                            language,
                            &format!("{:?} not documented in {:?}", base_name, language)
                        )
                    )
                    .unwrap();
                    writeln!(content).unwrap();
                    writeln!(content, "One of:").unwrap();
                    writeln!(content).unwrap();
                    for variant in variants {
                        writeln!(
                            content,
                            "- `{}` - {}",
                            variant.name.camel_case(conv),
                            get_doc(
                                &variant.documentation,
                                language,
                                &format!(
                                    "{:?}::{:?} not documented in {:?}",
                                    base_name, variant.name, language
                                )
                            ),
                        )
                        .unwrap();
                        writeln!(content).unwrap();
                        content.inc_ident();
                        if variant.fields.is_empty() {
                            writeln!(content, "No fields").unwrap();
                        } else {
                            writeln!(content, "Fields:").unwrap();
                        }
                        writeln!(content).unwrap();
                        for field in &variant.fields {
                            writeln!(
                                content,
                                "- `{}`: `{}` - {}",
                                field.name.snake_case(conv),
                                type_name(&field.schema),
                                get_doc(
                                    &field.documentation,
                                    language,
                                    &format!(
                                        "{:?}::{:?}::{:?} not documented in {:?}",
                                        base_name, variant.name, field.name, language
                                    )
                                ),
                            )
                            .unwrap();
                        }
                        content.dec_ident();
                    }
                }
                self.parts.push(content.get());
            }
            Schema::Enum {
                documentation,
                base_name,
                variants,
            } => {
                let mut content = Writer::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", base_name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(
                        content,
                        "{}",
                        get_doc(
                            documentation,
                            language,
                            &format!("{:?} not documented in {:?}", base_name, language)
                        ),
                    )
                    .unwrap();
                    writeln!(content).unwrap();
                    writeln!(content, "Variants:").unwrap();
                    writeln!(content).unwrap();
                    for variant in variants {
                        writeln!(
                            content,
                            "- `{}` - {}",
                            variant.name.camel_case(conv),
                            get_doc(
                                &variant.documentation,
                                language,
                                &format!(
                                    "{:?}::{:?} not documented in {:?}",
                                    base_name, variant.name, language
                                )
                            ),
                        )
                        .unwrap();
                    }
                }
                self.parts.push(content.get());
            }
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String
            | Schema::Option(_)
            | Schema::Vec(_)
            | Schema::Map(_, _) => {}
        }
    }
}
