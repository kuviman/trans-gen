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

#[derive(Debug, Serialize, Deserialize)]
pub struct Options {
    pub language: String,
    pub require_docs: bool,
}

impl Options {
    fn get_doc<'a>(&self, documentation: &'a Documentation, error_msg: &str) -> &'a str {
        if let Some(text) = documentation.get(&self.language) {
            text
        } else if self.require_docs {
            panic!("{}", error_msg);
        } else {
            "TODO: document"
        }
    }
}

impl Default for Options {
    fn default() -> Self {
        Self {
            language: "en".to_owned(),
            require_docs: false,
        }
    }
}

pub struct Generator {
    parts: Vec<String>,
    options: Options,
}
impl crate::Generator for Generator {
    type Options = Options;
    fn new(_name: &str, _version: &str, options: Options) -> Self {
        Self {
            parts: Vec::new(),
            options,
        }
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
        let language = &self.options.language;
        match schema {
            Schema::Struct(Struct {
                documentation,
                name,
                fields,
                magic: _,
            }) => {
                let mut content = Writer::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(
                        content,
                        "{}",
                        self.options.get_doc(
                            documentation,
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
                            self.options.get_doc(
                                &field.documentation,
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
                        self.options.get_doc(
                            documentation,
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
                            self.options.get_doc(
                                &variant.documentation,
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
                                self.options.get_doc(
                                    &field.documentation,
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
                        self.options.get_doc(
                            documentation,
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
                            self.options.get_doc(
                                &variant.documentation,
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
