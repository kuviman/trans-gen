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
        match schema {
            Schema::Struct(Struct {
                name,
                fields,
                magic,
            }) => {
                let mut content = String::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(content, "  Fields:").unwrap();
                    writeln!(content).unwrap();
                    for field in fields {
                        writeln!(
                            content,
                            "- `{}`: `{}`",
                            field.name.snake_case(conv),
                            type_name(&field.schema)
                        )
                        .unwrap();
                    }
                }
                self.parts.push(content);
            }
            Schema::OneOf {
                base_name,
                variants,
            } => {
                let mut content = String::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", base_name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(content, "One of:").unwrap();
                    for variant in variants {
                        writeln!(content, "- `{}`:", variant.name.camel_case(conv)).unwrap();
                        writeln!(content).unwrap();
                        if variant.fields.is_empty() {
                            writeln!(content, "  No fields:").unwrap();
                        } else {
                            writeln!(content, "  Fields:").unwrap();
                        }
                        writeln!(content).unwrap();
                        for field in &variant.fields {
                            writeln!(
                                content,
                                "  - `{}`: `{}`",
                                field.name.snake_case(conv),
                                type_name(&field.schema)
                            )
                            .unwrap();
                        }
                    }
                }
                self.parts.push(content);
            }
            Schema::Enum {
                base_name,
                variants,
            } => {
                let mut content = String::new();
                {
                    let content = &mut content;
                    writeln!(content, "## `{}`", base_name.camel_case(conv)).unwrap();
                    writeln!(content).unwrap();
                    writeln!(content, "Variants:").unwrap();
                    writeln!(content).unwrap();
                    for variant in variants {
                        writeln!(content, "- `{}`", variant.camel_case(conv)).unwrap();
                    }
                }
                self.parts.push(content);
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
