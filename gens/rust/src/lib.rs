#[macro_use]
extern crate include_dir;

use std::collections::HashMap;
use std::fmt::Write;
use trans_gen_core::trans_schema::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "I32")
        .replace("Int64", "I64")
        .replace("Float32", "F32")
        .replace("Float64", "F64")
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "i32".to_owned(),
        Schema::Int64 => "i64".to_owned(),
        Schema::Float32 => "f32".to_owned(),
        Schema::Float64 => "f64".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Option(inner) => format!("Option<{}>", type_name(inner)),
        Schema::Struct(Struct { name, .. }) => name.camel_case(conv),
        Schema::OneOf { base_name, .. } => base_name.camel_case(conv),
        Schema::Vec(inner) => format!("Vec<{}>", type_name(inner)),
        Schema::Map(key_type, value_type) => format!(
            "std::collections::HashMap<{}, {}>",
            type_name(key_type),
            type_name(value_type)
        ),
        Schema::Enum { base_name, .. } => base_name.camel_case(conv),
    }
}

pub struct Generator {
    files: HashMap<String, String>,
}
impl trans_gen_core::Generator for Generator {
    fn new(name: &str, version: &str) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "Cargo.toml".to_owned(),
            include_str!("../template/Cargo.toml.template")
                .replace("$name", name)
                .replace("$version", version),
        );
        files.insert("src/lib.rs".to_owned(), String::new());
        Self { files }
    }
    fn result(self) -> HashMap<String, String> {
        self.files
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Struct(Struct {
                name,
                fields,
                magic,
            }) => {
                let file_name = format!("src/{}.rs", name.snake_case(conv));
                let mut content = String::new();
                {
                    let content = &mut content;

                    writeln!(content, "use crate::*;").unwrap();

                    write!(content, "#[derive(Clone, Debug").unwrap();
                    if schema.hashable() {
                        write!(content, ", PartialEq, Eq, Hash").unwrap();
                    }
                    writeln!(content, ", trans::Trans)]").unwrap();
                    if let Some(magic) = magic {
                        writeln!(content, "#[trans(magic = \"{}\")]", magic).unwrap();
                    }
                    writeln!(content, "pub struct {} {{", name.camel_case(conv)).unwrap();
                    for field in fields {
                        writeln!(
                            content,
                            "    pub {}: {},",
                            field.name.snake_case(conv),
                            type_name(&field.schema)
                        )
                        .unwrap();
                    }
                    writeln!(content, "}}").unwrap();
                }

                let lib = self.files.get_mut("src/lib.rs").unwrap();
                writeln!(lib).unwrap();
                writeln!(lib, "mod {};", name.snake_case(conv)).unwrap();
                writeln!(lib, "pub use self::{}::*;", name.snake_case(conv)).unwrap();

                self.files.insert(file_name, content);
            }
            Schema::OneOf {
                base_name,
                variants,
            } => {
                let file_name = format!("src/{}.rs", base_name.snake_case(conv));
                let mut content = String::new();
                {
                    let content = &mut content;

                    writeln!(content, "use crate::*;").unwrap();

                    writeln!(content, "#[derive(Clone, Debug, trans::Trans)]").unwrap();
                    writeln!(content, "pub enum {} {{", base_name.camel_case(conv)).unwrap();
                    for variant in variants {
                        writeln!(content, "    {} {{", variant.name.camel_case(conv)).unwrap();
                        for field in &variant.fields {
                            writeln!(
                                content,
                                "        {}: {},",
                                field.name.snake_case(conv),
                                type_name(&field.schema)
                            )
                            .unwrap();
                        }
                        writeln!(content, "    }},").unwrap();
                    }
                    writeln!(content, "}}").unwrap();
                }

                let lib = self.files.get_mut("src/lib.rs").unwrap();
                writeln!(lib).unwrap();
                writeln!(lib, "mod {};", base_name.snake_case(conv)).unwrap();
                writeln!(lib, "pub use self::{}::*;", base_name.snake_case(conv)).unwrap();

                self.files.insert(file_name, content);
            }
            Schema::Enum {
                base_name,
                variants,
            } => {
                let file_name = format!("src/{}.rs", base_name.snake_case(conv));
                let mut content = String::new();
                {
                    let content = &mut content;

                    writeln!(content, "use crate::*;").unwrap();

                    writeln!(
                        content,
                        "#[derive(Clone, Debug, PartialEq, Eq, Hash, trans::Trans)]"
                    )
                    .unwrap();
                    writeln!(content, "pub enum {} {{", base_name.camel_case(conv)).unwrap();
                    for variant in variants {
                        writeln!(content, "    {},", variant.camel_case(conv)).unwrap();
                    }
                    writeln!(content, "}}").unwrap();
                }

                let lib = self.files.get_mut("src/lib.rs").unwrap();
                writeln!(lib).unwrap();
                writeln!(lib, "mod {};", base_name.snake_case(conv)).unwrap();
                writeln!(lib, "pub use self::{}::*;", base_name.snake_case(conv)).unwrap();

                self.files.insert(file_name, content);
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
