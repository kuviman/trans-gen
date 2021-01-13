use super::*;

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
    crate_name: String,
    crate_version: String,
    types: HashMap<String, String>,
}
impl crate::Generator for Generator {
    type Options = ();
    fn new(name: &str, version: &str, _: ()) -> Self {
        Self {
            crate_name: name.to_owned(),
            crate_version: version.to_owned(),
            types: HashMap::new(),
        }
    }
    fn result(self) -> GenResult {
        let mut files = HashMap::new();
        let types = self.types.keys();
        let Self {
            crate_name,
            crate_version,
            ..
        } = self;
        files.insert(
            "Cargo.toml".to_owned(),
            include_templing!("src/gens/rust/Cargo.toml.templing"),
        );
        files.insert(
            "src/lib.rs".to_owned(),
            include_templing!("src/gens/rust/lib.rs.templing"),
        );
        for (name, content) in self.types {
            files.insert(format!("src/{}.rs", name), content);
        }
        files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Struct(Struct {
                documentation: _,
                name,
                fields,
                magic,
            }) => {
                self.types.insert(
                    name.snake_case(conv),
                    include_templing!("src/gens/rust/struct.templing"),
                );
            }
            Schema::OneOf {
                base_name,
                variants,
                documentation: _,
            } => {
                self.types.insert(
                    base_name.snake_case(conv),
                    include_templing!("src/gens/rust/oneof.templing"),
                );
            }
            Schema::Enum {
                base_name,
                variants,
                documentation: _,
            } => {
                self.types.insert(
                    base_name.snake_case(conv),
                    include_templing!("src/gens/rust/enum.templing"),
                );
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
