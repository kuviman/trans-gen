use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
    index_file: String,
}

fn imports(schema: &Schema) -> String {
    let mut imports = HashSet::new();
    fn add_imports_struct(struc: &Struct, imports: &mut HashSet<Name>) {
        fn add_imports(schema: &Schema, imports: &mut HashSet<Name>) {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => {
                    imports.insert(name.clone());
                }
                Schema::Option(inner) => {
                    add_imports(inner, imports);
                }
                Schema::Vec(inner) => {
                    add_imports(inner, imports);
                }
                Schema::Map(key_type, value_type) => {
                    add_imports(key_type, imports);
                    add_imports(value_type, imports);
                }
                Schema::Enum { .. } => {}
                Schema::Bool
                | Schema::Int32
                | Schema::Int64
                | Schema::Float32
                | Schema::Float64
                | Schema::String => {}
            }
        }
        for field in &struc.fields {
            add_imports(&field.schema, imports);
        }
    }
    match schema {
        Schema::Struct(struc) => {
            add_imports_struct(struc, &mut imports);
        }
        Schema::OneOf { variants, .. } => {
            for variant in variants {
                add_imports_struct(variant, &mut imports);
            }
        }
        _ => {}
    }
    include_templing!("src/gens/javascript/imports.templing")
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/javascript/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/javascript/write_var.templing")
}

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/javascript/struct_impl.templing")
}

fn file_name(name: &Name) -> String {
    name.snake_case(conv).replace('_', "-")
}

impl Generator {
    fn add_only(&mut self, schema: &Schema) -> anyhow::Result<()> {
        match schema {
            Schema::Enum {
                documentation: _,
                base_name,
                variants,
            } => {
                writeln!(
                    self.index_file,
                    "module.exports.{} = require('./{}');",
                    base_name.camel_case(conv),
                    file_name(base_name),
                )
                .unwrap();
                self.files.insert(
                    format!("model/{}.js", file_name(base_name)),
                    include_templing!("src/gens/javascript/enum.templing"),
                );
            }
            Schema::Struct(struc) => {
                writeln!(
                    self.index_file,
                    "module.exports.{} = require('./{}');",
                    struc.name.camel_case(conv),
                    file_name(&struc.name),
                )
                .unwrap();
                self.files.insert(
                    format!("model/{}.js", file_name(&struc.name)),
                    include_templing!("src/gens/javascript/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                writeln!(
                    self.index_file,
                    "module.exports.{} = require('./{}');",
                    base_name.camel_case(conv),
                    file_name(base_name),
                )
                .unwrap();
                self.files.insert(
                    format!("model/{}.js", file_name(base_name)),
                    include_templing!("src/gens/javascript/oneof.templing"),
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
        Ok(())
    }
}

impl crate::Generator for Generator {
    type Options = ();
    fn new(_name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "stream-wrapper.js".to_owned(),
            include_str!("stream-wrapper.js").to_owned(),
        );
        Self {
            files,
            index_file: String::new(),
        }
    }
    fn result(mut self) -> GenResult {
        self.files
            .insert("model/index.js".to_owned(), self.index_file);
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        self.add_only(schema).unwrap();
    }
}
