use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    model_init: String,
    files: HashMap<String, String>,
}

fn imports(schema: &Schema) -> String {
    let mut imports = BTreeSet::new();
    fn add_imports_struct(struc: &Struct, imports: &mut BTreeSet<Name>) {
        fn add_for_field(schema: &Schema, imports: &mut BTreeSet<Name>) {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                }
                | Schema::Enum {
                    base_name: name, ..
                } => {
                    imports.insert(name.clone());
                }
                Schema::Option(inner) => {
                    add_for_field(inner, imports);
                }
                Schema::Vec(inner) => {
                    add_for_field(inner, imports);
                }
                Schema::Map(key_type, value_type) => {
                    add_for_field(key_type, imports);
                    add_for_field(value_type, imports);
                }
                Schema::Bool
                | Schema::Int32
                | Schema::Int64
                | Schema::Float32
                | Schema::Float64
                | Schema::String => {}
            }
        }
        for field in &struc.fields {
            add_for_field(&field.schema, imports);
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
    include_templing!("src/gens/ruby/imports.templing")
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/ruby/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/ruby/write_var.templing")
}

fn var_to_string(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/ruby/var_to_string.templing")
}

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/ruby/struct_impl.templing")
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Ruby";
    type Options = ();
    fn new(_name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "stream_wrapper.rb".to_owned(),
            include_str!("stream_wrapper.rb").to_owned(),
        );
        Self {
            model_init: String::new(),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        if !self.model_init.is_empty() {
            self.files.insert("model.rb".to_owned(), self.model_init);
        }
        for file in extra_files {
            self.files.insert(file.path, file.content);
        }
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                documentation: _,
                base_name,
                variants,
            } => {
                writeln!(
                    &mut self.model_init,
                    "require_relative 'model/{}'",
                    base_name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(
                    format!("model/{}.rb", base_name.snake_case(conv)),
                    include_templing!("src/gens/ruby/enum.templing"),
                );
            }
            Schema::Struct(struc) => {
                writeln!(
                    &mut self.model_init,
                    "require_relative 'model/{}'",
                    struc.name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(
                    format!("model/{}.rb", struc.name.snake_case(conv)),
                    include_templing!("src/gens/ruby/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                writeln!(
                    &mut self.model_init,
                    "require_relative 'model/{}'",
                    base_name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(
                    format!("model/{}.rb", base_name.snake_case(conv)),
                    include_templing!("src/gens/ruby/oneof.templing"),
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

impl RunnableGenerator for Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command("ruby");
        command.arg("main.rb").current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(_: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>();
        let schema: &Schema = &schema;
        fn type_name(schema: &Schema) -> String {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => name.camel_case(conv),
                _ => unreachable!(),
            }
        }
        vec![File {
            path: "main.rb".to_owned(),
            content: include_templing!("src/gens/ruby/file_read_write.rb.templing"),
        }]
    }
}
