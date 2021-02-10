use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
}

fn imports(schema: &Schema) -> String {
    let mut imports = BTreeSet::new();
    fn add_imports_struct(definition: &Struct, imports: &mut BTreeSet<String>) {
        fn add_imports(schema: &Schema, imports: &mut BTreeSet<String>) {
            match schema {
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    imports.insert(file_name(schema));
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
                Schema::Bool
                | Schema::Int32
                | Schema::Int64
                | Schema::Float32
                | Schema::Float64
                | Schema::String => {}
            }
        }
        for field in &definition.fields {
            add_imports(&field.schema, imports);
        }
    }
    match schema {
        Schema::Struct { definition, .. } => {
            add_imports_struct(definition, &mut imports);
        }
        Schema::OneOf { variants, .. } => {
            for variant in variants {
                add_imports_struct(variant, &mut imports);
            }
        }
        _ => {}
    }
    include_templing!("src/gens/php/imports.templing")
}

fn doc_comment(documentation: &Documentation) -> String {
    let mut result = String::new();
    result.push_str("/**\n");
    for line in documentation.get("en").unwrap().lines() {
        result.push_str(" * ");
        result.push_str(line);
        result.push('\n');
    }
    result.push_str(" */\n");
    result.trim().to_owned()
}

fn doc_read_from(name: &str) -> String {
    format!("/**\n * Read {} from input stream\n */", name)
}

fn doc_write_to(name: &str) -> String {
    format!("/**\n * Write {} to output stream\n */", name)
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/php/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/php/write_var.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Schema, usize)>) -> String {
    include_templing!("src/gens/php/struct_impl.templing")
}

fn file_name(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .chain(std::iter::once(schema.name().unwrap()))
        .map(|name| name.camel_case(conv))
        .collect::<Vec<String>>()
        .join("/")
}

fn namespace_path(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .map(|name| name.camel_case(conv))
        .collect::<Vec<String>>()
        .join("\\")
}

fn type_name(schema: &Schema) -> String {
    format!("\\{}", file_name(schema).replace('/', "\\"))
}

impl crate::Generator for Generator {
    const NAME: &'static str = "PHP";
    type Options = ();
    fn new(_name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "Stream.php".to_owned(),
            include_str!("Stream.php").to_owned(),
        );
        Self { files }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        for file in extra_files {
            self.files.insert(file.path, file.content);
        }
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.php", file_name(schema)),
                    include_templing!("src/gens/php/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.files.insert(
                    format!("{}.php", file_name(schema)),
                    include_templing!("src/gens/php/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.php", file_name(schema)),
                    include_templing!("src/gens/php/oneof.templing"),
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
    fn build_local(_path: &Path, _verbose: bool) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command("php");
        command.arg("Main.php").current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Main.php".to_owned(),
            content: include_templing!("src/gens/php/FileReadWrite.php.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "TcpStream.php".to_owned(),
                content: include_str!("TcpStream.php").to_owned(),
            },
            File {
                path: "Main.php".to_owned(),
                content: include_templing!("src/gens/php/TcpReadWrite.php.templing"),
            },
        ]
    }
}
