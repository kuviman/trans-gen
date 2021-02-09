use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

#[derive(Default)]
struct Package {
    inner_packages: BTreeSet<String>,
    types: BTreeSet<Name>,
}

pub struct Generator {
    packages: HashMap<String, Package>,
    files: HashMap<String, String>,
}

fn imports(schema: &Schema) -> String {
    let mut imports = BTreeSet::new();
    fn add_imports_struct(definition: &Struct, imports: &mut BTreeSet<String>) {
        fn add_imports(schema: &Schema, imports: &mut BTreeSet<String>) {
            match schema {
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    imports.insert(format!(
                        "from {} import {}",
                        file_name(schema).replace('/', "."),
                        schema.name().unwrap().camel_case(conv),
                    ));
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
    imports.into_iter().collect::<Vec<String>>().join("\n")
}

fn doc_comment(documentation: &Documentation) -> String {
    let mut result = String::new();
    result.push_str("\"\"\"");
    for (index, line) in documentation.get("en").unwrap().lines().enumerate() {
        if index == 1 {
            result.push('\n')
        }
        result.push_str(line);
        if index != 0 {
            result.push('\n');
        }
    }
    result.push_str("\"\"\"\n");
    result.trim().to_owned()
}

fn doc_read_from(name: &str) -> String {
    format!("\"\"\"Read {} from input stream\n\"\"\"", name)
}

fn doc_write_to(name: &str) -> String {
    format!("\"\"\"Write {} to output stream\n\"\"\"", name)
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/python/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/python/write_var.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/python/struct_impl.templing")
}

fn file_name(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .chain(std::iter::once(schema.name().unwrap()))
        .map(|name| name.snake_case(conv))
        .collect::<Vec<String>>()
        .join("/")
}

impl Generator {
    fn insert_package(&mut self, schema: &Schema) {
        let namespace = schema.namespace().unwrap();
        let mut parent: Option<String> = None;
        for part in &namespace.parts {
            let package = match &parent {
                Some(parent) => {
                    format!("{}.{}", parent, part.snake_case(conv))
                }
                None => part.snake_case(conv),
            };
            if let Some(parent) = parent {
                self.packages
                    .entry(parent)
                    .or_default()
                    .inner_packages
                    .insert(package.clone());
            }
            parent = Some(package);
        }
        if let Some(package) = parent {
            self.packages
                .entry(package)
                .or_default()
                .types
                .insert(schema.name().unwrap().clone());
        }
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Python";
    type Options = ();
    fn new(_name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "stream_wrapper.py".to_owned(),
            include_str!("stream_wrapper.py").to_owned(),
        );
        Self {
            packages: HashMap::new(),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        for (package_name, package) in self.packages {
            self.files.insert(
                format!("{}/__init__.py", package_name.replace('.', "/")),
                include_templing!("src/gens/python/__init__.py.templing"),
            );
        }
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
                self.insert_package(schema);
                self.files.insert(
                    format!("{}.py", file_name(schema)),
                    include_templing!("src/gens/python/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.insert_package(schema);
                self.files.insert(
                    format!("{}.py", file_name(schema)),
                    include_templing!("src/gens/python/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.insert_package(schema);
                self.files.insert(
                    format!("{}.py", file_name(schema)),
                    include_templing!("src/gens/python/oneof.templing"),
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
        let mut command = command(if cfg!(windows) { "py -3" } else { "python3" });
        command.arg("main.py").current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "main.py".to_owned(),
            content: include_templing!("src/gens/python/file_read_write.py.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "main.py".to_owned(),
            content: include_templing!("src/gens/python/tcp_read_write.py.templing"),
        }]
    }
}
