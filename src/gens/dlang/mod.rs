use super::*;

pub fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
        .replace("Params", "Parameters")
}

#[derive(Default)]
struct Package {
    inner_packages: BTreeSet<String>,
    types: BTreeSet<String>,
}

pub struct Generator {
    packages: HashMap<String, Package>,
    files: HashMap<String, String>,
}

pub fn imports(schema: &Schema) -> String {
    let mut imports = BTreeSet::new();
    fn add_imports_struct(definition: &Struct, imports: &mut BTreeSet<String>) {
        fn add_imports(schema: &Schema, imports: &mut BTreeSet<String>) {
            match schema {
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    imports.insert(module_path(schema));
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
    include_templing!("src/gens/dlang/imports.templing")
}

pub fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "long".to_owned(),
        Schema::Float32 => "float".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Struct {
            namespace,
            definition: Struct { name, .. },
            ..
        }
        | Schema::OneOf {
            namespace,
            base_name: name,
            ..
        }
        | Schema::Enum {
            namespace,
            base_name: name,
            ..
        } => match namespace_path(namespace) {
            Some(path) => format!("{}.{}", path, name.camel_case(conv)),
            None => name.camel_case(conv),
        },
        Schema::Option(inner) => format!("Nullable!({})", type_name(inner)),
        Schema::Vec(inner) => format!("{}[]", type_name(inner)),
        Schema::Map(key, value) => format!("{}[{}]", type_name(value), type_name(key),),
    }
}

pub fn one_of_methods(prefix: &str, schema: &Schema, call: &str) -> String {
    let method_name = |name: &Name| -> Name {
        Name::new(Name::new(prefix.to_owned()).as_str().to_owned() + name.as_str())
    };
    include_templing!("src/gens/dlang/one_of_methods.templing")
}

pub fn default_value(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "false".to_owned(),
        Schema::Int32 | Schema::Int64 => "0".to_owned(),
        Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
        Schema::Map(key, value) => {
            format!("({}[{}]).init", type_name(value), type_name(key))
        }
        Schema::Vec(inner) => format!("new {}[0]", type_name(inner)),
        Schema::Option(..) => "null".to_owned(),
        Schema::String => unimplemented!("No default string"),
        Schema::Struct { definition, .. } => {
            let mut result = format!("{}(", type_name(schema));
            for (index, field) in definition.fields.iter().enumerate() {
                if index != 0 {
                    result.push_str(", ");
                }
                result.push_str(&default_value(&field.schema));
            }
            result.push(')');
            result
        }
        Schema::Enum { .. } => unimplemented!("Can't determine default enum variant"),
        Schema::OneOf { .. } => unimplemented!("Can't determine default OneOf variant"),
    }
}

fn doc_comment(documentation: &Documentation) -> String {
    let mut result = String::new();
    for line in documentation.get("en").unwrap().lines() {
        result.push_str("/// ");
        result.push_str(line);
        result.push('\n');
    }
    result.trim().to_owned()
}

fn doc_read_from(name: &str) -> String {
    format!("/// Read {} from reader", name)
}

fn doc_write_to(name: &str) -> String {
    format!("/// Write {} to writer", name)
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/dlang/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/dlang/write_var.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/dlang/struct_impl.templing")
}

fn namespace_path(namespace: &Namespace) -> Option<String> {
    if namespace.parts.is_empty() {
        None
    } else {
        Some(
            namespace
                .parts
                .iter()
                .map(|name| name.snake_case(conv))
                .collect::<Vec<_>>()
                .join("."),
        )
    }
}

fn file_name(schema: &Schema) -> String {
    match schema {
        Schema::Enum {
            namespace,
            base_name: name,
            ..
        }
        | Schema::Struct {
            namespace,
            definition: Struct { name, .. },
            ..
        }
        | Schema::OneOf {
            namespace,
            base_name: name,
            ..
        } => match namespace_path(namespace) {
            Some(path) => format!("{}/{}", path.replace('.', "/"), name.snake_case(conv)),
            None => name.snake_case(conv),
        },
        _ => unreachable!(),
    }
}

pub fn module_path(schema: &Schema) -> String {
    file_name(schema).replace('/', ".")
}

impl Generator {
    fn insert_package(&mut self, namespace: &Namespace, schema: &Schema) {
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
                .insert(module_path(schema));
        }
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "D";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let name = Name::new(name.to_owned());
        let mut files = HashMap::new();
        files.insert(
            "source/stream.d".to_owned(),
            include_str!("stream.d").to_owned(),
        );
        files.insert(
            "source/buffered_stream.d".to_owned(),
            include_str!("buffered_stream.d").to_owned(),
        );
        files.insert(
            "dub.json".to_owned(),
            include_templing!("src/gens/dlang/dub.json.templing"),
        );
        Self {
            packages: HashMap::new(),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        for (package_name, package) in self.packages {
            self.files.insert(
                format!("source/{}/package.d", package_name.replace('.', "/")),
                include_templing!("src/gens/dlang/package.d.templing"),
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
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.insert_package(namespace, schema);
                self.files.insert(
                    format!("source/{}.d", file_name(schema)),
                    include_templing!("src/gens/dlang/enum.templing"),
                );
            }
            Schema::Struct {
                namespace,
                definition,
            } => {
                self.insert_package(namespace, schema);
                self.files.insert(
                    format!("source/{}.d", file_name(schema)),
                    include_templing!("src/gens/dlang/struct.templing"),
                );
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.insert_package(namespace, schema);
                self.files.insert(
                    format!("source/{}.d", file_name(schema)),
                    include_templing!("src/gens/dlang/oneof.templing"),
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
    fn build_local(path: &Path, verbose: bool) -> anyhow::Result<()> {
        command("dub")
            .arg("build")
            .arg("-b")
            .arg("release")
            .current_dir(path)
            .show_output(verbose)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let project_name = serde_json::from_str::<serde_json::Value>(
            &std::fs::read_to_string(path.join("dub.json")).context("Failed to read dub.json")?,
        )
        .context("Failed to parse dub.json")?
        .get("name")
        .ok_or_else(|| anyhow!("No name in dub.json"))?
        .as_str()
        .ok_or_else(|| anyhow!("Name is not string in dub.json"))?
        .to_owned();
        let mut command = command(
            path.join(format!(
                "{}{}",
                project_name,
                if cfg!(windows) { ".exe" } else { "" }
            ))
            .to_str()
            .unwrap(),
        );
        command.current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "source/app.d".to_owned(),
            content: include_templing!("src/gens/dlang/file_read_write.d.templing"),
        }]
    }
}

pub fn socket_stream_source() -> &'static str {
    include_str!("socket_stream.d")
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "source/socket_stream.d".to_owned(),
                content: socket_stream_source().to_owned(),
            },
            File {
                path: "source/app.d".to_owned(),
                content: include_templing!("src/gens/dlang/tcp_read_write.d.templing"),
            },
        ]
    }
}
