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
        fn add_for_field(schema: &Schema, imports: &mut BTreeSet<String>) {
            match schema {
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    imports.insert(file_name(schema));
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
        for field in &definition.fields {
            add_for_field(&field.schema, imports);
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
    include_templing!("src/gens/ruby/imports.templing")
}

fn doc_comment(documentation: &Documentation) -> String {
    let mut result = String::new();
    for line in documentation.get("en").unwrap().lines() {
        result.push_str("# ");
        result.push_str(line);
        result.push('\n');
    }
    result.trim().to_owned()
}

fn doc_read_from(name: &str) -> String {
    format!("# Read {} from input stream", name)
}

fn doc_write_to(name: &str) -> String {
    format!("# Write {} to output stream", name)
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

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/ruby/struct_impl.templing")
}

pub fn file_name(schema: &Schema) -> String {
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

pub fn default_value(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "false".to_owned(),
        Schema::Int32 | Schema::Int64 => "0".to_owned(),
        Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
        Schema::Map(..) => "Hash.new".to_owned(),
        Schema::Vec(..) => "[]".to_owned(),
        Schema::Option(..) => "nil".to_owned(),
        Schema::String => unimplemented!("No default string"),
        Schema::Struct { definition, .. } => {
            let mut result = type_name(schema);
            result.push_str(".new(");
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

pub fn type_name(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .chain(std::iter::once(schema.name().unwrap()))
        .map(|name| name.camel_case(conv))
        .collect::<Vec<String>>()
        .join("::")
}

fn modules_pre(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .map(|name| format!("module {}", name.camel_case(conv)))
        .collect::<Vec<String>>()
        .join("\n")
}

fn modules_end(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .map(|_| "end".to_owned())
        .collect::<Vec<String>>()
        .join("\n")
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
                    .insert(part.snake_case(conv));
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
    const NAME: &'static str = "Ruby";
    type Options = ();
    fn new(_name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert("stream.rb".to_owned(), include_str!("stream.rb").to_owned());
        Self {
            packages: HashMap::new(),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        for (package_name, package) in self.packages {
            self.files.insert(
                format!("{}.rb", package_name.replace('.', "/")),
                include_templing!("src/gens/ruby/package.rb.templing"),
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
                    format!("{}.rb", file_name(schema)),
                    include_templing!("src/gens/ruby/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.insert_package(schema);
                self.files.insert(
                    format!("{}.rb", file_name(schema)),
                    include_templing!("src/gens/ruby/struct.templing"),
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
                    format!("{}.rb", file_name(schema)),
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
    fn build_local(_path: &Path, _verbose: bool) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command("ruby");
        command.arg("main.rb").current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "main.rb".to_owned(),
            content: include_templing!("src/gens/ruby/file_read_write.rb.templing"),
        }]
    }
}

pub fn tcp_stream_source() -> &'static str {
    include_str!("tcp_stream.rb")
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "tcp_stream.rb".to_owned(),
                content: tcp_stream_source().to_owned(),
            },
            File {
                path: "main.rb".to_owned(),
                content: include_templing!("src/gens/ruby/tcp_read_write.rb.templing"),
            },
        ]
    }
}
