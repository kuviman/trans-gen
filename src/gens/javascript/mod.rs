use super::*;

pub fn conv(name: &str) -> String {
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
    fn add_imports_struct(definition: &Struct, imports: &mut BTreeSet<(Name, String)>) {
        fn add_imports(schema: &Schema, imports: &mut BTreeSet<(Name, String)>) {
            match schema {
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    imports.insert((schema.name().unwrap().clone(), file_name(schema)));
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
    include_templing!("src/gens/javascript/imports.templing")
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
    include_templing!("src/gens/javascript/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/javascript/write_var.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/javascript/struct_impl.templing")
}

pub fn file_name(schema: &Schema) -> String {
    let mut result = String::new();
    for part in schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .chain(std::iter::once(schema.name().unwrap()))
    {
        if !result.is_empty() {
            result.push('/');
        }
        result.push_str(&part.kebab_case(conv));
    }
    result
}

pub fn one_of_methods(prefix: &str, schema: &Schema, call: &str) -> String {
    let method_name = |name: &Name| -> Name {
        Name::new(Name::new(prefix.to_owned()).as_str().to_owned() + name.as_str())
    };
    include_templing!("src/gens/javascript/one_of_methods.templing")
}

pub fn default_value(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "false".to_owned(),
        Schema::Int32 | Schema::Int64 => "0".to_owned(),
        Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
        Schema::Map(..) => "new Map()".to_owned(),
        Schema::Vec(..) => "[]".to_owned(),
        Schema::Option(..) => "null".to_owned(),
        Schema::String => unimplemented!("No default string"),
        Schema::Struct { definition, .. } => {
            let mut result = format!("new {}(", definition.name.camel_case(conv));
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

impl Generator {
    fn add_only(&mut self, schema: &Schema) -> anyhow::Result<()> {
        match schema {
            Schema::Enum {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.js", file_name(schema)),
                    include_templing!("src/gens/javascript/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.files.insert(
                    format!("{}.js", file_name(schema)),
                    include_templing!("src/gens/javascript/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.js", file_name(schema)),
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
    const NAME: &'static str = "JavaScript";
    type Options = ();
    fn new(name: &str, version: &str, _: ()) -> Self {
        let project_name = Name::new(name.to_owned()).kebab_case(conv);
        let project_version = version;
        let mut files = HashMap::new();
        files.insert("stream.js".to_owned(), include_str!("stream.js").to_owned());
        files.insert(
            "package.json".to_owned(),
            include_templing!("src/gens/javascript/package.json.templing"),
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
        self.add_only(schema).unwrap();
    }
}

impl RunnableGenerator for Generator {
    fn build_local(path: &Path, verbose: bool) -> anyhow::Result<()> {
        command("npm")
            .arg("install")
            .current_dir(path)
            .show_output(verbose)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command("node");
        command.arg("main.js").current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "main.js".to_owned(),
            content: include_templing!("src/gens/javascript/file-read-write.js.templing"),
        }]
    }
}

pub fn tcp_stream_source() -> &'static str {
    include_str!("tcp-stream.js")
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "tcp-stream.js".to_owned(),
                content: tcp_stream_source().to_owned(),
            },
            File {
                path: "main.js".to_owned(),
                content: include_templing!("src/gens/javascript/tcp-read-write.js.templing"),
            },
        ]
    }
}
