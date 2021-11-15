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

pub fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "boolean".to_owned(),
        Schema::Int32 => "number".to_owned(),
        Schema::Int64 => "bigint".to_owned(),
        Schema::Float32 => "number".to_owned(),
        Schema::Float64 => "number".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Option(inner) => format!("{} | null", type_name(inner)),
        Schema::Struct {
            definition: Struct { name, .. },
            ..
        } => name.camel_case(conv),
        Schema::OneOf { base_name, .. } => base_name.camel_case(conv),
        Schema::Vec(inner) => format!("Array<{}>", type_name(inner)),
        Schema::Map(key_type, value_type) => {
            format!("Map<{}, {}>", type_name(key_type), type_name(value_type))
        }
        Schema::Enum { base_name, .. } => base_name.camel_case(conv),
    }
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
    include_templing!("src/gens/typescript/imports.templing")
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
    include_templing!("src/gens/typescript/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/typescript/write_var.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/typescript/struct_impl.templing")
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
                    format!("src/{}.ts", file_name(schema)),
                    include_templing!("src/gens/typescript/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.files.insert(
                    format!("src/{}.ts", file_name(schema)),
                    include_templing!("src/gens/typescript/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("src/{}.ts", file_name(schema)),
                    include_templing!("src/gens/typescript/oneof.templing"),
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
    const NAME: &'static str = "TypeScript";
    type Options = ();
    fn new(name: &str, version: &str, _: ()) -> Self {
        let project_name = Name::new(name.to_owned()).kebab_case(conv);
        let project_version = version;
        let mut files = HashMap::new();
        files.insert(
            "src/stream.ts".to_owned(),
            include_str!("stream.ts").to_owned(),
        );
        files.insert(
            "tsconfig.json".to_owned(),
            include_str!("tsconfig.json").to_owned(),
        );
        files.insert(
            "package.json".to_owned(),
            include_templing!("src/gens/typescript/package.json.templing"),
        );
        Self { files }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        for file in extra_files {
            self.files.insert(file.path, file.content);
        }
        // Replace absolute imports with relative
        for (source_path, content) in &mut self.files {
            *content = content
                .lines()
                .map(|line| {
                    if line.starts_with("import") {
                        let end = line.rfind('"').unwrap();
                        let start = line[..end].rfind('"').unwrap();
                        let path = &line[start + 1..end];
                        if let Some(path) = path.strip_prefix('@') {
                            let path = format!("src/{}", path);
                            let source_path = source_path.split('/').collect::<Vec<&str>>();
                            let path = path.split('/').collect::<Vec<&str>>();
                            let mut source_path = &source_path[..source_path.len() - 1];
                            let mut path = &path[..];
                            while !source_path.is_empty() && source_path[0] == path[0] {
                                source_path = &source_path[1..];
                                path = &path[1..];
                            }
                            let mut path = source_path
                                .iter()
                                .map(|_| "..")
                                .chain(path.iter().copied())
                                .collect::<Vec<&str>>()
                                .join("/");
                            if !path.starts_with("..") {
                                path = format!("./{}", path);
                            }
                            return format!("{}\"{}\"{}", &line[..start], path, &line[end + 1..]);
                        }
                    }
                    line.to_owned()
                })
                .collect::<Vec<String>>()
                .join("\n")
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
            .run()?;
        command("npm")
            .arg("run")
            .arg("build")
            .current_dir(path)
            .show_output(verbose)
            .run()?;
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command("node");
        command.arg("main.js").current_dir(path.join("build"));
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "src/main.ts".to_owned(),
            content: include_templing!("src/gens/typescript/file-read-write.ts.templing"),
        }]
    }
}

pub fn tcp_stream_source() -> &'static str {
    include_str!("tcp-stream.ts")
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "src/tcp-stream.ts".to_owned(),
                content: tcp_stream_source().to_owned(),
            },
            File {
                path: "src/main.ts".to_owned(),
                content: include_templing!("src/gens/typescript/tcp-read-write.ts.templing"),
            },
        ]
    }
}
