use super::*;

fn conv(name: &str) -> String {
    name.to_owned()
}

pub struct Generator {
    mod_name: String,
    files: HashMap<String, String>,
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int32".to_owned(),
        Schema::Int64 => "int64".to_owned(),
        Schema::Float32 => "float32".to_owned(),
        Schema::Float64 => "float64".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Struct {
            definition: Struct { name, .. },
            ..
        }
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => name.camel_case(conv),
        Schema::Option(inner) => format!("*{}", type_name(inner)),
        Schema::Vec(inner) => format!("[]{}", type_name(inner)),
        Schema::Map(key, value) => format!("map[{}]{}", type_name(key), type_name(value),),
    }
}

fn needs_stream(schema: &Schema) -> bool {
    fn needs_stream_inner(schema: &Schema) -> bool {
        match schema {
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String
            | Schema::Enum { .. }
            | Schema::Vec(_)
            | Schema::Map(_, _)
            | Schema::Option(_) => true,
            Schema::Struct { .. } => false,
            Schema::OneOf { .. } => false,
        }
    }
    fn struct_need_stream(definition: &Struct) -> bool {
        definition
            .fields
            .iter()
            .any(|field| needs_stream_inner(&field.schema))
    }
    match schema {
        Schema::Bool
        | Schema::Int32
        | Schema::Int64
        | Schema::Float32
        | Schema::Float64
        | Schema::String
        | Schema::Enum { .. }
        | Schema::Vec(_)
        | Schema::Map(_, _)
        | Schema::Option(_) => true,
        Schema::Struct { definition, .. } => struct_need_stream(definition),
        Schema::OneOf { .. } => true,
    }
}

fn doc_comment(documentation: &Documentation) -> String {
    let mut result = String::new();
    for line in documentation.get("en").unwrap().lines() {
        result.push_str("// ");
        result.push_str(line);
        result.push('\n');
    }
    result.trim().to_owned()
}

fn doc_read_from(name: &str) -> String {
    format!("// Read {} from reader", name)
}

fn doc_write_to(name: &str) -> String {
    format!("// Write {} to writer", name)
}

fn doc_to_string(name: &str) -> String {
    format!("// Get string representation of {}", name)
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/go/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/go/write_var.templing")
}

fn var_to_string(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/go/var_to_string.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/go/struct_impl.templing")
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
                .join("/"),
        )
    }
}

fn namespace_path_for(schema: &Schema) -> String {
    match schema {
        Schema::Struct { namespace, .. }
        | Schema::OneOf { namespace, .. }
        | Schema::Enum { namespace, .. } => {
            namespace_path(namespace).unwrap_or("common".to_owned())
        }
        _ => unreachable!(),
    }
}

fn package_name(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .last()
        .map(|name| name.snake_case(conv))
        .unwrap_or("common".to_owned())
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
        } => format!(
            "{}/{}",
            namespace_path(namespace).unwrap_or("common".to_owned()),
            name.snake_case(conv),
        ),
        _ => unreachable!(),
    }
}

impl Generator {
    fn imports(&self, schema: &Schema) -> String {
        let mut imports: BTreeSet<String> = BTreeSet::new();
        fn add_imports_struct(mod_name: &str, definition: &Struct, imports: &mut BTreeSet<String>) {
            fn add_imports(mod_name: &str, schema: &Schema, imports: &mut BTreeSet<String>) {
                match schema {
                    Schema::Struct { namespace, .. }
                    | Schema::OneOf { namespace, .. }
                    | Schema::Enum { namespace, .. } => {
                        imports.insert(format!(
                            ". \"{}/{}\"",
                            mod_name,
                            namespace_path(namespace).unwrap_or("common".to_owned()),
                        ));
                    }
                    Schema::Option(inner) => {
                        add_imports(mod_name, inner, imports);
                    }
                    Schema::Vec(inner) => {
                        add_imports(mod_name, inner, imports);
                    }
                    Schema::Map(key_type, value_type) => {
                        add_imports(mod_name, key_type, imports);
                        add_imports(mod_name, value_type, imports);
                    }
                    Schema::Bool
                    | Schema::Int32
                    | Schema::Int64
                    | Schema::Float32
                    | Schema::Float64 => {
                        imports.insert("\"fmt\"".to_owned());
                    }
                    Schema::String => {}
                }
            }
            for field in &definition.fields {
                add_imports(mod_name, &field.schema, imports);
            }
        }
        match schema {
            Schema::Struct { definition, .. } => {
                add_imports_struct(&self.mod_name, definition, &mut imports);
            }
            Schema::OneOf { variants, .. } => {
                for variant in variants {
                    add_imports_struct(&self.mod_name, variant, &mut imports);
                }
            }
            _ => {}
        }
        match schema {
            Schema::Struct { namespace, .. }
            | Schema::OneOf { namespace, .. }
            | Schema::Enum { namespace, .. } => {
                imports.remove(&format!(
                    ". \"{}/{}\"",
                    &self.mod_name,
                    namespace_path(namespace).unwrap_or("common".to_owned()),
                ));
            }
            _ => unreachable!(),
        }
        imports.insert("\"io\"".to_owned());
        if needs_stream(schema) {
            imports.insert(format!(". \"{}/stream\"", self.mod_name));
        }
        imports
            .into_iter()
            .map(|import| format!("import {}", import))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Go";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let name = Name::new(name.to_owned());
        let mut files = HashMap::new();
        files.insert(
            "stream/stream.go".to_owned(),
            include_str!("stream.go").to_owned(),
        );
        files.insert(
            "go.mod".to_owned(),
            include_templing!("src/gens/go/go.mod.templing"),
        );
        Self {
            mod_name: name.snake_case(conv),
            files,
        }
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
                    format!("{}.go", file_name(schema)),
                    include_templing!("src/gens/go/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.files.insert(
                    format!("{}.go", file_name(schema)),
                    include_templing!("src/gens/go/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.go", file_name(schema)),
                    include_templing!("src/gens/go/oneof.templing"),
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

fn project_name(path: &Path) -> anyhow::Result<String> {
    Ok(std::fs::read_to_string(path.join("go.mod"))
        .context("Failed to read go.mod")?
        .lines()
        .next()
        .ok_or(anyhow!("go.mod is empty"))?
        .strip_prefix("module ")
        .ok_or(anyhow!("Expected to see module name in go.mod"))?
        .trim()
        .to_owned())
}

impl RunnableGenerator for Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("go")
            .arg("build")
            .arg("-o")
            .arg(format!(
                "{}{}",
                project_name(path)?,
                if cfg!(windows) { ".exe" } else { "" }
            ))
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command(
            path.join(format!(
                "{}{}",
                project_name(path)?,
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
    fn extra_files(test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "main.go".to_owned(),
            content: include_templing!("src/gens/go/file_read_write.go.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "main.go".to_owned(),
            content: include_templing!("src/gens/go/tcp_read_write.go.templing"),
        }]
    }
}
