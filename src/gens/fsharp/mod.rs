use super::*;

pub fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Int64")
        .replace("Float32", "Single")
        .replace("Float64", "Double")
        .replace("Params", "Parameters")
}

pub struct Generator {
    main_namespace: String,
    files: Vec<File>,
}

impl Generator {
    pub fn main_namespace(&self) -> &str {
        &self.main_namespace
    }
}

pub fn one_of_methods(schema: &Schema, call: &str) -> String {
    include_templing!("src/gens/fsharp/one_of_methods.templing")
}

pub fn default_value(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "false".to_owned(),
        Schema::Int32 | Schema::Int64 => "0".to_owned(),
        Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
        Schema::Map(..) => "Map.empty".to_owned(),
        Schema::Vec(..) => "Array.empty".to_owned(),
        Schema::Option(..) => "None".to_owned(),
        Schema::String => unimplemented!("No default string"),
        Schema::Struct { definition, .. } => {
            let mut result = "{\n".to_owned();
            for field in &definition.fields {
                result.push_str(&format!(
                    "    {} = {}\n",
                    field.name.camel_case(conv),
                    default_value(&field.schema),
                ));
            }
            result.push('}');
            result
        }
        Schema::Enum { .. } => unimplemented!("Can't determine default enum variant"),
        Schema::OneOf { .. } => unimplemented!("Can't determine default OneOf variant"),
    }
}

pub fn type_name(schema: &Schema) -> String {
    format!("{}{}", type_name_prearray(schema), type_post_array(schema))
}

fn type_name_prearray(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "int64".to_owned(),
        Schema::Float32 => "single".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => name_path(schema),
        Schema::Option(inner) => format!("option<{}>", type_name(inner)),
        Schema::Vec(inner) => type_name_prearray(inner),
        Schema::Map(key, value) => format!("Map<{}, {}>", type_name(key), type_name(value)),
    }
}

fn type_post_array(schema: &Schema) -> String {
    match schema {
        Schema::Vec(inner) => format!("[]{}", type_post_array(inner)),
        _ => String::new(),
    }
}

fn new_var(var: &str, suffix: &str) -> String {
    let var = match var.rfind('.') {
        Some(index) => &var[index + 1..],
        None => var,
    };
    Name::new(format!("{}{}", var, suffix)).mixed_case(conv)
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

fn read_var(schema: &Schema) -> String {
    include_templing!("src/gens/fsharp/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/fsharp/write_var.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/fsharp/struct_impl.templing")
}

fn namespace_path(namespace: &Namespace) -> String {
    namespace
        .parts
        .iter()
        .map(|name| name.camel_case(conv))
        .collect::<Vec<_>>()
        .join(".")
}

fn namespace_path_suffix(namespace: &Namespace) -> String {
    let namespace_path = namespace_path(namespace);
    if namespace_path.is_empty() {
        namespace_path
    } else {
        format!(".{}", namespace_path)
    }
}

fn name_path(schema: &Schema) -> String {
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
        } => {
            let namespace_path = namespace_path(namespace);
            if namespace_path.is_empty() {
                name.camel_case(conv)
            } else {
                format!("{}.{}", namespace_path, name.camel_case(conv))
            }
        }
        _ => unreachable!(),
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "F#";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        Self {
            main_namespace: Name::new(name.to_owned()).camel_case(conv),
            files: Vec::new(),
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        self.files.extend(extra_files);
        let source_files = self.files.iter().filter(|file| file.path.ends_with(".fs"));
        let fsproj = include_templing!("src/gens/fsharp/project.fsproj.templing");
        self.files.push(File {
            path: format!("{}.fsproj", self.main_namespace),
            content: fsproj,
        });
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
                self.files.push(File {
                    path: format!("{}.fs", name_path(schema).replace('.', "/")),
                    content: include_templing!("src/gens/fsharp/enum.templing"),
                });
            }
            Schema::Struct {
                namespace,
                definition,
            } => {
                self.files.push(File {
                    path: format!("{}.fs", name_path(schema).replace('.', "/")),
                    content: include_templing!("src/gens/fsharp/struct.templing"),
                });
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.files.push(File {
                    path: format!("{}.fs", name_path(schema).replace('.', "/")),
                    content: include_templing!("src/gens/fsharp/oneof.templing"),
                });
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
        command("dotnet")
            .current_dir(path)
            .arg("publish")
            .arg("-c")
            .arg("Release")
            .arg("-o")
            .arg(".")
            .show_output(verbose)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        fn project_name(path: &Path) -> anyhow::Result<String> {
            for file in std::fs::read_dir(path)? {
                let file = file?;
                if file.path().extension() == Some("fsproj".as_ref()) {
                    return Ok(file
                        .path()
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_owned());
                }
            }
            anyhow::bail!("Failed to determine project name")
        }
        let mut command = command("dotnet");
        command
            .arg(format!("{}.dll", project_name(path)?))
            .current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Runner.fs".to_owned(),
            content: include_templing!("src/gens/fsharp/FileReadWrite.fs.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Runner.fs".to_owned(),
            content: include_templing!("src/gens/fsharp/TcpReadWrite.fs.templing"),
        }]
    }
}
