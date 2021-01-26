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
        Schema::Struct(Struct { name, .. })
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
            Schema::Struct(_) => false,
            Schema::OneOf { .. } => false,
        }
    }
    fn struct_need_stream(struc: &Struct) -> bool {
        struc
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
        Schema::Struct(struc) => struct_need_stream(struc),
        Schema::OneOf { .. } => true,
    }
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

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/go/struct_impl.templing")
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
                documentation: _,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!("model/{}.go", base_name.snake_case(conv)),
                    include_templing!("src/gens/go/enum.templing"),
                );
            }
            Schema::Struct(struc) => {
                self.files.insert(
                    format!("model/{}.go", struc.name.snake_case(conv)),
                    include_templing!("src/gens/go/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!("model/{}.go", base_name.snake_case(conv)),
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
    fn extra_files(_: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>();
        let schema: &Schema = &schema;
        vec![File {
            path: "main.go".to_owned(),
            content: include_templing!("src/gens/go/file_read_write.go.templing"),
        }]
    }
}
