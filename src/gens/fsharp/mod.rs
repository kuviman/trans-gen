use super::*;

fn conv(name: &str) -> String {
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

fn type_name(schema: &Schema) -> String {
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
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => format!("{}", name.camel_case(conv)),
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

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/fsharp/struct_impl.templing")
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
                documentation,
                base_name,
                variants,
            } => {
                self.files.push(File {
                    path: format!("Model/{}.fs", base_name.camel_case(conv)),
                    content: include_templing!("src/gens/fsharp/enum.templing"),
                });
            }
            Schema::Struct(struc) => {
                self.files.push(File {
                    path: format!("Model/{}.fs", struc.name.camel_case(conv)),
                    content: include_templing!("src/gens/fsharp/struct.templing"),
                });
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
            } => {
                self.files.push(File {
                    path: format!("Model/{}.fs", base_name.camel_case(conv)),
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
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("dotnet")
            .current_dir(path)
            .arg("publish")
            .arg("-c")
            .arg("Release")
            .arg("-o")
            .arg(".")
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
    fn extra_files(test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Runner.fs".to_owned(),
            content: include_templing!("src/gens/fsharp/FileReadWrite.fs.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Runner.fs".to_owned(),
            content: include_templing!("src/gens/fsharp/TcpReadWrite.fs.templing"),
        }]
    }
}
