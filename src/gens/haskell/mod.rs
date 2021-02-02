use super::*;

fn conv(name: &str) -> String {
    name.replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
}

fn imports(schema: &Schema) -> String {
    let mut imports = BTreeSet::new();
    fn add_imports_struct(struc: &Struct, imports: &mut BTreeSet<String>) {
        fn add_imports(schema: &Schema, imports: &mut BTreeSet<String>) {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                }
                | Schema::Enum {
                    base_name: name, ..
                } => {
                    imports.insert(format!(
                        "Model.{} ({})",
                        name.camel_case(conv),
                        name.camel_case(conv)
                    ));
                }
                Schema::Option(inner) => {
                    add_imports(inner, imports);
                }
                Schema::Vec(inner) => {
                    add_imports(inner, imports);
                }
                Schema::Map(key_type, value_type) => {
                    imports.insert("Data.Map".to_owned());
                    add_imports(key_type, imports);
                    add_imports(value_type, imports);
                }
                Schema::Int32 | Schema::Int64 => {
                    imports.insert("Data.Int".to_owned());
                }
                Schema::Bool | Schema::Float32 | Schema::Float64 | Schema::String => {}
            }
        }
        for field in &struc.fields {
            add_imports(&field.schema, imports);
        }
    }
    match schema {
        Schema::Struct(struc) => {
            add_imports_struct(struc, &mut imports);
        }
        Schema::OneOf { variants, .. } => {
            imports.insert("Data.Int".to_owned());
            for variant in variants {
                add_imports_struct(variant, &mut imports);
            }
        }
        Schema::Enum { .. } => {
            imports.insert("Data.Int".to_owned());
        }
        _ => {}
    }
    include_templing!("src/gens/haskell/imports.templing")
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "Bool".to_owned(),
        Schema::Int32 => "Int32".to_owned(),
        Schema::Int64 => "Int64".to_owned(),
        Schema::Float32 => "Float".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => format!("{}", name.camel_case(conv)),
        Schema::Option(inner) => format!("Maybe {}", type_name(inner)),
        Schema::Vec(inner) => format!("[{}]", type_name(inner)),
        Schema::Map(key, value) => format!("Map {} {}", type_name(key), type_name(value)),
    }
}

fn doc_comment(documentation: &Documentation, fields: &[Field]) -> String {
    include_templing!("src/gens/haskell/doc_comment.templing")
}

fn doc_read_from(name: &str) -> String {
    format!("/**\n * Read {} from input stream\n */", name)
}

fn doc_write_to(name: &str) -> String {
    format!("/**\n * Write {} to output stream\n */", name)
}

fn doc_to_string(name: &str) -> String {
    format!("/**\n * Get string representation of {}\n */", name)
}

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/haskell/struct_impl.templing")
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Haskell";
    type Options = ();
    fn new(name: &str, version: &str, _: ()) -> Self {
        let version = match version.find('-') {
            Some(index) => &version[..index],
            None => version,
        };
        let package_name = Name::new(name.to_owned())
            .snake_case(conv)
            .replace('_', "-");
        let package_name = &package_name;
        let mut files = HashMap::new();
        files.insert(
            "stack.yaml".to_owned(),
            include_templing!("src/gens/haskell/stack.yaml.templing"),
        );
        files.insert(
            "package.yaml".to_owned(),
            include_templing!("src/gens/haskell/package.yaml.templing"),
        );
        files.insert(
            "src/Trans.hs".to_owned(),
            include_str!("Trans.hs").to_owned(),
        );
        Self { files }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        let model_files: Vec<_> = self
            .files
            .keys()
            .filter_map(|file| {
                if let Some(name) = file.strip_prefix("src/Model/") {
                    if let Some(name) = name.strip_suffix(".hs") {
                        return Some(name);
                    }
                }
                None
            })
            .collect();
        let model_hs = include_templing!("src/gens/haskell/Model.hs.templing");
        self.files.insert("src/Model.hs".to_owned(), model_hs);
        for file in extra_files {
            self.files.insert(file.path, file.content);
        }
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                base_name,
                variants,
                documentation,
            } => {
                self.files.insert(
                    format!("src/Model/{}.hs", base_name.camel_case(conv)),
                    include_templing!("src/gens/haskell/enum.templing"),
                );
            }
            Schema::Struct(struc) => {
                self.files.insert(
                    format!("src/Model/{}.hs", struc.name.camel_case(conv)),
                    include_templing!("src/gens/haskell/struct.templing"),
                );
            }
            Schema::OneOf {
                base_name,
                variants,
                documentation,
            } => {
                self.files.insert(
                    format!("src/Model/{}.hs", base_name.camel_case(conv)),
                    include_templing!("src/gens/haskell/oneof.templing"),
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
        command("stack")
            .arg("--local-bin-path=bin")
            .arg("install")
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        fn project_name(path: &Path) -> anyhow::Result<String> {
            let package_yaml = std::fs::read_to_string(path.join("package.yaml"))
                .context("Failed to read package.yaml")?;
            for line in package_yaml.lines() {
                if let Some(name) = line.strip_prefix("name: ") {
                    return Ok(name.trim().to_owned());
                }
            }
            anyhow::bail!("Failed to determine project name")
        }
        let command = command(
            path.join("bin")
                .join(format!(
                    "{}{}",
                    project_name(path)?,
                    if cfg!(windows) { ".exe" } else { "" }
                ))
                .to_str()
                .unwrap(),
        );
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "src/Main.hs".to_owned(),
            content: include_templing!("src/gens/haskell/FileReadWrite.hs.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "src/Main.hs".to_owned(),
            content: include_templing!("src/gens/haskell/TcpReadWrite.hs.templing"),
        }]
    }
}
