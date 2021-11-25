use super::*;

fn conv(name: &str) -> String {
    name.replace("Bool", "Boolean")
        .replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    main_package: String,
    files: HashMap<String, String>,
}

impl Generator {
    pub fn main_package(&self) -> &str {
        &self.main_package
    }
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

fn namespace_path_suffix(namespace: &Namespace) -> String {
    match namespace_path(namespace) {
        None => String::new(),
        Some(path) => format!(".{}", path),
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
            None => name.camel_case(conv),
            Some(path) => format!("{}/{}", path.replace('.', "/"), name.camel_case(conv)),
        },
        _ => unreachable!(),
    }
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

fn doc_to_string(name: &str) -> String {
    format!("/**\n * Get string representation of {}\n */", name)
}

impl Generator {
    pub fn default_value(&self, schema: &Schema) -> String {
        match schema {
            Schema::Bool => "false".to_owned(),
            Schema::Int32 | Schema::Int64 => "0".to_owned(),
            Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
            Schema::Map(..) => "mutableMapOf()".to_owned(),
            Schema::Vec(..) => "emptyArray()".to_owned(),
            Schema::Option(..) => "null".to_owned(),
            Schema::String => unimplemented!("No default string"),
            Schema::Struct { definition, .. } => {
                let mut result = format!("{}(", self.type_name(schema));
                for (index, field) in definition.fields.iter().enumerate() {
                    if index != 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.default_value(&field.schema));
                }
                result.push(')');
                result
            }
            Schema::Enum { .. } => unimplemented!("Can't determine default enum variant"),
            Schema::OneOf { .. } => unimplemented!("Can't determine default OneOf variant"),
        }
    }
    pub fn type_name(&self, schema: &Schema) -> String {
        match schema {
            Schema::Bool => "Boolean".to_owned(),
            Schema::Int32 => "Int".to_owned(),
            Schema::Int64 => "Long".to_owned(),
            Schema::Float32 => "Float".to_owned(),
            Schema::Float64 => "Double".to_owned(),
            Schema::String => "String".to_owned(),
            Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => format!(
                "{}{}.{}",
                self.main_package,
                namespace_path_suffix(schema.namespace().unwrap()),
                schema.name().unwrap().camel_case(conv)
            ),
            Schema::Option(inner) => format!("{}?", self.type_name(inner)),
            Schema::Vec(inner) => format!("Array<{}>", self.type_name(inner)),
            Schema::Map(key, value) => format!(
                "MutableMap<{}, {}>",
                self.type_name(key),
                self.type_name(value)
            ),
        }
    }
    fn read_var(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/kotlin/read_var.templing")
    }
    fn write_var(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/kotlin/write_var.templing")
    }
    fn var_to_string(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/kotlin/var_to_string.templing")
    }
    fn struct_impl(&self, definition: &Struct, base: Option<(&Name, usize)>) -> String {
        include_templing!("src/gens/kotlin/struct_impl.templing")
    }
    fn package(&self, namespace: &Namespace) -> String {
        format!("{}{}", &self.main_package, namespace_path_suffix(namespace))
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Kotlin";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let project_name = Name::new(name.to_owned()).kebab_case(conv);
        let project_name = &project_name;
        let main_package = Name::new(name.to_owned()).snake_case(conv);
        let mut files = HashMap::new();
        files.insert(
            "pom.xml".to_owned(),
            include_templing!("src/gens/kotlin/pom.xml.templing"),
        );
        files.insert(
            format!("src/main/kotlin/{}/util/StreamUtil.kt", main_package),
            include_str!("StreamUtil.kt").replace("main_package", &main_package),
        );
        Self {
            main_package,
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
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!(
                        "src/main/kotlin/{}/{}.kt",
                        self.main_package,
                        file_name(schema),
                    ),
                    include_templing!("src/gens/kotlin/enum.templing"),
                );
            }
            Schema::Struct {
                namespace,
                definition,
            } => {
                self.files.insert(
                    format!(
                        "src/main/kotlin/{}/{}.kt",
                        self.main_package,
                        file_name(schema),
                    ),
                    include_templing!("src/gens/kotlin/struct.templing"),
                );
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!(
                        "src/main/kotlin/{}/{}.kt",
                        self.main_package,
                        file_name(schema),
                    ),
                    include_templing!("src/gens/kotlin/oneof.templing"),
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
        command("mvn")
            .arg("package")
            .arg("--batch-mode")
            .current_dir(path)
            .show_output(verbose)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        fn project_name(path: &Path) -> anyhow::Result<String> {
            let pom =
                std::fs::read_to_string(path.join("pom.xml")).context("Failed to read pom.xml")?;
            for line in pom.lines() {
                let line = line.trim();
                if let Some(line) = line.strip_prefix("<name>") {
                    if let Some(line) = line.strip_suffix("</name>") {
                        return Ok(line.trim().to_owned());
                    }
                }
            }
            anyhow::bail!("Failed to determine project name")
        }
        let mut command = command("java");
        command
            .arg("-jar")
            .arg(format!(
                "target/{}-jar-with-dependencies.jar",
                project_name(path)?,
            ))
            .current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        let type_name = |schema: &Schema| -> String {
            format!(
                "{}{}.{}",
                self.main_package(),
                namespace_path_suffix(schema.namespace().unwrap()),
                schema.name().unwrap().camel_case(conv),
            )
        };
        vec![File {
            path: format!("src/main/kotlin/{}/Runner.kt", self.main_package()),
            content: include_templing!("src/gens/kotlin/FileReadWrite.kt.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        let type_name = |schema: &Schema| -> String {
            format!(
                "{}{}.{}",
                self.main_package(),
                namespace_path_suffix(schema.namespace().unwrap()),
                schema.name().unwrap().camel_case(conv),
            )
        };
        vec![File {
            path: format!("src/main/kotlin/{}/Runner.kt", self.main_package()),
            content: include_templing!("src/gens/kotlin/TcpReadWrite.kt.templing"),
        }]
    }
}
