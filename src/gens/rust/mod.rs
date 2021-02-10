use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "I32")
        .replace("Int64", "I64")
        .replace("Float32", "F32")
        .replace("Float64", "F64")
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "i32".to_owned(),
        Schema::Int64 => "i64".to_owned(),
        Schema::Float32 => "f32".to_owned(),
        Schema::Float64 => "f64".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Option(inner) => format!("Option<{}>", type_name(inner)),
        Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => schema
            .namespace()
            .unwrap()
            .parts
            .iter()
            .map(|name| name.snake_case(conv))
            .chain(std::iter::once(schema.name().unwrap().camel_case(conv)))
            .collect::<Vec<String>>()
            .join("::"),
        Schema::Vec(inner) => format!("Vec<{}>", type_name(inner)),
        Schema::Map(key_type, value_type) => format!(
            "std::collections::HashMap<{}, {}>",
            type_name(key_type),
            type_name(value_type)
        ),
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

fn file_name(schema: &Schema) -> String {
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

#[derive(Default)]
struct Package {
    inner_packages: BTreeSet<String>,
    types: BTreeSet<Name>,
}

pub struct Generator {
    packages: HashMap<String, Package>,
    crate_name: String,
    crate_version: String,
    files: HashMap<String, String>,
}

impl Generator {
    fn insert_package(&mut self, schema: &Schema) {
        let namespace = schema.namespace().unwrap();
        let mut parent: Option<String> = None;
        for part in &namespace.parts {
            let package = match &parent {
                Some(parent) => {
                    format!("{}::{}", parent, part.snake_case(conv))
                }
                None => part.snake_case(conv),
            };
            self.packages
                .entry(parent.unwrap_or("lib.rs".to_owned()))
                .or_default()
                .inner_packages
                .insert(part.snake_case(conv));
            parent = Some(package);
        }
        self.packages
            .entry(parent.unwrap_or("lib.rs".to_owned()))
            .or_default()
            .types
            .insert(schema.name().unwrap().clone());
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Rust";
    type Options = ();
    fn new(name: &str, version: &str, _: ()) -> Self {
        Self {
            packages: HashMap::new(),
            crate_name: Name::new(name.to_owned())
                .snake_case(conv)
                .replace('_', "-"),
            crate_version: version.to_owned(),
            files: HashMap::new(),
        }
    }
    fn generate(self, extra_files: Vec<File>) -> GenResult {
        let mut files = HashMap::new();
        for (package_name, package) in self.packages {
            let mut content = include_templing!("src/gens/rust/mod.rs.templing");
            if package_name == "lib.rs" {
                content = format!("pub mod trans;\n\n{}", content);
            } else {
                content = format!("use super::*;\n\n{}", content);
            }
            files.insert(
                if package_name == "lib.rs" {
                    "src/lib.rs".to_owned()
                } else {
                    format!("src/{}/mod.rs", package_name.replace("::", "/"))
                },
                content,
            );
        }
        let Self {
            crate_name,
            crate_version,
            ..
        } = self;
        files.insert(
            "Cargo.toml".to_owned(),
            include_templing!("src/gens/rust/Cargo.toml.templing"),
        );
        files.insert(
            "src/trans.rs".to_owned(),
            include_str!("trans.rs").to_owned(),
        );
        for (path, content) in self.files {
            files.insert(path, content);
        }
        for file in extra_files {
            files.insert(file.path, file.content);
        }
        files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Struct {
                definition:
                    Struct {
                        documentation,
                        name,
                        fields,
                    },
                ..
            } => {
                self.insert_package(schema);
                self.files.insert(
                    format!("src/{}.rs", file_name(schema)),
                    include_templing!("src/gens/rust/struct.templing"),
                );
            }
            Schema::OneOf {
                base_name,
                variants,
                documentation,
                ..
            } => {
                self.insert_package(schema);
                self.files.insert(
                    format!("src/{}.rs", file_name(schema)),
                    include_templing!("src/gens/rust/oneof.templing"),
                );
            }
            Schema::Enum {
                base_name,
                variants,
                documentation,
                ..
            } => {
                self.insert_package(schema);
                self.files.insert(
                    format!("src/{}.rs", file_name(schema)),
                    include_templing!("src/gens/rust/enum.templing"),
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
        command("cargo")
            .arg("build")
            .arg("--release")
            .current_dir(path)
            .run(verbose)
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        fn package_name(path: &Path) -> anyhow::Result<String> {
            let toml: toml::Value = toml::from_str(
                &std::fs::read_to_string(path.join("Cargo.toml"))
                    .context("Failed to read Cargo.toml")?,
            )
            .context("Failed to parse Cargo.toml")?;
            Ok(toml
                .get("package")
                .ok_or(anyhow!("Failed to find package in Cargo.toml"))?
                .get("name")
                .ok_or(anyhow!("Failed to find package name in Cargo.toml"))?
                .as_str()
                .ok_or(anyhow!("Package name is not string"))?
                .to_owned())
        }
        let mut command = command(
            path.join("target")
                .join("release")
                .join(format!(
                    "{}{}",
                    package_name(path)?,
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
            path: "src/main.rs".to_owned(),
            content: include_templing!("src/gens/rust/file_read_write.rs.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "src/main.rs".to_owned(),
            content: include_templing!("src/gens/rust/tcp_read_write.rs.templing"),
        }]
    }
}
