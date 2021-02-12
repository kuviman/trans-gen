use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    package_name: String,
    files: HashMap<String, String>,
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "Bool".to_owned(),
        Schema::Int32 => "Int32".to_owned(),
        Schema::Int64 => "Int64".to_owned(),
        Schema::Float32 => "Float".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        Schema::String => "String".to_owned(),
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
        Schema::Option(inner) => format!("{}?", type_name(inner)),
        Schema::Vec(inner) => format!("[{}]", type_name(inner)),
        Schema::Map(key, value) => format!("[{}: {}]", type_name(key), type_name(value)),
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

fn doc_read_from(name: &str) -> String {
    format!("/// Read {} from input stream", name)
}

fn doc_write_to(name: &str) -> String {
    format!("/// Write {} to output stream", name)
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/swift/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/swift/write_var.templing")
}

fn file_name(schema: &Schema) -> String {
    schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .chain(std::iter::once(schema.name().unwrap()))
        .map(|name| name.camel_case(conv))
        .collect::<Vec<String>>()
        .join("/")
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Swift";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let name = Name::new(name.to_owned());
        let mut files = HashMap::new();
        let package_name = name.camel_case(conv);
        let package_name = &package_name;
        files.insert(
            "Package.swift".to_owned(),
            include_templing!("src/gens/swift/Package.swift.templing"),
        );
        files.insert(
            format!("Sources/{}/Stream.swift", package_name),
            include_str!("Stream.swift").to_owned(),
        );
        files.insert(
            format!("Sources/{}/BufferedStream.swift", package_name),
            include_str!("BufferedStream.swift").to_owned(),
        );
        Self {
            package_name: package_name.to_owned(),
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
                    format!("Sources/{}/{}.swift", self.package_name, file_name(schema)),
                    include_templing!("src/gens/swift/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.files.insert(
                    format!("Sources/{}/{}.swift", self.package_name, file_name(schema)),
                    include_templing!("src/gens/swift/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("Sources/{}/{}.swift", self.package_name, file_name(schema)),
                    include_templing!("src/gens/swift/oneof.templing"),
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
        command("swift")
            .current_dir(path)
            .arg("build")
            .arg("-c")
            .arg("release")
            .show_output(verbose)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        fn project_name(path: &Path) -> anyhow::Result<String> {
            for file in std::fs::read_dir(path)? {
                let file = file?;
                if file.path().file_name().unwrap() == "Package.swift" {
                    for line in std::fs::read_to_string(file.path())?.lines() {
                        let line = line.trim();
                        if line.starts_with("name:") {
                            let start = line
                                .find('"')
                                .ok_or(anyhow!("Faled to parse Package.swift"))?
                                + 1;
                            let end = line.rfind('"').unwrap();
                            return Ok(line[start..end].to_owned());
                        }
                    }
                }
            }
            anyhow::bail!("Failed to determine project name")
        }
        let mut command = command(
            path.join(".build")
                .join("release")
                .join(project_name(path)?)
                .to_str()
                .unwrap(),
        );
        command.current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Sources/TransGenTest/main.swift".to_owned(),
            content: include_templing!("src/gens/swift/FileReadWrite.swift.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "Sources/TransGenTest/TcpStream.swift".to_owned(),
                content: include_str!("TcpStream.swift").to_owned(),
            },
            File {
                path: "Sources/TransGenTest/main.swift".to_owned(),
                content: include_templing!("src/gens/swift/TcpReadWrite.swift.templing"),
            },
        ]
    }
}
