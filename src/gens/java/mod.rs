use super::*;

fn conv(name: &str) -> String {
    name.replace("Bool", "Boolean")
        .replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
}

fn type_name(schema: &Schema) -> String {
    format!(
        "{}{}",
        type_name_prearray(schema),
        type_name_postarray(schema),
    )
}

fn type_name_obj(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "Boolean".to_owned(),
        Schema::Int32 => "Integer".to_owned(),
        Schema::Int64 => "Long".to_owned(),
        Schema::Float32 => "Float".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        _ => type_name(schema),
    }
}

fn type_name_prearray(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "boolean".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "long".to_owned(),
        Schema::Float32 => "float".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => format!("model.{}", name.camel_case(conv)),
        Schema::Option(inner) => type_name_obj(inner),
        Schema::Vec(inner) => type_name_prearray(inner),
        Schema::Map(key, value) => format!(
            "java.util.Map<{}, {}>",
            type_name_obj(key),
            type_name_obj(value)
        ),
    }
}

fn type_name_postarray(schema: &Schema) -> String {
    match schema {
        Schema::Vec(inner) => format!("[]{}", type_name_postarray(inner)),
        _ => String::new(),
    }
}

fn getter_prefix(schema: &Schema) -> &'static str {
    match schema {
        Schema::Bool => "is",
        _ => "get",
    }
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/java/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/java/write_var.templing")
}

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/java/struct_impl.templing")
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Java";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let project_name = Name::new(name.to_owned())
            .snake_case(conv)
            .replace('_', "-");
        let project_name = &project_name;
        let mut files = HashMap::new();
        files.insert(
            "pom.xml".to_owned(),
            include_templing!("src/gens/java/pom.xml.templing"),
        );
        files.insert(
            "src/main/java/util/StreamUtil.java".to_owned(),
            include_str!("StreamUtil.java").to_owned(),
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
        match schema {
            Schema::Enum {
                documentation: _,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!("src/main/java/model/{}.java", base_name.camel_case(conv)),
                    include_templing!("src/gens/java/enum.templing"),
                );
            }
            Schema::Struct(struc) => {
                self.files.insert(
                    format!("src/main/java/model/{}.java", struc.name.camel_case(conv)),
                    include_templing!("src/gens/java/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!("src/main/java/model/{}.java", base_name.camel_case(conv)),
                    include_templing!("src/gens/java/oneof.templing"),
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
        command("mvn")
            .arg("package")
            .arg("--batch-mode")
            .current_dir(path)
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

impl<D: Trans + PartialEq> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(_: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>();
        let schema: &Schema = &schema;
        vec![File {
            path: "src/main/java/Runner.java".to_owned(),
            content: include_templing!("src/gens/java/FileReadWrite.java.templing"),
        }]
    }
}
