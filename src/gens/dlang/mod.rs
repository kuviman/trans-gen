use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
        .replace("Params", "Parameters")
}

pub struct Generator {
    model_init: String,
    files: HashMap<String, String>,
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "long".to_owned(),
        Schema::Float32 => "float".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => name.camel_case(conv),
        Schema::Option(inner) => format!("Nullable!({})", type_name(inner)),
        Schema::Vec(inner) => format!("{}[]", type_name(inner)),
        Schema::Map(key, value) => format!("{}[{}]", type_name(value), type_name(key),),
    }
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/dlang/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/dlang/write_var.templing")
}

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/dlang/struct_impl.templing")
}

impl crate::Generator for Generator {
    const NAME: &'static str = "D";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let name = Name::new(name.to_owned());
        let mut files = HashMap::new();
        files.insert(
            "source/stream.d".to_owned(),
            include_str!("stream.d").to_owned(),
        );
        files.insert(
            "dub.json".to_owned(),
            include_templing!("src/gens/dlang/dub.json.templing"),
        );
        Self {
            model_init: String::new(),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        if !self.model_init.is_empty() {
            self.files
                .insert("source/model/package.d".to_owned(), self.model_init);
        }
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
                writeln!(
                    &mut self.model_init,
                    "public import {};",
                    base_name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(
                    format!("source/model/{}.d", base_name.snake_case(conv)),
                    include_templing!("src/gens/dlang/enum.templing"),
                );
            }
            Schema::Struct(struc) => {
                writeln!(
                    &mut self.model_init,
                    "public import {};",
                    struc.name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(
                    format!("source/model/{}.d", struc.name.snake_case(conv)),
                    include_templing!("src/gens/dlang/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                writeln!(
                    &mut self.model_init,
                    "public import {};",
                    base_name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(
                    format!("source/model/{}.d", base_name.snake_case(conv)),
                    include_templing!("src/gens/dlang/oneof.templing"),
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
        command("dub")
            .arg("build")
            .arg("-b")
            .arg("release")
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let project_name = serde_json::from_str::<serde_json::Value>(
            &std::fs::read_to_string(path.join("dub.json")).context("Failed to read dub.json")?,
        )
        .context("Failed to parse dub.json")?
        .get("name")
        .ok_or(anyhow!("No name in dub.json"))?
        .as_str()
        .ok_or(anyhow!("Name is not string in dub.json"))?
        .to_owned();
        let mut command = command(
            path.join(format!(
                "{}{}",
                project_name,
                if cfg!(windows) { ".exe" } else { "" }
            ))
            .to_str()
            .unwrap(),
        );
        command.current_dir(path);
        Ok(command)
    }
}

impl<D: Trans + PartialEq> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(_: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>();
        let schema: &Schema = &schema;
        vec![File {
            path: "source/app.d".to_owned(),
            content: include_templing!("src/gens/dlang/file_read_write.d.templing"),
        }]
    }
}

impl<D: Trans + Debug> TestableGenerator<testing::ToString<D>> for Generator {
    fn extra_files(_: &testing::ToString<D>) -> Vec<File> {
        let schema = Schema::of::<D>();
        let schema: &Schema = &schema;
        vec![File {
            path: "source/app.d".to_owned(),
            content: include_templing!("src/gens/dlang/file_read_write.d.templing"),
        }]
    }
}
