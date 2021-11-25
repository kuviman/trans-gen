use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
        .replace("Params", "Parameters")
}

pub struct Generator {
    main_namespace: String,
    files: HashMap<String, String>,
}

impl Generator {
    pub fn main_namespace(&self) -> &str {
        &self.main_namespace
    }

    fn new_var(&self, var: &str, suffix: &str) -> String {
        let var = match var.find('.') {
            Some(index) => &var[index + 1..],
            None => var,
        };
        let indexing_count = var.chars().filter(|&c| c == '[').count();
        let var = match var.find('[') {
            Some(index) => &var[..index],
            None => var,
        };
        let mut var = var.to_owned();
        for _ in 0..indexing_count {
            var.push_str("Element");
        }
        var.push_str(suffix);
        Name::new(var).mixed_case(conv)
    }

    fn is_null(&self, var: &str, schema: &Schema) -> String {
        if self.nullable(schema) {
            format!("{} == null", var)
        } else {
            format!("!{}.HasValue", var)
        }
    }

    fn option_unwrap(&self, var: &str, schema: &Schema) -> String {
        if self.nullable(schema) {
            var.to_owned()
        } else {
            format!("{}.Value", var)
        }
    }

    fn nullable(&self, schema: &Schema) -> bool {
        match schema {
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::Enum { .. }
            | Schema::Struct { .. } => false,
            Schema::String
            | Schema::Option(_)
            | Schema::Vec(_)
            | Schema::Map(_, _)
            | Schema::OneOf { .. } => true,
        }
    }

    pub fn type_name(&self, schema: &Schema) -> String {
        format!(
            "{}{}",
            self.type_name_prearray(schema),
            self.type_name_postarray(schema),
        )
    }

    fn type_name_prearray(&self, schema: &Schema) -> String {
        match schema {
            Schema::Bool => "bool".to_owned(),
            Schema::Int32 => "int".to_owned(),
            Schema::Int64 => "long".to_owned(),
            Schema::Float32 => "float".to_owned(),
            Schema::Float64 => "double".to_owned(),
            Schema::String => "string".to_owned(),
            Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                self.full_name_path(schema)
            }
            Schema::Option(inner) => {
                if self.nullable(inner) {
                    self.type_name(inner)
                } else {
                    format!("{}?", self.type_name(inner))
                }
            }
            Schema::Vec(inner) => self.type_name_prearray(inner),
            Schema::Map(key, value) => format!(
                "System.Collections.Generic.IDictionary<{}, {}>",
                self.type_name(key),
                self.type_name(value)
            ),
        }
    }

    fn type_name_postarray(&self, schema: &Schema) -> String {
        match schema {
            Schema::Vec(inner) => format!("[]{}", self.type_name_postarray(inner)),
            _ => String::new(),
        }
    }

    pub fn default_value(&self, schema: &Schema) -> String {
        match schema {
            Schema::Bool => "false".to_owned(),
            Schema::Int32 | Schema::Int64 => "0".to_owned(),
            Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
            Schema::Map(key, value) => format!(
                "new System.Collections.Generic.Dictionary<{}, {}>()",
                self.type_name(key),
                self.type_name(value),
            ),
            Schema::Vec(inner) => format!(
                "new {}[0]{}",
                self.type_name_prearray(inner),
                self.type_name_postarray(inner),
            ),
            Schema::Option(..) => "null".to_owned(),
            Schema::String => unimplemented!("No default string"),
            Schema::Struct { definition, .. } => {
                let mut result = format!("new {}(", self.type_name(schema));
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

    fn doc_comment(&self, documentation: &Documentation) -> String {
        let mut result = String::new();
        result.push_str("/// <summary>\n");
        for line in documentation.get("en").unwrap().lines() {
            result.push_str("/// ");
            result.push_str(line);
            result.push('\n');
        }
        result.push_str("/// </summary>\n");
        result.trim().to_owned()
    }

    fn doc_read_from(&self, name: &str) -> String {
        format!("/// <summary> Read {} from reader </summary>", name)
    }

    fn doc_write_to(&self, name: &str) -> String {
        format!("/// <summary> Write {} to writer </summary>", name)
    }

    fn doc_to_string(&self, name: &str) -> String {
        format!(
            "/// <summary> Get string representation of {} </summary>",
            name,
        )
    }

    fn read_var(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/csharp/read_var.templing")
    }

    fn write_var(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/csharp/write_var.templing")
    }

    fn var_to_string(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/csharp/var_to_string.templing")
    }

    fn struct_impl(&self, definition: &Struct, base: Option<(&Name, usize)>) -> String {
        include_templing!("src/gens/csharp/struct_impl.templing")
    }

    fn namespace_path(&self, namespace: &Namespace) -> String {
        namespace
            .parts
            .iter()
            .map(|name| name.camel_case(conv))
            .collect::<Vec<_>>()
            .join(".")
    }

    fn namespace_path_suffix(&self, namespace: &Namespace) -> String {
        let namespace_path = self.namespace_path(namespace);
        if namespace_path.is_empty() {
            namespace_path
        } else {
            format!(".{}", namespace_path)
        }
    }

    fn short_name_path(&self, schema: &Schema) -> String {
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
                let namespace_path = self.namespace_path(namespace);
                if namespace_path.is_empty() {
                    name.camel_case(conv)
                } else {
                    format!("{}.{}", namespace_path, name.camel_case(conv))
                }
            }
            _ => unreachable!(),
        }
    }

    fn full_name_path(&self, schema: &Schema) -> String {
        format!("{}.{}", self.main_namespace, self.short_name_path(schema))
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "C#";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let name = Name::new(name.to_owned());
        let mut files = HashMap::new();
        files.insert(
            format!("{}.csproj", name.camel_case(conv)),
            include_str!("project.csproj").to_owned(),
        );
        Self {
            main_namespace: name.camel_case(conv),
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
                    format!("{}.cs", self.short_name_path(schema).replace('.', "/")),
                    include_templing!("src/gens/csharp/enum.templing"),
                );
            }
            Schema::Struct {
                namespace,
                definition,
            } => {
                self.files.insert(
                    format!("{}.cs", self.short_name_path(schema).replace('.', "/")),
                    include_templing!("src/gens/csharp/struct.templing"),
                );
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!("{}.cs", self.short_name_path(schema).replace('.', "/")),
                    include_templing!("src/gens/csharp/oneof.templing"),
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
                if file.path().extension() == Some("csproj".as_ref()) {
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
            path: "Runner.cs".to_owned(),
            content: include_templing!("src/gens/csharp/FileReadWrite.cs.templing"),
        }]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![File {
            path: "Runner.cs".to_owned(),
            content: include_templing!("src/gens/csharp/TcpReadWrite.cs.templing"),
        }]
    }
}
