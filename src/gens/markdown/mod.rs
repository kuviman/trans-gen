use super::*;

fn conv(name: &str) -> String {
    name.to_owned()
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "boolean".to_owned(),
        Schema::Int32 => "int32".to_owned(),
        Schema::Int64 => "int64".to_owned(),
        Schema::Float32 => "float32".to_owned(),
        Schema::Float64 => "float64".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Option(inner) => format!("Option<{}>", type_name(inner)),
        Schema::Struct {
            definition: Struct { name, .. },
            ..
        } => name.camel_case(conv),
        Schema::OneOf { base_name, .. } => base_name.camel_case(conv),
        Schema::Vec(inner) => format!("[{}]", type_name(inner)),
        Schema::Map(key_type, value_type) => {
            format!("Map<{} -> {}>", type_name(key_type), type_name(value_type))
        }
        Schema::Enum { base_name, .. } => base_name.camel_case(conv),
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Options {
    pub language: String,
    pub require_docs: bool,
}

impl Options {
    fn get_doc<'a>(&self, documentation: &'a Documentation, error_msg: &str) -> &'a str {
        if let Some(text) = documentation.get(&self.language) {
            text
        } else if self.require_docs {
            panic!("{}", error_msg);
        } else {
            "TODO: document"
        }
    }
}

impl Default for Options {
    fn default() -> Self {
        Self {
            language: "en".to_owned(),
            require_docs: false,
        }
    }
}

pub struct Generator {
    parts: Vec<String>,
    options: Options,
}
impl crate::Generator for Generator {
    const NAME: &'static str = "Markdown";
    type Options = Options;
    fn new(_name: &str, _version: &str, options: Options) -> Self {
        Self {
            parts: Vec::new(),
            options,
        }
    }
    fn generate(self, extra_files: Vec<File>) -> GenResult {
        let mut files = vec![File {
            path: "doc.md".to_owned(),
            content: self.parts.join("\n\n"),
        }];
        for file in extra_files {
            files.push(file);
        }
        GenResult { files }
    }
    fn add_only(&mut self, schema: &Schema) {
        let language = &self.options.language;
        match schema {
            Schema::Struct {
                namespace,
                definition:
                    Struct {
                        documentation,
                        name,
                        fields,
                    },
            } => {
                self.parts
                    .push(include_templing!("src/gens/markdown/struct.templing"));
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.parts
                    .push(include_templing!("src/gens/markdown/oneof.templing"));
            }
            Schema::Enum {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.parts
                    .push(include_templing!("src/gens/markdown/enum.templing"));
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
    fn is_runnable() -> bool {
        false
    }
    fn build_local(_path: &Path) -> anyhow::Result<()> {
        unreachable!()
    }
    fn run_local(_path: &Path) -> anyhow::Result<Command> {
        unreachable!()
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(_test: &testing::FileReadWrite<D>) -> Vec<File> {
        vec![]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(_test: &testing::TcpReadWrite<D>) -> Vec<File> {
        vec![]
    }
}
