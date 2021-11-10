use super::*;

fn conv(name: &str) -> String {
    name.to_owned()
}

fn type_name_relative(from: &Schema, schema: &Schema) -> String {
    match schema {
        Schema::Bool => "boolean".to_owned(),
        Schema::Int32 => "int32".to_owned(),
        Schema::Int64 => "int64".to_owned(),
        Schema::Float32 => "float32".to_owned(),
        Schema::Float64 => "float64".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Option(inner) => format!("Option<{}>", type_name_relative(from, inner)),
        Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
            let source = from.namespace().unwrap().parts.clone();
            let target = schema.namespace().unwrap().parts.clone();
            let prefix = if target.starts_with(&source) {
                &target[source.len()..]
            } else {
                &target[..]
            };
            prefix
                .iter()
                .chain(std::iter::once(schema.name().unwrap()))
                .map(|name| name.camel_case(conv))
                .collect::<Vec<String>>()
                .join("::")
        }
        Schema::Vec(inner) => format!("[{}]", type_name_relative(from, inner)),
        Schema::Map(key_type, value_type) => format!(
            "Map<{} -> {}>",
            type_name_relative(from, key_type),
            type_name_relative(from, value_type)
        ),
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Options {
    pub language: String,
    #[serde(default)]
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
    namespaces: BTreeMap<Namespace, Vec<String>>,
    options: Options,
}

impl Generator {
    pub fn options(&self) -> &Options {
        &self.options
    }
    fn push(&mut self, namespace: &Namespace, content: String) {
        self.namespaces
            .entry(namespace.clone())
            .or_default()
            .push(content);
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Markdown";
    type Options = Options;
    fn new(_name: &str, _version: &str, options: Options) -> Self {
        Self {
            namespaces: BTreeMap::new(),
            options,
        }
    }
    fn generate(self, extra_files: Vec<File>) -> GenResult {
        let options = self.options;
        let mut files = vec![File {
            path: "doc.md".to_owned(),
            content: self
                .namespaces
                .into_iter()
                .map(|(namespace, parts)| {
                    format!(
                        "## {}\n\n{}",
                        if namespace.parts.is_empty() {
                            match options.language.as_str() {
                                "ru" => "Общее",
                                "en" | _ => "Common",
                            }
                            .to_owned()
                        } else {
                            namespace
                                .parts
                                .iter()
                                .map(|part| part.camel_case(conv))
                                .collect::<Vec<String>>()
                                .join("::")
                        },
                        parts.join("\n\n")
                    )
                })
                .collect::<Vec<String>>()
                .join("\n\n")
                + "\n",
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
                let content = include_templing!("src/gens/markdown/struct.templing");
                self.push(namespace, content);
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                let content = include_templing!("src/gens/markdown/oneof.templing");
                self.push(namespace, content);
            }
            Schema::Enum {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                let content = include_templing!("src/gens/markdown/enum.templing");
                self.push(namespace, content);
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
    fn build_local(_path: &Path, _verbose: bool) -> anyhow::Result<()> {
        unreachable!()
    }
    fn run_local(_path: &Path) -> anyhow::Result<Command> {
        unreachable!()
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(&self, _test: &testing::FileReadWrite<D>) -> Vec<File> {
        vec![]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, _test: &testing::TcpReadWrite<D>) -> Vec<File> {
        vec![]
    }
}
