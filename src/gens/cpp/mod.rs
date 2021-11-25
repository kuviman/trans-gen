use super::*;

pub fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Options {
    pub cxx_standard: i32,
}

impl Default for Options {
    fn default() -> Self {
        Self { cxx_standard: 20 }
    }
}

pub struct Generator {
    project_name: Name,
    files: BTreeMap<String, String>,
    options: Options,
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
                .join("::"),
        )
    }
}

pub fn name_path(schema: &Schema) -> String {
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
            Some(path) => format!("{}::{}", path, name.camel_case(conv)),
            None => name.camel_case(conv),
        },
        _ => unreachable!(),
    }
}

pub fn file_name(schema: &Schema) -> String {
    name_path(schema).replace("::", "/")
}

impl Generator {
    pub fn one_of_methods_def(&self, schema: &Schema) -> String {
        include_templing!("src/gens/cpp/one_of_methods_def.templing")
    }
    pub fn one_of_methods_impl(&self, schema: &Schema, call: &str) -> String {
        include_templing!("src/gens/cpp/one_of_methods_impl.templing")
    }
    pub fn type_name(&self, schema: &Schema) -> String {
        match schema {
            Schema::Bool => "bool".to_owned(),
            Schema::Int32 => "int".to_owned(),
            Schema::Int64 => "long long".to_owned(),
            Schema::Float32 => "float".to_owned(),
            Schema::Float64 => "double".to_owned(),
            Schema::String => "std::string".to_owned(),
            Schema::OneOf { .. } => format!("std::shared_ptr<{}>", name_path(schema)),
            Schema::Struct { .. } | Schema::Enum { .. } => name_path(schema),
            Schema::Option(inner) => {
                if self.options.cxx_standard >= 17 {
                    format!("std::optional<{}>", self.type_name(inner))
                } else {
                    format!("std::shared_ptr<{}>", self.type_name(inner))
                }
            }
            Schema::Vec(inner) => format!("std::vector<{}>", self.type_name(inner)),
            Schema::Map(key, value) => format!(
                "std::unordered_map<{}, {}>",
                self.type_name(key),
                self.type_name(value)
            ),
        }
    }
    pub fn default_value(&self, schema: &Schema) -> String {
        match schema {
            Schema::Bool => "false".to_owned(),
            Schema::Int32 | Schema::Int64 => "0".to_owned(),
            Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
            Schema::Map(..) | Schema::Vec(..) | Schema::Option(..) => {
                format!("{}()", self.type_name(schema))
            }
            Schema::String => unimplemented!("No default string"),
            Schema::Struct { definition, .. } => {
                let mut result = self.type_name(schema);
                result.push('(');
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
    fn includes(&self, schema: &Schema) -> String {
        let mut includes = BTreeSet::new();
        includes.insert("<string>".to_owned());
        includes.insert("<sstream>".to_owned());
        includes.insert("\"Stream.hpp\"".to_owned());
        self.collect_includes(&mut includes, schema, false);
        include_templing!("src/gens/cpp/includes.templing")
    }
    fn collect_includes(&self, result: &mut BTreeSet<String>, schema: &Schema, current: bool) {
        if current {
            match schema {
                Schema::Bool
                | Schema::Int32
                | Schema::Int64
                | Schema::Float32
                | Schema::Float64
                | Schema::String => {}
                Schema::Option(_) => {
                    if self.options.cxx_standard >= 17 {
                        result.insert("<optional>".to_owned());
                    } else {
                        result.insert("<memory>".to_owned());
                    }
                }
                Schema::Map(_, _) => {
                    result.insert("<unordered_map>".to_owned());
                }
                Schema::Vec(_) => {
                    result.insert("<vector>".to_owned());
                }
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    result.insert("<stdexcept>".to_owned());
                    result.insert(format!("\"{}.hpp\"", file_name(schema)));
                }
            }
        }
        match schema {
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String
            | Schema::Enum { .. } => {}
            Schema::Option(inner) => {
                self.collect_includes(result, inner, true);
            }
            Schema::Map(key_type, value_type) => {
                self.collect_includes(result, key_type, true);
                self.collect_includes(result, value_type, true);
            }
            Schema::Vec(inner) => {
                self.collect_includes(result, inner, true);
            }
            Schema::Struct {
                definition: Struct { fields, .. },
                ..
            } => {
                for field in fields {
                    self.collect_includes(result, &field.schema, true);
                }
            }
            Schema::OneOf { variants, .. } => {
                result.insert("<memory>".to_owned());
                for variant in variants {
                    for field in &variant.fields {
                        self.collect_includes(result, &field.schema, true);
                    }
                }
            }
        }
    }
    fn doc_comment(&self, documentation: &Documentation) -> String {
        let mut result = String::new();
        for line in documentation.get("en").unwrap().lines() {
            result.push_str("// ");
            result.push_str(line);
            result.push('\n');
        }
        result.trim().to_owned()
    }
    fn doc_read_from(&self, name: &str) -> String {
        format!("// Read {} from input stream", name)
    }
    fn doc_write_to(&self, name: &str) -> String {
        format!("// Write {} to output stream", name)
    }
    fn doc_to_string(&self, name: &str) -> String {
        format!("// Get string representation of {}", name)
    }
    fn read_var(&self, var: &str, schema: &Schema, decl_var: bool) -> String {
        include_templing!("src/gens/cpp/read_var.templing")
    }
    fn write_var(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/cpp/write_var.templing")
    }
    fn var_to_string(&self, var: &str, schema: &Schema) -> String {
        include_templing!("src/gens/cpp/var_to_string.templing")
    }
    fn struct_def(&self, definition: &Struct, base: Option<(&Name, usize)>) -> String {
        include_templing!("src/gens/cpp/struct_def.templing")
    }
    fn struct_impl(&self, definition: &Struct, base: Option<(&Name, usize)>) -> String {
        include_templing!("src/gens/cpp/struct_impl.templing")
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "C++";
    type Options = Options;
    fn new(name: &str, _version: &str, options: Options) -> Self {
        let mut files = BTreeMap::new();
        files.insert(
            "Stream.hpp".to_owned(),
            include_str!("Stream.hpp").to_owned(),
        );
        files.insert(
            "Stream.cpp".to_owned(),
            include_str!("Stream.cpp").to_owned(),
        );
        Self {
            project_name: Name::new(name.to_owned()),
            files,
            options,
        }
    }
    fn generate(self, extra_files: Vec<File>) -> GenResult {
        let Self { mut files, .. } = self;
        for file in extra_files {
            files.insert(file.path, file.content);
        }
        let source_files = files.keys().filter(|file| file.ends_with(".cpp"));
        let header_files = files.keys().filter(|file| file.ends_with(".hpp"));
        let cmakelists = include_templing!("src/gens/cpp/CMakeLists.txt.templing");
        files.insert("CMakeLists.txt".to_owned(), cmakelists);
        files.into()
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
                    format!("{}.hpp", file_name(schema)),
                    include_templing!("src/gens/cpp/enum-hpp.templing"),
                );
                self.files.insert(
                    format!("{}.cpp", file_name(schema)),
                    include_templing!("src/gens/cpp/enum-cpp.templing"),
                );
            }
            Schema::Struct {
                namespace,
                definition,
            } => {
                self.files.insert(
                    format!("{}.hpp", file_name(schema)),
                    include_templing!("src/gens/cpp/struct-hpp.templing"),
                );
                self.files.insert(
                    format!("{}.cpp", file_name(schema)),
                    include_templing!("src/gens/cpp/struct-cpp.templing"),
                );
            }
            Schema::OneOf {
                namespace,
                documentation,
                base_name,
                variants,
            } => {
                self.files.insert(
                    format!("{}.hpp", file_name(schema)),
                    include_templing!("src/gens/cpp/oneof-hpp.templing"),
                );
                self.files.insert(
                    format!("{}.cpp", file_name(schema)),
                    include_templing!("src/gens/cpp/oneof-cpp.templing"),
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
        command("cmake")
            .current_dir(path)
            .arg("-DCMAKE_BUILD_TYPE=RELEASE")
            .arg("-DCMAKE_VERBOSE_MAKEFILE=ON")
            .arg(".")
            .show_output(verbose)
            .run()?;
        command("cmake")
            .current_dir(path)
            .arg("--build")
            .arg(".")
            .arg("--config")
            .arg("Release")
            .show_output(verbose)
            .run()?;
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let exe_dir = path.join(if cfg!(windows) { "Release" } else { "." });
        fn executable(path: &Path) -> anyhow::Result<String> {
            for line in std::fs::read_to_string(path.join("CMakeLists.txt"))?.lines() {
                if let Some(args) = line.strip_prefix("add_executable(") {
                    match args.split_whitespace().next() {
                        Some(executable) => return Ok(executable.to_owned()),
                        None => anyhow::bail!("Failed to parse executable()"),
                    }
                }
            }
            anyhow::bail!("Failed to determine executable");
        }
        let executable = executable(path)?;
        let mut command = command(
            exe_dir
                .join(format!(
                    "{}{}",
                    executable,
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
    fn extra_files(&self, test: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        fn type_name(schema: &Schema) -> String {
            match schema {
                Schema::OneOf { .. } => format!("std::shared_ptr<{}>", name_path(schema)),
                _ => name_path(schema),
            }
        }
        let access = match schema {
            Schema::OneOf { .. } => "->",
            _ => ".",
        };
        vec![File {
            path: "main.cpp".to_owned(),
            content: include_templing!("src/gens/cpp/file_read_write.cpp.templing"),
        }]
    }
}

pub fn tcp_stream_hpp_source() -> &'static str {
    include_str!("TcpStream.hpp")
}

pub fn tcp_stream_cpp_source() -> &'static str {
    include_str!("TcpStream.cpp")
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        fn type_name(schema: &Schema) -> String {
            match schema {
                Schema::OneOf { .. } => format!("std::shared_ptr<{}>", name_path(schema)),
                _ => name_path(schema),
            }
        }
        let access = match schema {
            Schema::OneOf { .. } => "->",
            _ => ".",
        };
        vec![
            File {
                path: "TcpStream.hpp".to_owned(),
                content: tcp_stream_hpp_source().to_owned(),
            },
            File {
                path: "TcpStream.cpp".to_owned(),
                content: tcp_stream_cpp_source().to_owned(),
            },
            File {
                path: "main.cpp".to_owned(),
                content: include_templing!("src/gens/cpp/tcp_read_write.cpp.templing"),
            },
        ]
    }
}
