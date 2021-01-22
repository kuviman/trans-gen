use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
    model_include: String,
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "long long".to_owned(),
        Schema::Float32 => "float".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "std::string".to_owned(),
        Schema::OneOf {
            base_name: name, ..
        } => format!("std::shared_ptr<{}>", name.camel_case(conv)),
        Schema::Struct(Struct { name, .. })
        | Schema::Enum {
            base_name: name, ..
        } => format!("{}", name.camel_case(conv)),
        Schema::Option(inner) => format!("std::shared_ptr<{}>", type_name(inner)),
        Schema::Vec(inner) => format!("std::vector<{}>", type_name(inner)),
        Schema::Map(key, value) => format!(
            "std::unordered_map<{}, {}>",
            type_name(key),
            type_name(value)
        ),
    }
}

fn includes(schema: &Schema) -> String {
    let mut includes = BTreeSet::new();
    includes.insert("<string>".to_owned());
    includes.insert("<sstream>".to_owned());
    includes.insert("\"../Stream.hpp\"".to_owned());
    collect_includes(&mut includes, schema, false);
    include_templing!("src/gens/cpp/includes.templing")
}

fn collect_includes(result: &mut BTreeSet<String>, schema: &Schema, current: bool) {
    if current {
        match schema {
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String => {}
            Schema::Option(_) => {
                result.insert("<memory>".to_owned());
            }
            Schema::Map(_, _) => {
                result.insert("<unordered_map>".to_owned());
            }
            Schema::Vec(_) => {
                result.insert("<vector>".to_owned());
            }
            Schema::Struct(Struct { name, .. })
            | Schema::OneOf {
                base_name: name, ..
            }
            | Schema::Enum {
                base_name: name, ..
            } => {
                result.insert("<stdexcept>".to_owned());
                result.insert(format!("\"{}.hpp\"", name.camel_case(conv)));
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
            collect_includes(result, inner, true);
        }
        Schema::Map(key_type, value_type) => {
            collect_includes(result, key_type, true);
            collect_includes(result, value_type, true);
        }
        Schema::Vec(inner) => {
            collect_includes(result, inner, true);
        }
        Schema::Struct(Struct { fields, .. }) => {
            for field in fields {
                collect_includes(result, &field.schema, true);
            }
        }
        Schema::OneOf { variants, .. } => {
            result.insert("<memory>".to_owned());
            for variant in variants {
                for field in &variant.fields {
                    collect_includes(result, &field.schema, true);
                }
            }
        }
    }
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/cpp/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/cpp/write_var.templing")
}

fn var_to_string(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/cpp/var_to_string.templing")
}

fn struct_def(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/cpp/struct_def.templing")
}

fn struct_impl(struc: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/cpp/struct_impl.templing")
}

impl crate::Generator for Generator {
    const NAME: &'static str = "C++";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let project_name = name;

        let mut files = HashMap::new();
        files.insert(
            "Stream.hpp".to_owned(),
            include_str!("Stream.hpp").to_owned(),
        );
        files.insert(
            "Stream.cpp".to_owned(),
            include_str!("Stream.cpp").to_owned(),
        );
        files.insert(
            "CMakeLists.txt".to_owned(),
            include_templing!("src/gens/cpp/CMakeLists.txt.templing"),
        );
        Self {
            files,
            model_include: "#ifndef _MODEL_HPP_\n#define _MODEL_HPP_\n\n".to_owned(),
        }
    }
    fn generate(self, extra_files: Vec<File>) -> GenResult {
        let Self {
            mut files,
            mut model_include,
        } = self;
        model_include.push_str("\n#endif\n");
        files.insert("model/Model.hpp".to_owned(), model_include.to_owned());
        for file in extra_files {
            files.insert(file.path, file.content);
        }
        files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                documentation: _,
                base_name,
                variants,
            } => {
                self.model_include.push_str(&format!(
                    "#include \"{}.hpp\"\n",
                    base_name.camel_case(conv)
                ));
                self.files.insert(
                    format!("model/{}.hpp", base_name.camel_case(conv)),
                    include_templing!("src/gens/cpp/enum-hpp.templing"),
                );
                self.files.insert(
                    format!("model/{}.cpp", base_name.camel_case(conv)),
                    include_templing!("src/gens/cpp/enum-cpp.templing"),
                );
            }
            Schema::Struct(struc) => {
                self.model_include.push_str(&format!(
                    "#include \"{}.hpp\"\n",
                    struc.name.camel_case(conv)
                ));
                self.files.insert(
                    format!("model/{}.hpp", struc.name.camel_case(conv)),
                    include_templing!("src/gens/cpp/struct-hpp.templing"),
                );
                self.files.insert(
                    format!("model/{}.cpp", struc.name.camel_case(conv)),
                    include_templing!("src/gens/cpp/struct-cpp.templing"),
                );
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                self.model_include.push_str(&format!(
                    "#include \"{}.hpp\"\n",
                    base_name.camel_case(conv)
                ));
                self.files.insert(
                    format!("model/{}.hpp", base_name.camel_case(conv)),
                    include_templing!("src/gens/cpp/oneof-hpp.templing"),
                );
                self.files.insert(
                    format!("model/{}.cpp", base_name.camel_case(conv)),
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
    fn build_local(path: &Path) -> anyhow::Result<()> {
        let standard: &str = "17";
        command("cmake")
            .current_dir(path)
            .arg(format!("-DCMAKE_CXX_STANDARD={}", standard))
            .arg("-DCMAKE_BUILD_TYPE=RELEASE")
            .arg("-DCMAKE_VERBOSE_MAKEFILE=ON")
            .arg(".")
            .run()?;
        command("cmake")
            .current_dir(path)
            .arg("--build")
            .arg(".")
            .arg("--config")
            .arg("Release")
            .run()?;
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let exe_dir = PathBuf::from(if cfg!(windows) { "Release" } else { "." });
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
        };
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

impl<D: Trans + PartialEq> TestableGenerator<testing::FileReadWrite<D>> for Generator {
    fn extra_files(_: &testing::FileReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>();
        let schema: &Schema = &schema;
        vec![File {
            path: "main.cpp".to_owned(),
            content: include_templing!("src/gens/cpp/file_read_write.cpp.templing"),
        }]
    }
}
