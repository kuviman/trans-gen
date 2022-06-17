use super::*;

pub fn conv(name: &str) -> String {
    name.replace("Float32", "Single")
        .replace("Float64", "Double")
}

pub struct Generator {
    project_name: Name,
    files: HashMap<String, String>,
}

pub fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "Boolean".to_owned(),
        Schema::Int32 => "Int32".to_owned(),
        Schema::Int64 => "Int64".to_owned(),
        Schema::Float32 => "Single".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Option(inner) => format!("TNullable<{}>", type_name(inner)),
        Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
            format!("T{}", schema.name().unwrap().camel_case(conv))
        }
        Schema::Vec(inner) => format!("TArray<{}>", type_name(inner)),
        Schema::Map(key_type, value_type) => {
            format!(
                "TDictionary<{}, {}>",
                type_name(key_type),
                type_name(value_type),
            )
        }
    }
}

pub fn imports(schema: &Schema) -> String {
    let mut imports = BTreeSet::new();
    fn add_imports_struct(definition: &Struct, imports: &mut BTreeSet<String>) {
        fn add_imports(schema: &Schema, imports: &mut BTreeSet<String>) {
            match schema {
                Schema::Struct { .. } | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    imports.insert(format!(
                        "U{} in '{}.pas'",
                        schema.name().unwrap().camel_case(conv),
                        file_name(schema),
                    ));
                }
                Schema::Option(inner) => {
                    imports.insert("Nullable".to_owned());
                    add_imports(inner, imports);
                }
                Schema::Vec(inner) => {
                    add_imports(inner, imports);
                }
                Schema::Map(key_type, value_type) => {
                    imports.insert("Generics.Collections".to_owned());
                    add_imports(key_type, imports);
                    add_imports(value_type, imports);
                }
                Schema::Bool
                | Schema::Int32
                | Schema::Int64
                | Schema::Float32
                | Schema::Float64
                | Schema::String => {}
            }
        }
        for field in &definition.fields {
            add_imports(&field.schema, imports);
        }
    }
    match schema {
        Schema::Struct { definition, .. } => {
            add_imports_struct(definition, &mut imports);
        }
        Schema::OneOf { variants, .. } => {
            for variant in variants {
                add_imports_struct(variant, &mut imports);
            }
        }
        _ => {}
    }
    imports.insert("Stream".to_owned());
    imports.insert("SysUtils".to_owned());
    include_templing!("src/gens/pascal/imports.templing")
}

fn doc_comment(documentation: &Documentation) -> String {
    let mut result = String::new();
    for line in documentation.get("en").unwrap().lines() {
        result.push_str("// ");
        result.push_str(line);
        result.push('\n');
    }
    result.trim().to_owned()
}

fn doc_read_from(name: &str) -> String {
    format!("// Read {} from input stream", name)
}

fn doc_write_to(name: &str) -> String {
    format!("// Write {} to output stream", name)
}

fn read_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/pascal/read_var.templing")
}

fn write_var(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/pascal/write_var.templing")
}

fn var_to_string(var: &str, schema: &Schema) -> String {
    include_templing!("src/gens/pascal/var_to_string.templing")
}

fn struct_def(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    include_templing!("src/gens/pascal/struct_def.templing")
}

fn struct_impl(definition: &Struct, base: Option<(&Name, usize)>) -> String {
    #[derive(Default)]
    struct Vars {
        read: BTreeMap<String, String>,
        write: BTreeMap<String, String>,
        to_string: BTreeMap<String, String>,
    }
    #[derive(Debug, PartialEq, Eq, Copy, Clone)]
    enum VarsMode {
        Read,
        Write,
        ToString,
    }
    let mut vars = Vars::default();
    fn add_vars(
        var: &str,
        schema: &Schema,
        vars: &mut BTreeMap<String, String>,
        field: bool,
        mode: VarsMode,
    ) {
        if !field || mode == VarsMode::Read {
            vars.insert(var.to_owned(), type_name(schema));
        }
        match schema {
            Schema::Option(inner) => {
                add_vars(&format!("{}Value", var), inner, vars, false, mode);
            }
            Schema::Vec(inner) => {
                if mode == VarsMode::Read || mode == VarsMode::ToString {
                    add_vars(&format!("{}Index", var), &Schema::Int32, vars, false, mode);
                }
                add_vars(&format!("{}Element", var), inner, vars, false, mode);
            }
            Schema::Map(key_type, value_type) => {
                if mode == VarsMode::Read {
                    add_vars(&format!("{}Size", var), &Schema::Int32, vars, false, mode);
                }
                if mode == VarsMode::Read {
                    add_vars(&format!("{}Index", var), &Schema::Int32, vars, false, mode);
                }
                add_vars(&format!("{}Key", var), key_type, vars, false, mode);
                add_vars(&format!("{}Value", var), value_type, vars, false, mode);
                if mode == VarsMode::ToString {
                    add_vars(&format!("{}First", var), &Schema::Bool, vars, false, mode);
                }
            }
            Schema::Enum { .. } => {
                if mode == VarsMode::ToString {
                    add_vars(&format!("{}Name", var), &Schema::String, vars, false, mode);
                }
            }
            _ => {}
        }
    }
    for field in &definition.fields {
        add_vars(
            &field.name.mixed_case(conv),
            &field.schema,
            &mut vars.read,
            true,
            VarsMode::Read,
        );
        add_vars(
            &field.name.mixed_case(conv),
            &field.schema,
            &mut vars.write,
            true,
            VarsMode::Write,
        );
        add_vars(
            &field.name.mixed_case(conv),
            &field.schema,
            &mut vars.to_string,
            true,
            VarsMode::ToString,
        );
    }
    include_templing!("src/gens/pascal/struct_impl.templing")
}

pub fn file_name(schema: &Schema) -> String {
    let mut result = String::new();
    for part in schema
        .namespace()
        .unwrap()
        .parts
        .iter()
        .map(|part| part.camel_case(conv))
        .chain(std::iter::once(format!(
            "U{}",
            schema.name().unwrap().camel_case(conv)
        )))
    {
        if !result.is_empty() {
            result.push('/');
        }
        result.push_str(&part);
    }
    result
}

pub fn default_value(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "false".to_owned(),
        Schema::Int32 | Schema::Int64 => "0".to_owned(),
        Schema::Float32 | Schema::Float64 => "0.0".to_owned(),
        Schema::Map(..) => format!("{}.Create", type_name(schema)),
        Schema::Vec(..) => "nil".to_owned(),
        Schema::Option(..) => "nil".to_owned(),
        Schema::String => unimplemented!("No default string"),
        Schema::Struct { definition, .. } => {
            let mut result = type_name(schema);
            result.push_str(".Create(");
            for (index, field) in definition.fields.iter().enumerate() {
                if index != 0 {
                    result.push_str(", ");
                }
                result.push_str(&default_value(&field.schema));
            }
            result.push(')');
            result
        }
        Schema::Enum { .. } => unimplemented!("Can't determine default enum variant"),
        Schema::OneOf { .. } => unimplemented!("Can't determine default OneOf variant"),
    }
}

pub fn tcp_stream_source() -> &'static str {
    include_str!("TcpStream.pas")
}

impl Generator {
    fn add_only(&mut self, schema: &Schema) -> anyhow::Result<()> {
        match schema {
            Schema::Enum {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.pas", file_name(schema)),
                    include_templing!("src/gens/pascal/enum.templing"),
                );
            }
            Schema::Struct { definition, .. } => {
                self.files.insert(
                    format!("{}.pas", file_name(schema)),
                    include_templing!("src/gens/pascal/struct.templing"),
                );
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
                ..
            } => {
                self.files.insert(
                    format!("{}.pas", file_name(schema)),
                    include_templing!("src/gens/pascal/oneof.templing"),
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
        Ok(())
    }
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Pascal";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "Stream.pas".to_owned(),
            include_str!("Stream.pas").to_owned(),
        );
        files.insert(
            "BufferedStream.pas".to_owned(),
            include_str!("BufferedStream.pas").to_owned(),
        );
        files.insert(
            "Nullable.pas".to_owned(),
            include_str!("Nullable.pas").to_owned(),
        );
        Self {
            project_name: Name::new(name.to_owned()),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        for file in extra_files {
            self.files.insert(file.path, file.content);
        }
        let sources: Vec<_> = self
            .files
            .keys()
            .filter(|path| path.ends_with(".pas") || path.ends_with(".dpr"))
            .collect();
        let project_name = self.project_name.camel_case(conv);
        let project_name = &project_name;
        let lpi = include_templing!("src/gens/pascal/Project.lpi.templing");
        self.files
            .insert(self.project_name.camel_case(conv) + ".lpi", lpi);
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        self.add_only(schema).unwrap();
    }
}

impl RunnableGenerator for Generator {
    fn build_local(path: &Path, verbose: bool) -> anyhow::Result<()> {
        let main = path
            .read_dir()?
            .filter_map(Result::ok)
            .find(|entry| entry.path().extension() == Some(std::ffi::OsStr::new("dpr")))
            .ok_or_else(|| anyhow!("Failed to find .dpr"))?
            .path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();
        // command("fpc")
        //     .arg("-Mdelphi")
        //     .arg("-O3")
        //     .arg(format!(
        //         "-o{}{}",
        //         main,
        //         if cfg!(windows) { ".exe" } else { "" }
        //     ))
        //     .arg(format!("{}.dpr", main))
        //     .current_dir(path)
        //     .show_output(verbose)
        //     .run()
        command("lazbuild")
            .arg(format!("{}.lpi", main))
            .current_dir(path)
            .show_output(verbose)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let main = path
            .read_dir()?
            .filter_map(Result::ok)
            .find(|entry| entry.path().extension() == Some(std::ffi::OsStr::new("dpr")))
            .ok_or_else(|| anyhow!("Failed to find .dpr"))?
            .path()
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();
        let mut command = command(
            path.join(format!(
                "{}{}",
                main,
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
        vec![
            File {
                path: "FileStream.pas".to_owned(),
                content: include_str!("FileStream.pas").to_owned(),
            },
            File {
                path: format!("{}.dpr", self.project_name.camel_case(conv)),
                content: include_templing!("src/gens/pascal/FileReadWrite.dpr.templing"),
            },
        ]
    }
}

impl<D: Trans + PartialEq + Debug> TestableGenerator<testing::TcpReadWrite<D>> for Generator {
    fn extra_files(&self, test: &testing::TcpReadWrite<D>) -> Vec<File> {
        let schema = Schema::of::<D>(&test.version);
        let schema: &Schema = &schema;
        vec![
            File {
                path: "TcpStream.pas".to_owned(),
                content: tcp_stream_source().to_owned(),
            },
            File {
                path: format!("{}.dpr", self.project_name.camel_case(conv)),
                content: include_templing!("src/gens/pascal/TcpReadWrite.dpr.templing"),
            },
        ]
    }
}
