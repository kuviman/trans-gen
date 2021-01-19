use super::*;

fn conv(name: &str) -> String {
    name.to_owned()
}

pub struct Generator {
    mod_name: String,
    files: HashMap<String, String>,
}

fn type_name(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int32".to_owned(),
        Schema::Int64 => "int64".to_owned(),
        Schema::Float32 => "float32".to_owned(),
        Schema::Float64 => "float64".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => name.camel_case(conv),
        Schema::Option(inner) => format!("*{}", type_name(inner)),
        Schema::Vec(inner) => format!("[]{}", type_name(inner)),
        Schema::Map(key, value) => format!("map[{}]{}", type_name(key), type_name(value),),
    }
}

fn index_var_name(index_var: &mut usize) -> String {
    let result = "ijk".chars().nth(*index_var).unwrap();
    *index_var += 1;
    result.to_string()
}

fn var_name(name: &str) -> &str {
    match name.rfind('.') {
        Some(index) => &name[(index + 1)..],
        None => name,
    }
}

fn needs_stream(schema: &Schema) -> bool {
    fn needs_stream_inner(schema: &Schema) -> bool {
        match schema {
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String
            | Schema::Enum { .. }
            | Schema::Vec(_)
            | Schema::Map(_, _)
            | Schema::Option(_) => true,
            Schema::Struct(_) => false,
            Schema::OneOf { .. } => false,
        }
    }
    fn struct_need_stream(struc: &Struct) -> bool {
        struc
            .fields
            .iter()
            .any(|field| needs_stream_inner(&field.schema))
    }
    match schema {
        Schema::Bool
        | Schema::Int32
        | Schema::Int64
        | Schema::Float32
        | Schema::Float64
        | Schema::String
        | Schema::Enum { .. }
        | Schema::Vec(_)
        | Schema::Map(_, _)
        | Schema::Option(_) => true,
        Schema::Struct(struc) => struct_need_stream(struc),
        Schema::OneOf { .. } => true,
    }
}

fn write_struct(
    writer: &mut Writer,
    struc: &Struct,
    base: Option<(&Name, usize)>,
) -> std::fmt::Result {
    let struct_name = match base {
        Some((base_name, _)) => format!(
            "{}{}",
            base_name.camel_case(conv),
            struc.name.camel_case(conv)
        ),
        None => struc.name.camel_case(conv),
    };
    writeln!(writer, "type {} struct {{", struct_name)?;
    writer.inc_ident();
    if let Some((_base_name, _tag)) = base {
        // TODO writeln!(writer, "static const int TAG = {}", tag)?;
    }

    // Fields
    for field in &struc.fields {
        writeln!(
            writer,
            "{} {}",
            field.name.camel_case(conv),
            type_name(&field.schema),
        )?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // Constructor
    write!(writer, "func New{}(", struct_name)?;
    for (index, field) in struc.fields.iter().enumerate() {
        if index > 0 {
            write!(writer, ", ")?;
        }
        write!(
            writer,
            "{} {}",
            field.name.mixed_case(conv),
            type_name(&field.schema),
        )?;
    }
    writeln!(writer, ") {} {{", struct_name)?;
    writer.inc_ident();
    writeln!(writer, "return {} {{", struct_name)?;
    writer.inc_ident();
    for (_index, field) in struc.fields.iter().enumerate() {
        writeln!(
            writer,
            "{}: {},",
            field.name.camel_case(conv),
            field.name.mixed_case(conv)
        )?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // Reading
    writeln!(
        writer,
        "func Read{}(reader io.Reader) {} {{",
        struct_name, struct_name
    )?;
    writer.inc_ident();
    writeln!(writer, "result := {} {{}}", struct_name)?;
    for field in &struc.fields {
        fn assign(
            writer: &mut Writer,
            to: &str,
            schema: &Schema,
            index_var: &mut usize,
        ) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "{} = ReadBool(reader)", to)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "{} = ReadInt32(reader)", to)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "{} = ReadInt64(reader)", to)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "{} = ReadFloat32(reader)", to)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "{} = ReadFloat64(reader)", to)?;
                }
                Schema::String => {
                    writeln!(writer, "{} = ReadString(reader)", to)?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                }
                | Schema::Enum {
                    base_name: name, ..
                } => {
                    writeln!(writer, "{} = Read{}(reader)", to, name.camel_case(conv))?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if ReadBool(reader) {{")?;
                    writer.inc_ident();
                    writeln!(writer, "var {}Value {}", var_name(to), type_name(inner))?;
                    assign(writer, &format!("{}Value", var_name(to)), inner, index_var)?;
                    writeln!(writer, "{} = &{}Value", to, var_name(to))?;
                    writer.dec_ident();
                    writeln!(writer, "}} else {{")?;
                    writeln!(writer, "    {} = nil", to)?;
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(
                        writer,
                        "{} = make({}, ReadInt32(reader))",
                        to,
                        type_name(schema),
                    )?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(writer, "for {} := range {} {{", index_var_name, to)?;
                    writer.inc_ident();
                    assign(
                        writer,
                        &format!("{}[{}]", to, index_var_name),
                        inner,
                        index_var,
                    )?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    let to_size = format!("{}Size", var_name(to));
                    writeln!(writer, "{} := ReadInt32(reader)", to_size)?;
                    writeln!(writer, "{} = make({})", to, type_name(schema))?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for {} := int32(0); {} < {}; {}++ {{",
                        index_var_name, index_var_name, to_size, index_var_name
                    )?;
                    writer.inc_ident();
                    writeln!(writer, "var {}Key {}", var_name(to), type_name(key_type))?;
                    assign(writer, &format!("{}Key", var_name(to)), key_type, index_var)?;
                    writeln!(
                        writer,
                        "var {}Value {}",
                        var_name(to),
                        type_name(value_type)
                    )?;
                    assign(
                        writer,
                        &format!("{}Value", var_name(to)),
                        value_type,
                        index_var,
                    )?;
                    writeln!(
                        writer,
                        "{}[{}Key] = {}Value",
                        to,
                        var_name(to),
                        var_name(to)
                    )?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
            }
            Ok(())
        }
        assign(
            writer,
            &format!("result.{}", field.name.camel_case(conv)),
            &field.schema,
            &mut 0,
        )?;
    }
    writeln!(writer, "return result")?;
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // Writing
    writeln!(
        writer,
        "func (value {}) Write(writer io.Writer) {{",
        struct_name,
    )?;
    writer.inc_ident();
    if let Some((_, tag)) = base {
        writeln!(writer, "WriteInt32(writer, {})", tag)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "WriteBool(writer, {})", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "WriteInt32(writer, {})", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "WriteInt64(writer, {})", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "WriteFloat32(writer, {})", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "WriteFloat64(writer, {})", value)?;
                }
                Schema::String => {
                    writeln!(writer, "WriteString(writer, {})", value)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "{}.Write(writer)", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if {} == nil {{", value)?;
                    writeln!(writer, "    WriteBool(writer, false)")?;
                    writeln!(writer, "}} else {{")?;
                    writer.inc_ident();
                    writeln!(writer, "WriteBool(writer, true)")?;
                    write(writer, &format!("(*{})", value), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "WriteInt32(writer, int32(len({})))", value)?;
                    writeln!(
                        writer,
                        "for _, {}Element := range {} {{",
                        var_name(value),
                        value
                    )?;
                    writer.inc_ident();
                    write(writer, &format!("{}Element", var_name(value)), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "WriteInt32(writer, int32(len({})))", value)?;
                    writeln!(
                        writer,
                        "for {}Key, {}Value := range {} {{",
                        var_name(value),
                        var_name(value),
                        value
                    )?;
                    writer.inc_ident();
                    write(writer, &format!("{}Key", var_name(value)), key_type)?;
                    write(writer, &format!("{}Value", var_name(value)), value_type)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum { .. } => {
                    writeln!(writer, "WriteInt32(writer, int32({}))", value)?;
                }
            }
            Ok(())
        }
        write(
            writer,
            &format!("value.{}", field.name.camel_case(conv)),
            &field.schema,
        )?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;
    Ok(())
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Go";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        let name = Name::new(name.to_owned());
        let mut files = HashMap::new();
        files.insert(
            "stream/stream.go".to_owned(),
            include_str!("stream.go").to_owned(),
        );
        files.insert(
            "go.mod".to_owned(),
            include_templing!("src/gens/go/go.mod.templing"),
        );
        Self {
            mod_name: name.snake_case(conv),
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
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.go", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import \"io\"").unwrap();
                writeln!(writer, "import . \"{}/stream\"", self.mod_name).unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "type {} int32", base_name.camel_case(conv)).unwrap();
                writeln!(writer, "const (").unwrap();
                writer.inc_ident();
                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "{}{} {} = {}",
                        base_name.camel_case(conv),
                        variant.name.camel_case(conv),
                        base_name.camel_case(conv),
                        tag,
                    )
                    .unwrap();
                }
                writer.dec_ident();
                writeln!(writer, ")").unwrap();

                writeln!(
                    writer,
                    "func Read{}(reader io.Reader) {} {{",
                    base_name.camel_case(conv),
                    base_name.camel_case(conv),
                )
                .unwrap();
                writer.inc_ident();
                writeln!(writer, "switch ReadInt32(reader) {{").unwrap();
                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(writer, "case {}:", tag).unwrap();
                    writeln!(
                        writer,
                        "    return {}{}",
                        base_name.camel_case(conv),
                        variant.name.camel_case(conv)
                    )
                    .unwrap();
                }
                writeln!(writer, "}}").unwrap();
                writeln!(writer, "panic(\"Unexpected tag value\")").unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                let file_name = format!("model/{}.go", struc.name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import \"io\"").unwrap();
                if needs_stream(schema) {
                    writeln!(writer, "import . \"{}/stream\"", self.mod_name).unwrap();
                }
                writeln!(writer).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.go", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import \"io\"").unwrap();
                if needs_stream(schema) {
                    writeln!(writer, "import . \"{}/stream\"", self.mod_name).unwrap();
                }
                writeln!(writer).unwrap();
                writeln!(writer, "type {} interface {{", base_name.camel_case(conv)).unwrap();
                writeln!(writer, "    Write(writer io.Writer)").unwrap();
                writeln!(writer, "}}").unwrap();
                {
                    writeln!(
                        writer,
                        "func Read{}(reader io.Reader) {} {{",
                        base_name.camel_case(conv),
                        base_name.camel_case(conv),
                    )
                    .unwrap();
                    writer.inc_ident();
                    writeln!(writer, "switch ReadInt32(reader) {{").unwrap();
                    writer.inc_ident();
                    for (tag, variant) in variants.iter().enumerate() {
                        writeln!(writer, "case {}:", tag).unwrap();
                        writeln!(
                            writer,
                            "    return Read{}{}(reader)",
                            base_name.camel_case(conv),
                            variant.name.camel_case(conv),
                        )
                        .unwrap();
                    }
                    writer.dec_ident();
                    writeln!(writer, "}}").unwrap();
                    writeln!(writer, "panic(\"Unexpected tag value\")").unwrap();
                    writer.dec_ident();
                    writeln!(writer, "}}").unwrap();
                    for (tag, variant) in variants.iter().enumerate() {
                        writeln!(writer).unwrap();
                        write_struct(&mut writer, variant, Some((base_name, tag))).unwrap();
                    }
                }
                self.files.insert(file_name, writer.get());
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

fn project_name(path: &Path) -> anyhow::Result<String> {
    Ok(std::fs::read_to_string(path.join("go.mod"))
        .context("Failed to read go.mod")?
        .lines()
        .next()
        .ok_or(anyhow!("go.mod is empty"))?
        .strip_prefix("module ")
        .ok_or(anyhow!("Expected to see module name in go.mod"))?
        .trim()
        .to_owned())
}

impl RunnableGenerator for Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        command("go")
            .arg("build")
            .arg("-o")
            .arg(format!(
                "{}{}",
                project_name(path)?,
                if cfg!(windows) { ".exe" } else { "" }
            ))
            .current_dir(path)
            .run()
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command(
            path.join(format!(
                "{}{}",
                project_name(path)?,
                if cfg!(windows) { ".exe" } else { "" }
            ))
            .to_str()
            .unwrap(),
        );
        command.current_dir(path);
        Ok(command)
    }
}

impl testing::FileReadWrite for Generator {
    fn extra_files(schema: &Schema) -> Vec<File> {
        vec![File {
            path: "main.go".to_owned(),
            content: include_templing!("src/gens/go/file_read_write.go.templing"),
        }]
    }
}
