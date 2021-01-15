use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    model_init: String,
    files: HashMap<String, String>,
}

fn write_struct(
    writer: &mut Writer,
    struc: &Struct,
    base: Option<(&Name, usize)>,
) -> std::fmt::Result {
    // Imports
    for field in &struc.fields {
        fn add_imports(writer: &mut Writer, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                }
                | Schema::Enum {
                    base_name: name, ..
                } => {
                    writeln!(
                        writer,
                        "from .{} import {}",
                        name.snake_case(conv),
                        name.camel_case(conv)
                    )?;
                }
                Schema::Option(inner) => {
                    add_imports(writer, inner)?;
                }
                Schema::Vec(inner) => {
                    add_imports(writer, inner)?;
                }
                Schema::Map(key_type, value_type) => {
                    add_imports(writer, key_type)?;
                    add_imports(writer, value_type)?;
                }
                Schema::Bool
                | Schema::Int32
                | Schema::Int64
                | Schema::Float32
                | Schema::Float64
                | Schema::String => {}
            }
            Ok(())
        }
        add_imports(writer, &field.schema)?;
    }

    // Class
    write!(writer, "class {}", struc.name.camel_case(conv))?;
    if let Some((base_name, _)) = base {
        write!(writer, "({})", base_name.camel_case(conv))?;
    }
    writeln!(writer, ":")?;
    writer.inc_ident();
    if let Some((_, tag)) = base {
        writeln!(writer, "TAG = {}", tag)?;
    }

    // Constructor
    write!(writer, "def __init__(self")?;
    for field in &struc.fields {
        write!(writer, ", {}", field.name.snake_case(conv))?;
    }
    writeln!(writer, "):")?;
    for field in &struc.fields {
        writeln!(
            writer,
            "    self.{} = {}",
            field.name.snake_case(conv),
            field.name.snake_case(conv)
        )?;
    }
    if struc.fields.is_empty() {
        writeln!(writer, "    pass")?;
    }

    // Reading
    writeln!(writer, "@staticmethod")?;
    writeln!(writer, "def read_from(stream):")?;
    writer.inc_ident();
    for field in &struc.fields {
        fn assign(writer: &mut Writer, to: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "{} = stream.read_bool()", to)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "{} = stream.read_int()", to)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "{} = stream.read_long()", to)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "{} = stream.read_float()", to)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "{} = stream.read_double()", to)?;
                }
                Schema::String => {
                    writeln!(writer, "{} = stream.read_string()", to)?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => {
                    writeln!(
                        writer,
                        "{} = {}.read_from(stream)",
                        to,
                        name.camel_case(conv)
                    )?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if stream.read_bool():")?;
                    writer.inc_ident();
                    assign(writer, to, inner)?;
                    writer.dec_ident();
                    writeln!(writer, "else:")?;
                    writeln!(writer, "    {} = None", to)?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "{} = []", to)?;
                    writeln!(writer, "for _ in range(stream.read_int()):")?;
                    writer.inc_ident();
                    assign(writer, &format!("{}_element", to), inner)?;
                    writeln!(writer, "{}.append({}_element)", to, to)?;
                    writer.dec_ident();
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "{} = {{}}", to)?;
                    writeln!(writer, "for _ in range(stream.read_int()):")?;
                    writer.inc_ident();
                    assign(writer, &format!("{}_key", to), key_type)?;
                    assign(writer, &format!("{}_value", to), value_type)?;
                    writeln!(writer, "{}[{}_key] = {}_value", to, to, to)?;
                    writer.dec_ident();
                }
                Schema::Enum { base_name, .. } => {
                    writeln!(
                        writer,
                        "{} = {}(stream.read_int())",
                        to,
                        base_name.camel_case(conv)
                    )?;
                }
            }
            Ok(())
        }
        assign(writer, &field.name.snake_case(conv), &field.schema)?;
    }
    write!(writer, "return {}(", struc.name.camel_case(conv))?;
    let mut first = true;
    for field in &struc.fields {
        if first {
            first = false;
        } else {
            write!(writer, ", ")?;
        }
        write!(writer, "{}", field.name.snake_case(conv))?;
    }
    writeln!(writer, ")")?;
    writer.dec_ident();

    // Writing
    writeln!(writer, "def write_to(self, stream):")?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(writer, "stream.write_int(self.TAG)")?;
    } else if struc.fields.is_empty() && struc.magic.is_none() {
        writeln!(writer, "pass")?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "stream.write_int({})", magic)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "stream.write_bool({})", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "stream.write_int({})", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "stream.write_long({})", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "stream.write_float({})", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "stream.write_double({})", value)?;
                }
                Schema::String => {
                    writeln!(writer, "stream.write_string({})", value)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "{}.write_to(stream)", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if {} is None:", value)?;
                    writeln!(writer, "    stream.write_bool(False)")?;
                    writeln!(writer, "else:")?;
                    writer.inc_ident();
                    writeln!(writer, "stream.write_bool(True)")?;
                    write(writer, value, inner)?;
                    writer.dec_ident();
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "stream.write_int(len({}))", value)?;
                    writeln!(writer, "for element in {}:", value)?;
                    writer.inc_ident();
                    write(writer, "element", inner)?;
                    writer.dec_ident();
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "stream.write_int(len({}))", value)?;
                    writeln!(writer, "for key, value in {}.items():", value)?;
                    writer.inc_ident();
                    write(writer, "key", key_type)?;
                    write(writer, "value", value_type)?;
                    writer.dec_ident();
                }
                Schema::Enum { .. } => {
                    writeln!(writer, "stream.write_int({})", value)?;
                }
            }
            Ok(())
        }
        write(
            writer,
            &format!("self.{}", field.name.snake_case(conv)),
            &field.schema,
        )?;
    }
    writer.dec_ident();

    // Repr
    writeln!(writer, "def __repr__(self):")?;
    writer.inc_ident();
    writeln!(writer, "return \"{}(\" + \\", struc.name.camel_case(conv))?;
    for (index, field) in struc.fields.iter().enumerate() {
        write!(writer, "    repr(self.{})", field.name.snake_case(conv))?;
        if index + 1 < struc.fields.len() {
            write!(writer, " + \",\"")?;
        }
        writeln!(writer, " + \\")?;
    }
    writeln!(writer, "    \")\"")?;
    writer.dec_ident();

    writer.dec_ident();

    if let Some((base_name, _)) = base {
        writeln!(
            writer,
            "{}.{} = {}",
            base_name.camel_case(conv),
            struc.name.camel_case(conv),
            struc.name.camel_case(conv)
        )?;
    }

    Ok(())
}

impl crate::Generator for Generator {
    const NAME: &'static str = "Python";
    type Options = ();
    fn new(_name: &str, _version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "stream_wrapper.py".to_owned(),
            include_str!("stream_wrapper.py").to_owned(),
        );
        Self {
            model_init: String::new(),
            files,
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        if !self.model_init.is_empty() {
            self.files
                .insert("model/__init__.py".to_owned(), self.model_init);
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
                let file_name = format!("model/{}.py", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "from enum import IntEnum").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "class {}(IntEnum):", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for (index, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "{} = {}",
                        variant.name.shouty_snake_case(conv),
                        index
                    )
                    .unwrap();
                }
                writer.dec_ident();
                writeln!(
                    &mut self.model_init,
                    "from .{} import {}",
                    base_name.snake_case(conv),
                    base_name.camel_case(conv),
                )
                .unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                let file_name = format!("model/{}.py", struc.name.snake_case(conv));
                let mut writer = Writer::new();
                write_struct(&mut writer, struc, None).unwrap();
                writeln!(
                    &mut self.model_init,
                    "from .{} import {}",
                    struc.name.snake_case(conv),
                    struc.name.camel_case(conv)
                )
                .unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.py", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(&mut writer, "class {}:", base_name.camel_case(conv)).unwrap();
                {
                    writer.inc_ident();
                    writeln!(&mut writer, "@staticmethod").unwrap();
                    writeln!(&mut writer, "def read_from(stream):").unwrap();
                    {
                        writer.inc_ident();
                        writeln!(&mut writer, "tag = stream.read_int()").unwrap();
                        for variant in variants {
                            writeln!(
                                &mut writer,
                                "if tag == {}.TAG:",
                                variant.name.camel_case(conv)
                            )
                            .unwrap();
                            writeln!(
                                &mut writer,
                                "    return {}.{}.read_from(stream)",
                                base_name.camel_case(conv),
                                variant.name.camel_case(conv),
                            )
                            .unwrap();
                        }
                        writeln!(&mut writer, "raise Exception(\"Unexpected tag value\")").unwrap();
                        writer.dec_ident();
                    }
                    writer.dec_ident();
                }
                writeln!(&mut writer).unwrap();
                for (tag, variant) in variants.iter().enumerate() {
                    write_struct(&mut writer, variant, Some((base_name, tag))).unwrap();
                }
                writeln!(
                    &mut self.model_init,
                    "from .{} import {}",
                    base_name.snake_case(conv),
                    base_name.camel_case(conv)
                )
                .unwrap();
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

impl RunnableGenerator for Generator {
    fn build_local(path: &Path) -> anyhow::Result<()> {
        Ok(())
    }
    fn run_local(path: &Path) -> anyhow::Result<Command> {
        let mut command = command(if cfg!(windows) { "py -3" } else { "python3" });
        command.arg("main.py").current_dir(path);
        Ok(command)
    }
}

impl testing::FileReadWrite for Generator {
    fn extra_files(schema: &Schema) -> Vec<File> {
        fn type_name(schema: &Schema) -> String {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => name.camel_case(conv),
                _ => unreachable!(),
            }
        }
        vec![File {
            path: "main.py".to_owned(),
            content: include_templing!("src/gens/python/file_read_write.py.templing"),
        }]
    }
}
