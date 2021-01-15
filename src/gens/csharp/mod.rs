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

fn is_null(value: &str, schema: &Schema) -> String {
    if nullable(schema) {
        format!("{} == null", value)
    } else {
        format!("!{}.HasValue", value)
    }
}

fn option_unwrap(name: &str, schema: &Schema) -> String {
    if nullable(schema) {
        name.to_owned()
    } else {
        format!("{}.Value", name)
    }
}

fn nullable(schema: &Schema) -> bool {
    match schema {
        Schema::Bool
        | Schema::Int32
        | Schema::Int64
        | Schema::Float32
        | Schema::Float64
        | Schema::Enum { .. }
        | Schema::Struct(_) => false,
        Schema::String
        | Schema::Option(_)
        | Schema::Vec(_)
        | Schema::Map(_, _)
        | Schema::OneOf { .. } => true,
    }
}

fn type_name(schema: &Schema) -> String {
    format!("{}{}", type_name_prearray(schema), type_post_array(schema))
}

fn type_name_prearray(schema: &Schema) -> String {
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
        } => format!("Model.{}", name.camel_case(conv)),
        Schema::Option(inner) => {
            if nullable(inner) {
                type_name(inner)
            } else {
                format!("{}?", type_name(inner))
            }
        }
        Schema::Vec(inner) => type_name_prearray(inner),
        Schema::Map(key, value) => format!(
            "System.Collections.Generic.IDictionary<{}, {}>",
            type_name(key),
            type_name(value)
        ),
    }
}

fn type_post_array(schema: &Schema) -> String {
    match schema {
        Schema::Vec(inner) => format!("[]{}", type_post_array(inner)),
        _ => String::new(),
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

fn write_struct(
    writer: &mut Writer,
    struc: &Struct,
    base: Option<(&Name, usize)>,
) -> std::fmt::Result {
    // Class
    if let Some((base_name, _)) = base {
        writeln!(
            writer,
            "public class {} : {}",
            struc.name.camel_case(conv),
            base_name.camel_case(conv)
        )?;
    } else {
        writeln!(writer, "public struct {}", struc.name.camel_case(conv))?;
    }
    writeln!(writer, "{{")?;
    writer.inc_ident();
    if let Some((_, tag)) = base {
        writeln!(writer, "public const int TAG = {};", tag)?;
    }

    // Fields
    for field in &struc.fields {
        writeln!(
            writer,
            "public {} {} {{ get; set; }}",
            type_name(&field.schema),
            field.name.camel_case(conv)
        )?;
    }

    // Constructor
    if base.is_some() {
        writeln!(writer, "public {}() {{}}", struc.name.camel_case(conv))?;
    }
    if !struc.fields.is_empty() {
        write!(writer, "public {}(", struc.name.camel_case(conv))?;
        for (index, field) in struc.fields.iter().enumerate() {
            if index > 0 {
                write!(writer, ", ")?;
            }
            write!(
                writer,
                "{} {}",
                type_name(&field.schema),
                field.name.mixed_case(conv)
            )?;
        }
        writeln!(writer, ")")?;
        writeln!(writer, "{{")?;
        for field in &struc.fields {
            writeln!(
                writer,
                "    this.{} = {};",
                field.name.camel_case(conv),
                field.name.mixed_case(conv)
            )?;
        }
        writeln!(writer, "}}")?;
    }

    // Reading
    writeln!(
        writer,
        "public static {}{} ReadFrom(System.IO.BinaryReader reader)",
        if base.is_some() { "new " } else { "" },
        struc.name.camel_case(conv)
    )?;
    writeln!(writer, "{{")?;
    writer.inc_ident();
    writeln!(
        writer,
        "var result = new {}();",
        struc.name.camel_case(conv)
    )?;
    for field in &struc.fields {
        fn assign(
            writer: &mut Writer,
            to: &str,
            schema: &Schema,
            index_var: &mut usize,
        ) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "{} = reader.ReadBoolean();", to)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "{} = reader.ReadInt32();", to)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "{} = reader.ReadInt64();", to)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "{} = reader.ReadSingle();", to)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "{} = reader.ReadDouble();", to)?;
                }
                Schema::String => {
                    writeln!(writer, "{} = System.Text.Encoding.UTF8.GetString(reader.ReadBytes(reader.ReadInt32()));", to)?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => {
                    writeln!(
                        writer,
                        "{} = Model.{}.ReadFrom(reader);",
                        to,
                        name.camel_case(conv)
                    )?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if (reader.ReadBoolean())")?;
                    writeln!(writer, "{{")?;
                    writer.inc_ident();
                    assign(writer, to, inner, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}} else")?;
                    writeln!(writer, "{{")?;
                    writeln!(writer, "    {} = null;", to)?;
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(
                        writer,
                        "{} = new {}[reader.ReadInt32()]{};",
                        to,
                        type_name_prearray(inner),
                        type_post_array(inner)
                    )?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for (int {} = 0; {} < {}.Length; {}++)",
                        index_var_name, index_var_name, to, index_var_name
                    )?;
                    writeln!(writer, "{{")?;
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
                    writeln!(writer, "int {} = reader.ReadInt32();", to_size)?;
                    writeln!(
                        writer,
                        "{} = new System.Collections.Generic.Dictionary<{}, {}>({});",
                        to,
                        type_name(key_type),
                        type_name(value_type),
                        to_size
                    )?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for (int {} = 0; {} < {}; {}++)",
                        index_var_name, index_var_name, to_size, index_var_name
                    )?;
                    writeln!(writer, "{{")?;
                    writer.inc_ident();
                    writeln!(writer, "{} {}Key;", type_name(key_type), var_name(to))?;
                    assign(writer, &format!("{}Key", var_name(to)), key_type, index_var)?;
                    writeln!(writer, "{} {}Value;", type_name(value_type), var_name(to))?;
                    assign(
                        writer,
                        &format!("{}Value", var_name(to)),
                        value_type,
                        index_var,
                    )?;
                    writeln!(
                        writer,
                        "{}.Add({}Key, {}Value);",
                        to,
                        var_name(to),
                        var_name(to)
                    )?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum {
                    documentation: _,
                    base_name,
                    variants,
                } => {
                    writeln!(writer, "switch (reader.ReadInt32())")?;
                    writeln!(writer, "{{")?;
                    for (tag, variant) in variants.iter().enumerate() {
                        writeln!(writer, "case {}:", tag)?;
                        writeln!(
                            writer,
                            "    {} = Model.{}.{};",
                            to,
                            base_name.camel_case(conv),
                            variant.name.camel_case(conv)
                        )?;
                        writeln!(writer, "    break;")?;
                    }
                    writeln!(writer, "default:")?;
                    writeln!(
                        writer,
                        "    throw new System.Exception(\"Unexpected tag value\");"
                    )?;
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
    writeln!(writer, "return result;")?;
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // Writing
    writeln!(
        writer,
        "public {}void WriteTo(System.IO.BinaryWriter writer)",
        if base.is_some() { "override " } else { "" }
    )?;
    writeln!(writer, "{{")?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(writer, "writer.Write(TAG);")?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "writer.Write({});", magic)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "writer.Write({});", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "writer.Write({});", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "writer.Write({});", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "writer.Write({});", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "writer.Write({});", value)?;
                }
                Schema::String => {
                    let data_var = format!("{}Data", var_name(value));
                    writeln!(
                        writer,
                        "var {} = System.Text.Encoding.UTF8.GetBytes({});",
                        data_var, value
                    )?;
                    writeln!(writer, "writer.Write({}.Length);", data_var)?;
                    writeln!(writer, "writer.Write({});", data_var)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "{}.WriteTo(writer);", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if ({})", is_null(value, inner))?;
                    writeln!(writer, "{{")?;
                    writeln!(writer, "    writer.Write(false);")?;
                    writeln!(writer, "}} else")?;
                    writeln!(writer, "{{")?;
                    writer.inc_ident();
                    writeln!(writer, "writer.Write(true);")?;
                    write(writer, &option_unwrap(value, inner), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "writer.Write({}.Length);", value)?;
                    writeln!(
                        writer,
                        "foreach (var {}Element in {})",
                        var_name(value),
                        value
                    )?;
                    writeln!(writer, "{{")?;
                    writer.inc_ident();
                    write(writer, &format!("{}Element", var_name(value)), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "writer.Write({}.Count);", value)?;
                    writeln!(
                        writer,
                        "foreach (var {}Entry in {})",
                        var_name(value),
                        value
                    )?;
                    writeln!(writer, "{{")?;
                    writer.inc_ident();
                    writeln!(
                        writer,
                        "var {}Key = {}Entry.Key;",
                        var_name(value),
                        var_name(value)
                    )?;
                    writeln!(
                        writer,
                        "var {}Value = {}Entry.Value;",
                        var_name(value),
                        var_name(value)
                    )?;
                    write(writer, &format!("{}Key", var_name(value)), key_type)?;
                    write(writer, &format!("{}Value", var_name(value)), value_type)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum { .. } => {
                    writeln!(writer, "writer.Write((int) ({}));", value)?;
                }
            }
            Ok(())
        }
        write(writer, &field.name.camel_case(conv), &field.schema)?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;
    writer.dec_ident();
    writeln!(writer, "}}")?;
    Ok(())
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
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("Model/{}.cs", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "namespace {}.Model", self.main_namespace).unwrap();
                writeln!(writer, "{{").unwrap();
                writer.inc_ident();
                writeln!(writer, "public enum {}", base_name.camel_case(conv)).unwrap();
                writeln!(writer, "{{").unwrap();
                writer.inc_ident();
                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(writer, "{} = {},", variant.name.camel_case(conv), tag,).unwrap();
                }
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                let file_name = format!("Model/{}.cs", struc.name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "namespace {}.Model", self.main_namespace).unwrap();
                writeln!(writer, "{{").unwrap();
                writer.inc_ident();
                write_struct(&mut writer, struc, None).unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("Model/{}.cs", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "namespace {}.Model", self.main_namespace).unwrap();
                writeln!(writer, "{{").unwrap();
                writer.inc_ident();
                writeln!(
                    &mut writer,
                    "public abstract class {}",
                    base_name.camel_case(conv)
                )
                .unwrap();
                writeln!(writer, "{{").unwrap();
                {
                    writer.inc_ident();
                    writeln!(
                        &mut writer,
                        "public abstract void WriteTo(System.IO.BinaryWriter writer);"
                    )
                    .unwrap();
                    writeln!(
                        &mut writer,
                        "public static {} ReadFrom(System.IO.BinaryReader reader)",
                        base_name.camel_case(conv)
                    )
                    .unwrap();
                    writeln!(writer, "{{").unwrap();
                    {
                        writer.inc_ident();
                        writeln!(&mut writer, "switch (reader.ReadInt32())").unwrap();
                        writeln!(writer, "{{").unwrap();
                        writer.inc_ident();
                        for variant in variants {
                            writeln!(&mut writer, "case {}.TAG:", variant.name.camel_case(conv))
                                .unwrap();
                            writeln!(
                                &mut writer,
                                "    return {}.ReadFrom(reader);",
                                variant.name.camel_case(conv)
                            )
                            .unwrap();
                        }
                        writeln!(&mut writer, "default:").unwrap();
                        writeln!(
                            &mut writer,
                            "    throw new System.Exception(\"Unexpected tag value\");"
                        )
                        .unwrap();
                        writer.dec_ident();
                        writeln!(&mut writer, "}}").unwrap();
                        writer.dec_ident();
                    }
                    writeln!(&mut writer, "}}").unwrap();
                    for (tag, variant) in variants.iter().enumerate() {
                        writeln!(&mut writer).unwrap();
                        write_struct(&mut writer, variant, Some((base_name, tag))).unwrap();
                    }
                    writer.dec_ident();
                }
                writeln!(&mut writer, "}}").unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
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
        command("dotnet")
            .current_dir(path)
            .arg("publish")
            .arg("-c")
            .arg("Release")
            .arg("-o")
            .arg(".")
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

impl testing::FileReadWrite for Generator {
    fn extra_files(schema: &Schema) -> Vec<File> {
        vec![File {
            path: "Runner.cs".to_owned(),
            content: include_templing!("src/gens/csharp/FileReadWrite.cs.templing"),
        }]
    }
}
