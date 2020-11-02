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
            "static class {} : {} {{",
            struc.name.camel_case(conv),
            base_name.camel_case(conv)
        )?;
    } else {
        writeln!(writer, "struct {} {{", struc.name.camel_case(conv))?;
    }
    writer.inc_ident();
    if let Some((_, tag)) = base {
        writeln!(writer, "static const int TAG = {};", tag)?;
    }

    // Fields
    for field in &struc.fields {
        writeln!(
            writer,
            "{} {};",
            type_name(&field.schema),
            field.name.mixed_case(conv)
        )?;
    }

    // Constructor
    if base.is_some() {
        writeln!(writer, "this() {{}}")?;
    }
    if !struc.fields.is_empty() {
        write!(writer, "this(")?;
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
        writeln!(writer, ") {{")?;
        for field in &struc.fields {
            writeln!(
                writer,
                "    this.{} = {};",
                field.name.mixed_case(conv),
                field.name.mixed_case(conv)
            )?;
        }
        writeln!(writer, "}}")?;
    }

    // Reading
    writeln!(
        writer,
        "static {} readFrom(Stream reader) {{",
        struc.name.camel_case(conv)
    )?;
    writer.inc_ident();
    writeln!(
        writer,
        "auto result = {}{}();",
        if base.is_some() { "new " } else { "" },
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
                    writeln!(writer, "{} = reader.readBool();", to)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "{} = reader.readInt();", to)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "{} = reader.readLong();", to)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "{} = reader.readFloat();", to)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "{} = reader.readDouble();", to)?;
                }
                Schema::String => {
                    writeln!(writer, "{} = reader.readString();", to)?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => {
                    writeln!(
                        writer,
                        "{} = {}.readFrom(reader);",
                        to,
                        name.camel_case(conv)
                    )?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if (reader.readBool()) {{")?;
                    writer.inc_ident();
                    assign(writer, to, inner, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}} else {{")?;
                    writeln!(writer, "    {}.nullify();", to)?;
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(
                        writer,
                        "{} = new {}[reader.readInt()];",
                        to,
                        type_name(inner),
                    )?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for (int {} = 0; {} < {}.length; {}++) {{",
                        index_var_name, index_var_name, to, index_var_name
                    )?;
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
                    writeln!(writer, "int {} = reader.readInt();", to_size)?;
                    writeln!(writer, "{}.clear();", to)?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for (int {} = 0; {} < {}; {}++) {{",
                        index_var_name, index_var_name, to_size, index_var_name
                    )?;
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
                        "{}[{}Key] = {}Value;",
                        to,
                        var_name(to),
                        var_name(to)
                    )?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum {
                    documentation,
                    base_name,
                    variants,
                } => {
                    writeln!(writer, "switch (reader.readInt()) {{")?;
                    for (tag, variant) in variants.iter().enumerate() {
                        writeln!(writer, "case {}:", tag)?;
                        writeln!(
                            writer,
                            "    {} = {}.{};",
                            to,
                            base_name.camel_case(conv),
                            variant.name.camel_case(conv)
                        )?;
                        writeln!(writer, "    break;")?;
                    }
                    writeln!(writer, "default:")?;
                    writeln!(writer, "    throw new Exception(\"Unexpected tag value\");")?;
                    writeln!(writer, "}}")?;
                }
            }
            Ok(())
        }
        assign(
            writer,
            &format!("result.{}", field.name.mixed_case(conv)),
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
        "{}void writeTo(Stream writer) const {{",
        if base.is_some() { "override " } else { "" }
    )?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(writer, "writer.write(TAG);")?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "writer.write({});", magic)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "writer.write({});", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "writer.write({});", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "writer.write({});", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "writer.write({});", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "writer.write({});", value)?;
                }
                Schema::String => {
                    writeln!(writer, "writer.write({});", value)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "{}.writeTo(writer);", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if ({}.isNull()) {{", value)?;
                    writeln!(writer, "    writer.write(false);")?;
                    writeln!(writer, "}} else {{")?;
                    writer.inc_ident();
                    writeln!(writer, "writer.write(true);")?;
                    write(writer, &format!("{}.get", value), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "writer.write(cast(int)({}.length));", value)?;
                    writeln!(writer, "foreach ({}Element; {}) {{", var_name(value), value)?;
                    writer.inc_ident();
                    write(writer, &format!("{}Element", var_name(value)), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "writer.write(cast(int)({}.length));", value)?;
                    writeln!(
                        writer,
                        "foreach ({}Key, {}Value; {}) {{",
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
                    writeln!(writer, "writer.write(cast(int)({}));", value)?;
                }
            }
            Ok(())
        }
        write(writer, &field.name.mixed_case(conv), &field.schema)?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // ToString
    writeln!(
        writer,
        "{}string toString() const {{",
        if base.is_some() { "override " } else { "" }
    )?;
    writer.inc_ident();
    writeln!(writer, "return {:?} ~ \"(\" ~", struc.name.camel_case(conv))?;
    writer.inc_ident();
    for field in &struc.fields {
        writeln!(writer, "to!string({}) ~", field.name.mixed_case(conv))?;
    }
    writeln!(writer, "\")\";")?;
    writer.dec_ident();
    writer.dec_ident();
    writeln!(writer, "}}")?;

    writer.dec_ident();
    writeln!(writer, "}}")?;
    Ok(())
}

impl crate::Generator for Generator {
    type Options = ();
    fn new(name: &str, version: &str, _: ()) -> Self {
        let mut files = HashMap::new();
        files.insert("stream.d".to_owned(), include_str!("stream.d").to_owned());
        Self {
            model_init: String::new(),
            files,
        }
    }
    fn result(mut self) -> GenResult {
        if !self.model_init.is_empty() {
            self.files
                .insert("model/package.d".to_owned(), self.model_init);
        }
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                documentation,
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.d", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "enum {} : int {{", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(writer, "{} = {},", variant.name.camel_case(conv), tag,).unwrap();
                }
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
                writeln!(
                    &mut self.model_init,
                    "public import {};",
                    base_name.snake_case(conv),
                )
                .unwrap();
            }
            Schema::Struct(struc) => {
                let file_name = format!("model/{}.d", struc.name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "import model;").unwrap();
                writeln!(writer, "import stream;").unwrap();
                writeln!(writer, "import std.conv;").unwrap();
                writeln!(writer, "import std.typecons : Nullable;").unwrap();
                writeln!(writer).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                self.files.insert(file_name, writer.get());
                writeln!(
                    &mut self.model_init,
                    "public import {};",
                    struc.name.snake_case(conv),
                )
                .unwrap();
            }
            Schema::OneOf {
                documentation,
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.d", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "import model;").unwrap();
                writeln!(writer, "import stream;").unwrap();
                writeln!(writer, "import std.conv;").unwrap();
                writeln!(writer, "import std.typecons : Nullable;").unwrap();
                writeln!(writer).unwrap();
                writeln!(
                    &mut writer,
                    "abstract class {} {{",
                    base_name.camel_case(conv)
                )
                .unwrap();
                {
                    writer.inc_ident();
                    writeln!(&mut writer, "abstract void writeTo(Stream writer) const;").unwrap();
                    writeln!(
                        &mut writer,
                        "static {} readFrom(Stream reader) {{",
                        base_name.camel_case(conv)
                    )
                    .unwrap();
                    {
                        writer.inc_ident();
                        writeln!(&mut writer, "switch (reader.readInt()) {{").unwrap();
                        writer.inc_ident();
                        for variant in variants {
                            writeln!(&mut writer, "case {}.TAG:", variant.name.camel_case(conv))
                                .unwrap();
                            writeln!(
                                &mut writer,
                                "    return {}.readFrom(reader);",
                                variant.name.camel_case(conv)
                            )
                            .unwrap();
                        }
                        writeln!(&mut writer, "default:").unwrap();
                        writeln!(
                            &mut writer,
                            "    throw new Exception(\"Unexpected tag value\");"
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
                self.files.insert(file_name, writer.get());
                writeln!(
                    &mut self.model_init,
                    "public import {};",
                    base_name.snake_case(conv),
                )
                .unwrap();
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
