use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
    index_file: String,
}

fn index_var_name(index_var: &mut usize) -> String {
    let result = "ijk".chars().nth(*index_var).unwrap();
    *index_var += 1;
    result.to_string()
}

fn write_imports(writer: &mut Writer, schema: &Schema) -> std::fmt::Result {
    fn write_imports_struct(
        writer: &mut Writer,
        struc: &Struct,
        added: &mut HashSet<String>,
    ) -> std::fmt::Result {
        for field in &struc.fields {
            fn add_imports(
                writer: &mut Writer,
                schema: &Schema,
                added: &mut HashSet<String>,
            ) -> std::fmt::Result {
                match schema {
                    Schema::Struct(Struct { name, .. })
                    | Schema::OneOf {
                        base_name: name, ..
                    } => {
                        if added.insert(file_name(name)) {
                            writeln!(
                                writer,
                                "const {} = require('./{}');",
                                name.camel_case(conv),
                                file_name(name),
                            )?;
                        }
                    }
                    Schema::Option(inner) => {
                        add_imports(writer, inner, added)?;
                    }
                    Schema::Vec(inner) => {
                        add_imports(writer, inner, added)?;
                    }
                    Schema::Map(key_type, value_type) => {
                        add_imports(writer, key_type, added)?;
                        add_imports(writer, value_type, added)?;
                    }
                    Schema::Enum { .. } => {}
                    Schema::Bool
                    | Schema::Int32
                    | Schema::Int64
                    | Schema::Float32
                    | Schema::Float64
                    | Schema::String => {}
                }
                Ok(())
            }
            add_imports(writer, &field.schema, added)?;
        }
        Ok(())
    }
    let mut added = HashSet::new();
    match schema {
        Schema::Struct(struc) => {
            write_imports_struct(writer, struc, &mut added)?;
        }
        Schema::OneOf { variants, .. } => {
            for variant in variants {
                write_imports_struct(writer, variant, &mut added)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn write_struct(
    writer: &mut Writer,
    struc: &Struct,
    base: Option<(&Name, usize)>,
) -> std::fmt::Result {
    // Class
    write!(writer, "class {}", struc.name.camel_case(conv))?;
    if let Some((base_name, _)) = base {
        write!(writer, " extends {}", base_name.camel_case(conv))?;
    }
    writeln!(writer, " {{")?;
    writer.inc_ident();

    // Constructor
    write!(writer, "constructor(")?;
    for (index, field) in struc.fields.iter().enumerate() {
        write!(writer, "{}", field.name.mixed_case(conv))?;
        if index + 1 < struc.fields.len() {
            write!(writer, ", ")?;
        }
    }
    writeln!(writer, ") {{")?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(writer, "super();")?;
    }
    for field in &struc.fields {
        writeln!(
            writer,
            "this.{} = {};",
            field.name.mixed_case(conv),
            field.name.mixed_case(conv)
        )?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // Reading
    writeln!(writer, "static async readFrom(stream) {{")?;
    writer.inc_ident();
    for field in &struc.fields {
        fn assign(
            writer: &mut Writer,
            to: &str,
            schema: &Schema,
            index_var: &mut usize,
        ) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "{} = await stream.readBool();", to)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "{} = await stream.readInt();", to)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "{} = await stream.readLong();", to)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "{} = await stream.readFloat();", to)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "{} = await stream.readDouble();", to)?;
                }
                Schema::String => {
                    writeln!(writer, "{} = await stream.readString();", to)?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => {
                    writeln!(
                        writer,
                        "{} = await {}.readFrom(stream);",
                        to,
                        name.camel_case(conv)
                    )?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if (await stream.readBool()) {{")?;
                    writer.inc_ident();
                    assign(writer, to, inner, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}} else {{")?;
                    writeln!(writer, "    {} = null;", to)?;
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "{} = [];", to)?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for (let {} = await stream.readInt(); {} > 0; {}--) {{",
                        index_var_name, index_var_name, index_var_name,
                    )?;
                    writer.inc_ident();
                    writeln!(writer, "let {}Element;", to)?;
                    assign(writer, &format!("{}Element", to), inner, index_var)?;
                    writeln!(writer, "{}.push({}Element);", to, to)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "{} = new Map();", to)?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(
                        writer,
                        "for (let {} = await stream.readInt(); {} > 0; {}--) {{",
                        index_var_name, index_var_name, index_var_name,
                    )?;
                    writer.inc_ident();
                    writeln!(writer, "let {}Key;", to)?;
                    writeln!(writer, "let {}Value;", to)?;
                    assign(writer, &format!("{}Key", to), key_type, index_var)?;
                    assign(writer, &format!("{}Value", to), value_type, index_var)?;
                    writeln!(writer, "{}.set({}Key, {}Value);", to, to, to)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum { base_name, .. } => {
                    writeln!(writer, "{} = await stream.readInt();", to)?;
                }
            }
            Ok(())
        }
        writeln!(writer, "let {};", field.name.mixed_case(conv))?;
        assign(writer, &field.name.mixed_case(conv), &field.schema, &mut 0)?;
    }
    write!(writer, "return new {}(", struc.name.camel_case(conv))?;
    let mut first = true;
    for field in &struc.fields {
        if first {
            first = false;
        } else {
            write!(writer, ", ")?;
        }
        write!(writer, "{}", field.name.mixed_case(conv))?;
    }
    writeln!(writer, ");")?;
    writer.dec_ident();
    writeln!(writer, "}}")?;

    // Writing
    writeln!(writer, "async writeTo(stream) {{")?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(
            writer,
            "await stream.writeInt({}.TAG);",
            struc.name.camel_case(conv),
        )?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "await stream.writeInt({});", magic)?;
    }
    for field in &struc.fields {
        fn write(
            writer: &mut Writer,
            value: &str,
            schema: &Schema,
            index_var: &mut usize,
        ) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "await stream.writeBool({});", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "await stream.writeInt({});", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "await stream.writeLong({});", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "await stream.writeFloat({});", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "await stream.writeDouble({});", value)?;
                }
                Schema::String => {
                    writeln!(writer, "await stream.writeString({});", value)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "await {}.writeTo(stream);", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if ({} === null) {{", value)?;
                    writeln!(writer, "    await stream.writeBool(false);")?;
                    writeln!(writer, "}} else {{")?;
                    writer.inc_ident();
                    writeln!(writer, "await stream.writeBool(true);")?;
                    write(writer, value, inner, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "await stream.writeInt({}.length);", value)?;
                    writeln!(writer, "for (let {}Element of {}) {{", value, value,)?;
                    writer.inc_ident();
                    write(writer, &format!("{}Element", value), inner, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "await stream.writeInt({}.size);", value)?;
                    let index_var_name = index_var_name(index_var);
                    writeln!(writer, "for (let {}Entry of {}) {{", value, value)?;
                    writer.inc_ident();
                    writeln!(writer, "let {}Key = {}Entry[0];", value, value)?;
                    writeln!(writer, "let {}Value = {}Entry[1];", value, value)?;
                    write(writer, &format!("{}Key", value), key_type, index_var)?;
                    write(writer, &format!("{}Value", value), value_type, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum { .. } => {
                    writeln!(writer, "await stream.writeInt({});", value)?;
                }
            }
            Ok(())
        }
        writeln!(
            writer,
            "let {} = this.{};",
            field.name.mixed_case(conv),
            field.name.mixed_case(conv),
        )?;
        write(writer, &field.name.mixed_case(conv), &field.schema, &mut 0)?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;

    writer.dec_ident();
    writeln!(writer, "}}")?;

    if let Some((base_name, _)) = base {
        writeln!(
            writer,
            "{}.{} = {};",
            base_name.camel_case(conv),
            struc.name.camel_case(conv),
            struc.name.camel_case(conv)
        )?;
    }

    if let Some((_, tag)) = base {
        writeln!(writer, "{}.TAG = {};", struc.name.camel_case(conv), tag,)?;
    }

    Ok(())
}

fn file_name(name: &Name) -> String {
    name.snake_case(conv).replace('_', "-")
}

impl crate::Generator for Generator {
    fn new(name: &str, version: &str) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "stream-wrapper.js".to_owned(),
            include_str!("stream-wrapper.js").to_owned(),
        );
        Self {
            files,
            index_file: String::new(),
        }
    }
    fn result(mut self) -> GenResult {
        self.files
            .insert("model/index.js".to_owned(), self.index_file);
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                base_name,
                variants,
            } => {
                writeln!(
                    self.index_file,
                    "module.exports.{} = require('./{}');",
                    base_name.camel_case(conv),
                    file_name(base_name),
                )
                .unwrap();
                let file_name = format!("model/{}.js", file_name(base_name));
                let mut writer = Writer::new();
                writeln!(writer, "module.exports = {{").unwrap();
                writer.inc_ident();
                for (index, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "{} : {}{}",
                        variant.camel_case(conv),
                        index,
                        if index + 1 < variants.len() { "," } else { "" }
                    )
                    .unwrap();
                }
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                writeln!(
                    self.index_file,
                    "module.exports.{} = require('./{}');",
                    struc.name.camel_case(conv),
                    file_name(&struc.name),
                )
                .unwrap();
                let file_name = format!("model/{}.js", file_name(&struc.name));
                let mut writer = Writer::new();
                write_imports(&mut writer, schema).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                writeln!(writer, "module.exports = {}", struc.name.camel_case(conv)).unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                base_name,
                variants,
            } => {
                writeln!(
                    self.index_file,
                    "module.exports.{} = require('./{}');",
                    base_name.camel_case(conv),
                    file_name(base_name),
                )
                .unwrap();
                let file_name = format!("model/{}.js", file_name(base_name));
                let mut writer = Writer::new();
                write_imports(&mut writer, schema).unwrap();
                writeln!(&mut writer, "class {} {{", base_name.camel_case(conv)).unwrap();
                {
                    writer.inc_ident();
                    writeln!(&mut writer, "static async readFrom(stream) {{").unwrap();
                    writer.inc_ident();
                    writeln!(&mut writer, "let tag = await stream.readInt();").unwrap();
                    for variant in variants {
                        writeln!(
                            &mut writer,
                            "if (tag == {}.TAG) {{",
                            variant.name.camel_case(conv)
                        )
                        .unwrap();
                        writeln!(
                            &mut writer,
                            "    return await {}.readFrom(stream);",
                            variant.name.camel_case(conv),
                        )
                        .unwrap();
                        writeln!(&mut writer, "}}").unwrap();
                    }
                    writeln!(&mut writer, "throw new Error(\"Unexpected tag value\");").unwrap();
                    writer.dec_ident();
                    writeln!(writer, "}}").unwrap();
                }
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                writeln!(&mut writer).unwrap();
                for (tag, variant) in variants.iter().enumerate() {
                    write_struct(&mut writer, variant, Some((base_name, tag))).unwrap();
                }
                writeln!(writer, "module.exports = {};", base_name.camel_case(conv)).unwrap();
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
