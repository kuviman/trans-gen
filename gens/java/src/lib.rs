use std::collections::HashMap;
use std::fmt::Write;
use trans_gen_core::trans_schema::*;
use trans_gen_core::Writer;

fn conv(name: &str) -> String {
    name.replace("Bool", "Boolean")
        .replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Float")
        .replace("Float64", "Double")
}

pub struct Generator {
    files: HashMap<String, String>,
}

fn type_name(schema: &Schema) -> String {
    format!("{}{}", type_name_prearray(schema), type_post_array(schema))
}

fn type_name_obj(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "Boolean".to_owned(),
        Schema::Int32 => "Integer".to_owned(),
        Schema::Int64 => "Long".to_owned(),
        Schema::Float32 => "Float".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        _ => type_name(schema),
    }
}

fn type_name_prearray_obj(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "Boolean".to_owned(),
        Schema::Int32 => "Integer".to_owned(),
        Schema::Int64 => "Long".to_owned(),
        Schema::Float32 => "Float".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        _ => type_name_prearray(schema),
    }
}

fn type_name_prearray(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "boolean".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "long".to_owned(),
        Schema::Float32 => "float".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => format!("model.{}", name.camel_case(conv)),
        Schema::Option(inner) => type_name_obj(inner),
        Schema::Vec(inner) => type_name_prearray_obj(inner),
        Schema::Map(key, value) => format!(
            "java.util.Map<{}, {}>",
            type_name_obj(key),
            type_name_obj(value)
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

fn getter_prefix(schema: &Schema) -> &'static str {
    match schema {
        Schema::Bool => "is",
        _ => "get",
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
            "public static class {} extends {} {{",
            struc.name.camel_case(conv),
            base_name.camel_case(conv)
        )?;
    } else {
        writeln!(writer, "public class {} {{", struc.name.camel_case(conv))?;
    }
    writer.inc_ident();
    if let Some((_, discriminant)) = base {
        writeln!(writer, "public static final int TAG = {};", discriminant)?;
    }

    // Fields
    for field in &struc.fields {
        writeln!(
            writer,
            "private {} {};",
            type_name(&field.schema),
            field.name.mixed_case(conv)
        )?;
        writeln!(
            writer,
            "public {} {}{}() {{ return {}; }}",
            type_name(&field.schema),
            getter_prefix(&field.schema),
            field.name.camel_case(conv),
            field.name.mixed_case(conv)
        )?;
        writeln!(
            writer,
            "public void set{}({} {}) {{ this.{} = {}; }}",
            field.name.camel_case(conv),
            type_name(&field.schema),
            field.name.mixed_case(conv),
            field.name.mixed_case(conv),
            field.name.mixed_case(conv)
        )?;
    }

    // Constructor
    writeln!(writer, "public {}() {{}}", struc.name.camel_case(conv))?;
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
        "public static {} readFrom(java.io.InputStream stream) throws java.io.IOException {{",
        struc.name.camel_case(conv)
    )?;
    writer.inc_ident();
    writeln!(
        writer,
        "{} result = new {}();",
        struc.name.camel_case(conv),
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
                    writeln!(writer, "{} = StreamUtil.readBoolean(stream);", to)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "{} = StreamUtil.readInt(stream);", to)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "{} = StreamUtil.readLong(stream);", to)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "{} = StreamUtil.readFloat(stream);", to)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "{} = StreamUtil.readDouble(stream);", to)?;
                }
                Schema::String => {
                    writeln!(writer, "{} = StreamUtil.readString(stream);", to)?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                } => {
                    writeln!(
                        writer,
                        "{} = model.{}.readFrom(stream);",
                        to,
                        name.camel_case(conv)
                    )?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if (StreamUtil.readBoolean(stream)) {{")?;
                    writer.inc_ident();
                    assign(writer, to, inner, index_var)?;
                    writer.dec_ident();
                    writeln!(writer, "}} else {{")?;
                    writeln!(writer, "    {} = null;", to)?;
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(
                        writer,
                        "{} = new {}[StreamUtil.readInt(stream)]{};",
                        to,
                        type_name_prearray(inner),
                        type_post_array(inner)
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
                    writeln!(writer, "int {} = StreamUtil.readInt(stream);", to_size)?;
                    writeln!(writer, "{} = new java.util.HashMap<>({});", to, to_size)?;
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
                        "{}.put({}Key, {}Value);",
                        to,
                        var_name(to),
                        var_name(to)
                    )?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum {
                    base_name,
                    variants,
                } => {
                    writeln!(writer, "switch (StreamUtil.readInt(stream)) {{")?;
                    for (discriminant, variant) in variants.iter().enumerate() {
                        writeln!(writer, "case {}:", discriminant)?;
                        writeln!(
                            writer,
                            "    {} = model.{}.{};",
                            to,
                            base_name.camel_case(conv),
                            variant.shouty_snake_case(conv)
                        )?;
                        writeln!(writer, "    break;")?;
                    }
                    writeln!(writer, "default:")?;
                    writeln!(
                        writer,
                        "    throw new java.io.IOException(\"Unexpected discriminant value\");"
                    )?;
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
    if base.is_some() {
        writeln!(writer, "@Override")?;
    }
    writeln!(
        writer,
        "public void writeTo(java.io.OutputStream stream) throws java.io.IOException {{",
    )?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(writer, "StreamUtil.writeInt(stream, TAG);")?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "StreamUtil.writeInt(stream, {});", magic)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "StreamUtil.writeBoolean(stream, {});", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "StreamUtil.writeInt(stream, {});", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "StreamUtil.writeLong(stream, {});", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "StreamUtil.writeFloat(stream, {});", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "StreamUtil.writeDouble(stream, {});", value)?;
                }
                Schema::String => {
                    writeln!(writer, "StreamUtil.writeString(stream, {});", value)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "{}.writeTo(stream);", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if ({} == null) {{", value)?;
                    writeln!(writer, "    StreamUtil.writeBoolean(stream, false);")?;
                    writeln!(writer, "}} else {{")?;
                    writer.inc_ident();
                    writeln!(writer, "StreamUtil.writeBoolean(stream, true);")?;
                    write(writer, value, inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "StreamUtil.writeInt(stream, {}.length);", value)?;
                    writeln!(
                        writer,
                        "for ({} {}Element : {}) {{",
                        type_name(inner),
                        var_name(value),
                        value
                    )?;
                    writer.inc_ident();
                    write(writer, &format!("{}Element", var_name(value)), inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "StreamUtil.writeInt(stream, {}.size());", value)?;
                    writeln!(
                        writer,
                        "for (java.util.Map.Entry<{}, {}> {}Entry : {}.entrySet()) {{",
                        type_name_obj(key_type),
                        type_name_obj(value_type),
                        var_name(value),
                        value
                    )?;
                    writer.inc_ident();
                    writeln!(
                        writer,
                        "{} {}Key = {}Entry.getKey();",
                        type_name(key_type),
                        var_name(value),
                        var_name(value)
                    )?;
                    writeln!(
                        writer,
                        "{} {}Value = {}Entry.getValue();",
                        type_name(value_type),
                        var_name(value),
                        var_name(value)
                    )?;
                    write(writer, &format!("{}Key", var_name(value)), key_type)?;
                    write(writer, &format!("{}Value", var_name(value)), value_type)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Enum { .. } => {
                    writeln!(
                        writer,
                        "StreamUtil.writeInt(stream, {}.discriminant);",
                        value
                    )?;
                }
            }
            Ok(())
        }
        write(writer, &field.name.mixed_case(conv), &field.schema)?;
    }
    writer.dec_ident();
    writeln!(writer, "}}")?;
    writer.dec_ident();
    writeln!(writer, "}}")?;
    Ok(())
}

impl trans_gen_core::Generator for Generator {
    fn new(name: &str, version: &str) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "util/StreamUtil.java".to_owned(),
            include_str!("../template/StreamUtil.java").to_owned(),
        );
        Self { files }
    }
    fn result(self) -> HashMap<String, String> {
        self.files
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.java", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model;").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import util.StreamUtil;").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "public enum {} {{", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for (index, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "{}({}){}",
                        variant.shouty_snake_case(conv),
                        index,
                        if index + 1 < variants.len() { "," } else { ";" }
                    )
                    .unwrap();
                }
                writeln!(writer, "public int discriminant;").unwrap();
                writeln!(
                    writer,
                    "{}(int discriminant) {{",
                    base_name.camel_case(conv)
                )
                .unwrap();
                writeln!(writer, "  this.discriminant = discriminant;").unwrap();
                writeln!(writer, "}}").unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                let file_name = format!("model/{}.java", struc.name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model;").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import util.StreamUtil;").unwrap();
                writeln!(writer).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.java", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model;").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import util.StreamUtil;").unwrap();
                writeln!(writer).unwrap();
                writeln!(
                    &mut writer,
                    "public abstract class {} {{",
                    base_name.camel_case(conv)
                )
                .unwrap();
                {
                    writer.inc_ident();
                    writeln!(
                        &mut writer,
                        "public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;"
                    )
                    .unwrap();
                    writeln!(
                        &mut writer,
                        "public static {} readFrom(java.io.InputStream stream) throws java.io.IOException {{",
                        base_name.camel_case(conv)
                    )
                    .unwrap();
                    {
                        writer.inc_ident();
                        writeln!(&mut writer, "switch (StreamUtil.readInt(stream)) {{").unwrap();
                        writer.inc_ident();
                        for variant in variants {
                            writeln!(&mut writer, "case {}.TAG:", variant.name.camel_case(conv))
                                .unwrap();
                            writeln!(
                                &mut writer,
                                "    return {}.readFrom(stream);",
                                variant.name.camel_case(conv)
                            )
                            .unwrap();
                        }
                        writeln!(&mut writer, "default:").unwrap();
                        writeln!(
                            &mut writer,
                            "    throw new java.io.IOException(\"Unexpected discriminant value\");"
                        )
                        .unwrap();
                        writer.dec_ident();
                        writeln!(&mut writer, "}}").unwrap();
                        writer.dec_ident();
                    }
                    writeln!(&mut writer, "}}").unwrap();
                    for (discriminant, variant) in variants.iter().enumerate() {
                        writeln!(&mut writer).unwrap();
                        write_struct(&mut writer, variant, Some((base_name, discriminant)))
                            .unwrap();
                    }
                    writer.dec_ident();
                }
                writeln!(&mut writer, "}}").unwrap();
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
