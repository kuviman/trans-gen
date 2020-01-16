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
    match schema {
        Schema::Bool => "Boolean".to_owned(),
        Schema::Int32 => "Int".to_owned(),
        Schema::Int64 => "Long".to_owned(),
        Schema::Float32 => "Float".to_owned(),
        Schema::Float64 => "Double".to_owned(),
        Schema::String => "String".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => format!("model.{}", name.camel_case(conv)),
        Schema::Option(inner) => format!("Option[{}]", type_name(inner)),
        Schema::Vec(inner) => format!("Seq[{}]", type_name(inner)),
        Schema::Map(key, value) => format!("Map[{}, {}]", type_name(key), type_name(value)),
    }
}

fn write_struct(
    writer: &mut Writer,
    struc: &Struct,
    base: Option<(&Name, usize)>,
) -> std::fmt::Result {
    // Class
    write!(writer, "case class {}(", struc.name.camel_case(conv))?;
    for (index, field) in struc.fields.iter().enumerate() {
        if index > 0 {
            write!(writer, ", ")?;
        }
        write!(
            writer,
            "{}: {}",
            field.name.mixed_case(conv),
            type_name(&field.schema),
        )?;
    }
    write!(writer, ")")?;
    if let Some((base, _)) = base {
        write!(writer, " extends {}", base.camel_case(conv))?;
    }
    writeln!(writer, " {{")?;
    writer.inc_ident();

    // Writing
    if base.is_some() {
        write!(writer, "override ")?;
    }
    writeln!(writer, "def writeTo(stream: java.io.OutputStream) {{")?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(
            writer,
            "StreamUtil.writeInt(stream, {}.TAG)",
            struc.name.camel_case(conv),
        )?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "StreamUtil.writeInt(stream, {})", magic)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "StreamUtil.writeBoolean(stream, {})", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "StreamUtil.writeInt(stream, {})", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "StreamUtil.writeLong(stream, {})", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "StreamUtil.writeFloat(stream, {})", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "StreamUtil.writeDouble(stream, {})", value)?;
                }
                Schema::String => {
                    writeln!(writer, "StreamUtil.writeString(stream, {})", value)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } | Schema::Enum { .. } => {
                    writeln!(writer, "{}.writeTo(stream)", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "{} match {{", value)?;
                    writer.inc_ident();
                    writeln!(
                        writer,
                        "case None => StreamUtil.writeBoolean(stream, false)"
                    )?;
                    writeln!(writer, "case Some(value) => {{")?;
                    writer.inc_ident();
                    writeln!(writer, "StreamUtil.writeBoolean(stream, true)")?;
                    write(writer, "value", inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "StreamUtil.writeInt(stream, {}.length)", value)?;
                    writeln!(writer, "{}.foreach {{ value =>", value)?;
                    writer.inc_ident();
                    write(writer, "value", inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "StreamUtil.writeInt(stream, {}.size)", value)?;
                    writeln!(writer, "{}.foreach {{ case (key, value) =>", value)?;
                    writer.inc_ident();
                    write(writer, "key", key_type)?;
                    write(writer, "value", value_type)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
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

    // Object
    writeln!(writer, "object {} {{", struc.name.camel_case(conv))?;
    writer.inc_ident();
    if let Some((_, discriminant)) = base {
        writeln!(writer, "val TAG: Int = {}", discriminant)?;
    }

    // Reading
    writeln!(
        writer,
        "def readFrom(stream: java.io.InputStream): {} = {}(",
        struc.name.camel_case(conv),
        struc.name.camel_case(conv),
    )?;
    writer.inc_ident();
    for (index, field) in struc.fields.iter().enumerate() {
        fn read(writer: &mut Writer, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "StreamUtil.readBoolean(stream)")?;
                }
                Schema::Int32 => {
                    writeln!(writer, "StreamUtil.readInt(stream)")?;
                }
                Schema::Int64 => {
                    writeln!(writer, "StreamUtil.readLong(stream)")?;
                }
                Schema::Float32 => {
                    writeln!(writer, "StreamUtil.readFloat(stream)")?;
                }
                Schema::Float64 => {
                    writeln!(writer, "StreamUtil.readDouble(stream)")?;
                }
                Schema::String => {
                    writeln!(writer, "StreamUtil.readString(stream)")?;
                }
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                }
                | Schema::Enum {
                    base_name: name, ..
                } => {
                    writeln!(writer, "model.{}.readFrom(stream)", name.camel_case(conv))?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "if (StreamUtil.readBoolean(stream)) Some(")?;
                    writer.inc_ident();
                    read(writer, inner)?;
                    writer.dec_ident();
                    writeln!(writer, ") else None")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "(0 until StreamUtil.readInt(stream)).map {{ _ =>",)?;
                    writer.inc_ident();
                    read(writer, inner)?;
                    writer.dec_ident();
                    writeln!(writer, "}}")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "(0 until StreamUtil.readInt(stream)).map {{ _ => (",)?;
                    writer.inc_ident();
                    read(writer, key_type)?;
                    writeln!(writer, ",")?;
                    read(writer, value_type)?;
                    writer.dec_ident();
                    writeln!(writer, ")}}.toMap")?;
                }
            }
            Ok(())
        }
        read(writer, &field.schema)?;
        if index + 1 < struc.fields.len() {
            writeln!(writer, ",")?;
        }
    }
    writeln!(writer, ")")?;
    writer.dec_ident();
    writer.dec_ident();
    writeln!(writer, "}}")?;
    Ok(())
}

impl trans_gen_core::Generator for Generator {
    fn new(name: &str, version: &str) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "util/StreamUtil.scala".to_owned(),
            include_str!("../template/StreamUtil.scala").to_owned(),
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
                let file_name = format!("model/{}.scala", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import util.StreamUtil").unwrap();
                writeln!(writer).unwrap();
                writeln!(
                    writer,
                    "sealed abstract class {} (val discriminant: Int) {{",
                    base_name.camel_case(conv)
                )
                .unwrap();
                writer.inc_ident();
                writeln!(writer, "def writeTo(stream: java.io.OutputStream) {{").unwrap();
                writeln!(writer, "    StreamUtil.writeInt(stream, discriminant)").unwrap();
                writeln!(writer, "}}").unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "object {} {{", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for (index, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "case object {} extends {}({})",
                        variant.shouty_snake_case(conv),
                        base_name.camel_case(conv),
                        index,
                    )
                    .unwrap();
                }
                writeln!(
                    writer,
                    "def readFrom(stream: java.io.InputStream): {} = StreamUtil.readInt(stream) match {{",
                    base_name.camel_case(conv)
                )
                .unwrap();
                writer.inc_ident();
                for (index, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "case {} => {}",
                        index,
                        variant.shouty_snake_case(conv),
                    )
                    .unwrap();
                }
                writeln!(
                    writer,
                    "case _ => throw new java.io.IOException(\"Unexpected discriminant value\")"
                )
                .unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                writer.dec_ident();
                writeln!(writer, "}}").unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                let file_name = format!("model/{}.scala", struc.name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import util.StreamUtil").unwrap();
                writeln!(writer).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.scala", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "package model").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "import util.StreamUtil").unwrap();
                writeln!(writer).unwrap();
                writeln!(writer, "sealed trait {} {{", base_name.camel_case(conv)).unwrap();
                writeln!(writer, "    def writeTo(stream: java.io.OutputStream)").unwrap();
                writeln!(writer, "}}").unwrap();
                writeln!(&mut writer, "object {} {{", base_name.camel_case(conv)).unwrap();
                {
                    writer.inc_ident();
                    for (discriminant, variant) in variants.iter().enumerate() {
                        write_struct(&mut writer, variant, Some((base_name, discriminant)))
                            .unwrap();
                        writeln!(&mut writer).unwrap();
                    }
                    writeln!(
                        &mut writer,
                        "def readFrom(stream: java.io.InputStream): {} = {{",
                        base_name.camel_case(conv)
                    )
                    .unwrap();
                    {
                        writer.inc_ident();
                        writeln!(&mut writer, "StreamUtil.readInt(stream) match {{").unwrap();
                        writer.inc_ident();
                        for variant in variants {
                            writeln!(
                                &mut writer,
                                "case {}.TAG => {}.readFrom(stream)",
                                variant.name.camel_case(conv),
                                variant.name.camel_case(conv),
                            )
                            .unwrap();
                        }
                        writeln!(
                            &mut writer,
                            "case _ => throw new java.io.IOException(\"Unexpected discriminant value\")"
                        )
                        .unwrap();
                        writer.dec_ident();
                        writeln!(&mut writer, "}}").unwrap();
                        writer.dec_ident();
                    }
                    writeln!(&mut writer, "}}").unwrap();
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
