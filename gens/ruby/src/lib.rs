use std::collections::HashMap;
use std::fmt::Write;
use trans_gen_core::trans::*;
use trans_gen_core::Writer;

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

fn add_imports(writer: &mut Writer, schema: &Schema) -> std::fmt::Result {
    fn add_imports_struct(writer: &mut Writer, struc: &Struct) -> std::fmt::Result {
        fn add_for_field(writer: &mut Writer, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Struct(Struct { name, .. })
                | Schema::OneOf {
                    base_name: name, ..
                }
                | Schema::Enum {
                    base_name: name, ..
                } => {
                    writeln!(writer, "require_relative '{}'", name.snake_case(conv),)?;
                }
                Schema::Option(inner) => {
                    add_for_field(writer, inner)?;
                }
                Schema::Vec(inner) => {
                    add_for_field(writer, inner)?;
                }
                Schema::Map(key_type, value_type) => {
                    add_for_field(writer, key_type)?;
                    add_for_field(writer, value_type)?;
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
        for field in &struc.fields {
            add_for_field(writer, &field.schema)?;
        }
        Ok(())
    }
    match schema {
        Schema::Struct(struc) => {
            add_imports_struct(writer, struc)?;
        }
        Schema::OneOf { variants, .. } => {
            for variant in variants {
                add_imports_struct(writer, variant)?;
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
    writeln!(writer, "class {}", struc.name.camel_case(conv))?;
    writer.inc_ident();
    if let Some((_, discriminant)) = base {
        writeln!(writer, "TAG = {}", discriminant)?;
    }

    // Fields
    for field in &struc.fields {
        writeln!(writer, "attr_accessor :{}", field.name.snake_case(conv))?;
    }

    // Constructor
    write!(writer, "def initialize(")?;
    for (index, field) in struc.fields.iter().enumerate() {
        if (index > 0) {
            write!(writer, ", ")?;
        }
        write!(writer, "{}", field.name.snake_case(conv))?;
    }
    writeln!(writer, ")")?;
    for field in &struc.fields {
        writeln!(
            writer,
            "    @{} = {}",
            field.name.snake_case(conv),
            field.name.snake_case(conv)
        )?;
    }
    writeln!(writer, "end")?;

    // Reading
    writeln!(writer, "def self.read_from(stream)")?;
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
                    writeln!(writer, "if stream.read_bool()")?;
                    writer.inc_ident();
                    assign(writer, to, inner)?;
                    writer.dec_ident();
                    writeln!(writer, "else")?;
                    writeln!(writer, "    {} = nil", to)?;
                    writeln!(writer, "end")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "{} = []", to)?;
                    writeln!(writer, "stream.read_int().times do |_|")?;
                    writer.inc_ident();
                    assign(writer, &format!("{}_element", to), inner)?;
                    writeln!(writer, "{}.push({}_element)", to, to)?;
                    writer.dec_ident();
                    writeln!(writer, "end")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "{} = Hash.new", to)?;
                    writeln!(writer, "stream.read_int().times do |_|")?;
                    writer.inc_ident();
                    assign(writer, &format!("{}_key", to), key_type)?;
                    assign(writer, &format!("{}_value", to), value_type)?;
                    writeln!(writer, "{}[{}_key] = {}_value", to, to, to)?;
                    writer.dec_ident();
                    writeln!(writer, "end")?;
                }
                Schema::Enum {
                    base_name,
                    variants,
                } => {
                    writeln!(writer, "{} = stream.read_int()", to,)?;
                    writeln!(writer, "if {} < 0 || {} > {}", to, to, variants.len())?;
                    writeln!(writer, "    raise \"Unexpected discriminant value\"")?;
                    writeln!(writer, "end")?;
                }
            }
            Ok(())
        }
        assign(writer, &field.name.snake_case(conv), &field.schema)?;
    }
    write!(writer, "{}.new(", struc.name.camel_case(conv))?;
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
    writeln!(writer, "end")?;

    // Writing
    writeln!(writer, "def write_to(stream)")?;
    writer.inc_ident();
    if base.is_some() {
        writeln!(writer, "stream.write_int(TAG)")?;
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
                    writeln!(writer, "if {}.nil?", value)?;
                    writeln!(writer, "    stream.write_bool(false)")?;
                    writeln!(writer, "else")?;
                    writer.inc_ident();
                    writeln!(writer, "stream.write_bool(true)")?;
                    write(writer, value, inner)?;
                    writer.dec_ident();
                    writeln!(writer, "end")?;
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "stream.write_int({}.length())", value)?;
                    writeln!(writer, "{}.each do |element|", value)?;
                    writer.inc_ident();
                    write(writer, "element", inner)?;
                    writer.dec_ident();
                    writeln!(writer, "end")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "stream.write_int({}.length())", value)?;
                    writeln!(writer, "{}.each do |key, value|", value)?;
                    writer.inc_ident();
                    write(writer, "key", key_type)?;
                    write(writer, "value", value_type)?;
                    writer.dec_ident();
                    writeln!(writer, "end")?;
                }
                Schema::Enum { .. } => {
                    writeln!(writer, "stream.write_int({})", value)?;
                }
            }
            Ok(())
        }
        write(
            writer,
            &format!("@{}", field.name.snake_case(conv)),
            &field.schema,
        )?;
    }
    writer.dec_ident();
    writeln!(writer, "end")?;

    writer.dec_ident();
    writeln!(writer, "end")?;
    Ok(())
}

impl trans_gen_core::Generator for Generator {
    fn new(name: &str, version: &str) -> Self {
        let mut files = HashMap::new();
        files.insert(
            "stream_wrapper.rb".to_owned(),
            include_str!("../template/stream_wrapper.rb").to_owned(),
        );
        Self {
            model_init: String::new(),
            files,
        }
    }
    fn result(mut self) -> trans_gen_core::GenResult {
        if !self.model_init.is_empty() {
            self.files.insert("model.rb".to_owned(), self.model_init);
        }
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.rb", base_name.snake_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "module {}", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for (index, variant) in variants.iter().enumerate() {
                    writeln!(writer, "{} = {}", variant.shouty_snake_case(conv), index).unwrap();
                }
                writer.dec_ident();
                writeln!(writer, "end").unwrap();
                writeln!(
                    &mut self.model_init,
                    "require_relative 'model/{}'",
                    base_name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::Struct(struc) => {
                let file_name = format!("model/{}.rb", struc.name.snake_case(conv));
                let mut writer = Writer::new();
                add_imports(&mut writer, schema).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                writeln!(
                    &mut self.model_init,
                    "require_relative 'model/{}'",
                    struc.name.snake_case(conv),
                )
                .unwrap();
                self.files.insert(file_name, writer.get());
            }
            Schema::OneOf {
                base_name,
                variants,
            } => {
                let file_name = format!("model/{}.rb", base_name.snake_case(conv));
                let mut writer = Writer::new();
                add_imports(&mut writer, schema).unwrap();
                writeln!(writer, "class {}", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                writeln!(writer, "def self.read_from(stream)").unwrap();
                writer.inc_ident();
                writeln!(writer, "discriminant = stream.read_int()").unwrap();
                for variant in variants {
                    writeln!(
                        writer,
                        "if discriminant == {}::{}::TAG",
                        base_name.camel_case(conv),
                        variant.name.camel_case(conv),
                    )
                    .unwrap();
                    writeln!(
                        writer,
                        "    return {}::{}.read_from(stream)",
                        base_name.camel_case(conv),
                        variant.name.camel_case(conv),
                    )
                    .unwrap();
                    writeln!(writer, "end").unwrap();
                }
                writeln!(writer, "raise \"Unexpected discriminant value\"").unwrap();
                writer.dec_ident();
                writeln!(writer, "end").unwrap();
                writeln!(writer).unwrap();
                for (discriminant, variant) in variants.iter().enumerate() {
                    write_struct(&mut writer, variant, Some((base_name, discriminant))).unwrap();
                }
                writer.dec_ident();
                writeln!(writer, "end").unwrap();
                writeln!(
                    &mut self.model_init,
                    "require_relative 'model/{}'",
                    base_name.snake_case(conv),
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
