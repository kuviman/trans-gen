use super::*;

fn conv(name: &str) -> String {
    name.replace("Int32", "Int")
        .replace("Int64", "Long")
        .replace("Float32", "Single")
        .replace("Float64", "Double")
        .replace("Params", "Parameters")
}

pub struct Generator {
    main_namespace: String,
    files: Vec<File>,
}

fn type_name(schema: &Schema) -> String {
    format!("{}{}", type_name_prearray(schema), type_post_array(schema))
}

fn type_name_prearray(schema: &Schema) -> String {
    match schema {
        Schema::Bool => "bool".to_owned(),
        Schema::Int32 => "int".to_owned(),
        Schema::Int64 => "long".to_owned(),
        Schema::Float32 => "single".to_owned(),
        Schema::Float64 => "double".to_owned(),
        Schema::String => "string".to_owned(),
        Schema::Struct(Struct { name, .. })
        | Schema::OneOf {
            base_name: name, ..
        }
        | Schema::Enum {
            base_name: name, ..
        } => format!("{}", name.camel_case(conv)),
        Schema::Option(inner) => format!("option<{}>", type_name(inner)),
        Schema::Vec(inner) => type_name_prearray(inner),
        Schema::Map(key, value) => format!("Map<{}, {}>", type_name(key), type_name(value)),
    }
}

fn type_post_array(schema: &Schema) -> String {
    match schema {
        Schema::Vec(inner) => format!("[]{}", type_post_array(inner)),
        _ => String::new(),
    }
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
    let struc_name = if let Some((base, _)) = base {
        format!("{}{}", base.camel_case(conv), struc.name.camel_case(conv))
    } else {
        struc.name.camel_case(conv)
    };

    if struc.fields.is_empty() {
        writeln!(writer, "type {} = struct end with", struc_name)?;
        writer.inc_ident();
    } else {
        writeln!(writer, "type {} = {{", struc_name)?;
        writer.inc_ident();
        for field in &struc.fields {
            writeln!(
                writer,
                "{}: {};",
                field.name.camel_case(conv),
                type_name(&field.schema),
            )?;
        }
        writeln!(writer, "}} with")?;
    }

    // Writing
    writeln!(
        writer,
        "member this.writeTo(writer: System.IO.BinaryWriter) ="
    )?;
    writer.inc_ident();
    if let Some((_, tag)) = base {
        writeln!(writer, "writer.Write {}", tag)?;
    }
    if let Some(magic) = struc.magic {
        writeln!(writer, "writer.Write {}", magic)?;
    }
    for field in &struc.fields {
        fn write(writer: &mut Writer, value: &str, schema: &Schema) -> std::fmt::Result {
            match schema {
                Schema::Bool => {
                    writeln!(writer, "writer.Write {}", value)?;
                }
                Schema::Int32 => {
                    writeln!(writer, "writer.Write {}", value)?;
                }
                Schema::Int64 => {
                    writeln!(writer, "writer.Write {}", value)?;
                }
                Schema::Float32 => {
                    writeln!(writer, "writer.Write {}", value)?;
                }
                Schema::Float64 => {
                    writeln!(writer, "writer.Write {}", value)?;
                }
                Schema::String => {
                    let data_var = format!("{}Data", var_name(value));
                    writeln!(
                        writer,
                        "let {} : byte[] = System.Text.Encoding.UTF8.GetBytes {}",
                        data_var, value
                    )?;
                    writeln!(writer, "writer.Write {}.Length", data_var)?;
                    writeln!(writer, "writer.Write {}", data_var)?;
                }
                Schema::Struct(_) | Schema::OneOf { .. } => {
                    writeln!(writer, "{}.writeTo writer", value)?;
                }
                Schema::Option(inner) => {
                    writeln!(writer, "match {} with", value)?;
                    writer.inc_ident();
                    writeln!(writer, "| Some value ->")?;
                    writer.inc_ident();
                    writeln!(writer, "writer.Write true")?;
                    write(writer, "value", inner)?;
                    writer.dec_ident();
                    writeln!(writer, "| None -> writer.Write false")?;
                    writer.dec_ident();
                }
                Schema::Vec(inner) => {
                    writeln!(writer, "writer.Write {}.Length", value)?;
                    writeln!(writer, "{} |> Array.iter (fun value ->", value)?;
                    writer.inc_ident();
                    write(writer, "value", inner)?;
                    writer.dec_ident();
                    writeln!(writer, ")")?;
                }
                Schema::Map(key_type, value_type) => {
                    writeln!(writer, "writer.Write {}.Count", value)?;
                    writeln!(writer, "{} |> Map.iter (fun key value ->", value)?;
                    writer.inc_ident();
                    write(writer, "key", key_type)?;
                    write(writer, "value", value_type)?;
                    writer.dec_ident();
                    writeln!(writer, ")")?;
                }
                Schema::Enum { .. } => {
                    writeln!(writer, "writer.Write (int {})", value)?;
                }
            }
            Ok(())
        }
        write(
            writer,
            &format!("this.{}", field.name.camel_case(conv)),
            &field.schema,
        )?;
    }
    writer.dec_ident();

    // Reading
    if struc.fields.is_empty() {
        writeln!(
            writer,
            "static member readFrom(reader: System.IO.BinaryReader) = new {}()",
            struc_name,
        )?;
    } else {
        writeln!(
            writer,
            "static member readFrom(reader: System.IO.BinaryReader) = {{"
        )?;
        writer.inc_ident();
        for field in &struc.fields {
            fn read(writer: &mut Writer, schema: &Schema) -> std::fmt::Result {
                match schema {
                    Schema::Bool => {
                        writeln!(writer, "reader.ReadBoolean()")?;
                    }
                    Schema::Int32 => {
                        writeln!(writer, "reader.ReadInt32()")?;
                    }
                    Schema::Int64 => {
                        writeln!(writer, "reader.ReadInt64()")?;
                    }
                    Schema::Float32 => {
                        writeln!(writer, "reader.ReadSingle()")?;
                    }
                    Schema::Float64 => {
                        writeln!(writer, "reader.ReadDouble()")?;
                    }
                    Schema::String => {
                        writeln!(writer, "reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString")?;
                    }
                    Schema::Struct(Struct { name, .. })
                    | Schema::OneOf {
                        base_name: name, ..
                    } => {
                        writeln!(writer, "{}.readFrom reader", name.camel_case(conv))?;
                    }
                    Schema::Option(inner) => {
                        writeln!(writer, "match reader.ReadBoolean() with")?;
                        writer.inc_ident();
                        writeln!(writer, "| true ->")?;
                        writer.inc_ident();
                        writeln!(writer, "Some(")?;
                        writer.inc_ident();
                        read(writer, inner)?;
                        writeln!(writer, ")")?;
                        writer.dec_ident();
                        writer.dec_ident();
                        writeln!(writer, "| false -> None")?;
                        writer.dec_ident();
                    }
                    Schema::Vec(inner) => {
                        writeln!(writer, "[|for _ in 1 .. reader.ReadInt32() do")?;
                        writer.inc_ident();
                        write!(writer, "yield ")?;
                        read(writer, inner)?;
                        writer.dec_ident();
                        writeln!(writer, "|]")?;
                    }
                    Schema::Map(key_type, value_type) => {
                        writeln!(writer, "[for _ in 1 .. reader.ReadInt32() do")?;
                        writer.inc_ident();
                        write!(writer, "let key = ")?;
                        read(writer, key_type)?;
                        write!(writer, "let value = ")?;
                        read(writer, value_type)?;
                        writeln!(writer, "yield (key, value)")?;
                        writeln!(writer, "] |> Map.ofList")?;
                        writer.dec_ident();
                    }
                    Schema::Enum { .. } => {
                        writeln!(writer, "reader.ReadInt32() |> enum")?;
                    }
                }
                Ok(())
            }
            write!(writer, "{} = ", field.name.camel_case(conv))?;
            read(writer, &field.schema)?;
        }
        writer.dec_ident();
        writeln!(writer, "}}")?;
    }

    writer.dec_ident();

    Ok(())
}

impl crate::Generator for Generator {
    const NAME: &'static str = "F#";
    type Options = ();
    fn new(name: &str, _version: &str, _: ()) -> Self {
        Self {
            main_namespace: Name::new(name.to_owned()).camel_case(conv),
            files: Vec::new(),
        }
    }
    fn generate(mut self, extra_files: Vec<File>) -> GenResult {
        self.files.extend(extra_files);
        let source_files = self.files.iter().filter(|file| file.path.ends_with(".fs"));
        let fsproj = include_templing!("src/gens/fsharp/project.fsproj.templing");
        self.files.push(File {
            path: format!("{}.fsproj", self.main_namespace),
            content: fsproj,
        });
        self.files.into()
    }
    fn add_only(&mut self, schema: &Schema) {
        match schema {
            Schema::Enum {
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("Model/{}.fs", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "#nowarn \"0058\"").unwrap();
                writeln!(writer, "namespace {}.Model", self.main_namespace).unwrap();
                writeln!(writer, "type {} =", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(writer, "| {} = {}", variant.name.camel_case(conv), tag).unwrap();
                }
                writer.dec_ident();
                self.files.push(File {
                    path: file_name,
                    content: writer.get(),
                });
            }
            Schema::Struct(struc) => {
                let file_name = format!("Model/{}.fs", struc.name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "#nowarn \"0058\"").unwrap();
                writeln!(writer, "namespace {}.Model", self.main_namespace).unwrap();
                write_struct(&mut writer, struc, None).unwrap();
                self.files.push(File {
                    path: file_name,
                    content: writer.get(),
                });
            }
            Schema::OneOf {
                documentation: _,
                base_name,
                variants,
            } => {
                let file_name = format!("Model/{}.fs", base_name.camel_case(conv));
                let mut writer = Writer::new();
                writeln!(writer, "#nowarn \"0058\"").unwrap();
                writeln!(writer, "namespace {}.Model", self.main_namespace).unwrap();

                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(&mut writer).unwrap();
                    write_struct(&mut writer, variant, Some((base_name, tag))).unwrap();
                }

                writeln!(&mut writer, "type {} = ", base_name.camel_case(conv)).unwrap();
                writer.inc_ident();
                for variant in variants {
                    writeln!(
                        writer,
                        "| {} of {}{}",
                        variant.name.camel_case(conv),
                        base_name.camel_case(conv),
                        variant.name.camel_case(conv)
                    )
                    .unwrap();
                }
                writeln!(writer, "with").unwrap();

                writeln!(
                    writer,
                    "member this.writeTo(writer: System.IO.BinaryWriter) ="
                )
                .unwrap();
                writer.inc_ident();
                writeln!(writer, "match this with").unwrap();
                writer.inc_ident();
                for variant in variants {
                    writeln!(
                        writer,
                        "| {} value -> value.writeTo writer",
                        variant.name.camel_case(conv),
                    )
                    .unwrap();
                }
                writer.dec_ident();
                writer.dec_ident();

                writeln!(
                    writer,
                    "static member readFrom(reader: System.IO.BinaryReader) ="
                )
                .unwrap();
                writer.inc_ident();
                writeln!(writer, "match reader.ReadInt32() with").unwrap();
                writer.inc_ident();
                for (tag, variant) in variants.iter().enumerate() {
                    writeln!(
                        writer,
                        "| {} -> {} ({}{}.readFrom reader)",
                        tag,
                        variant.name.camel_case(conv),
                        base_name.camel_case(conv),
                        variant.name.camel_case(conv),
                    )
                    .unwrap();
                }
                writeln!(writer, "| x -> failwith (sprintf \"Unexpected tag %d\" x)").unwrap();
                writer.dec_ident();
                writer.dec_ident();

                writer.dec_ident();
                self.files.push(File {
                    path: file_name,
                    content: writer.get(),
                });
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
                if file.path().extension() == Some("fsproj".as_ref()) {
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
            path: "Runner.fs".to_owned(),
            content: include_templing!("src/gens/fsharp/FileReadWrite.fs.templing"),
        }]
    }
}
