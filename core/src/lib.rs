use std::collections::{HashMap, HashSet};

pub use trans;
pub use trans_schema;
use trans_schema::*;

pub trait Generator {
    fn new(name: &str, version: &str) -> Self;
    fn add_only(&mut self, schema: &Schema);
    fn result(self) -> HashMap<String, String>;
}

pub struct GeneratorImpl<T: Generator> {
    inner: T,
    added: HashMap<Name, Schema>,
}

impl<T: Generator> GeneratorImpl<T> {
    pub fn new(name: &str, version: &str) -> Self {
        Self {
            inner: T::new(name, version),
            added: HashMap::new(),
        }
    }
    pub fn add(&mut self, schema: &Schema) {
        let schema_name = schema.full_name();
        let current = self.added.get(&schema_name);
        if let Some(current) = current {
            assert_eq!(
                current, schema,
                "Two schemas with same name but different structure"
            );
            return;
        }
        self.added.insert(schema_name, schema.clone());
        match schema {
            Schema::Struct(Struct { fields, .. }) => {
                for field in fields {
                    self.add(&field.schema);
                }
            }
            Schema::OneOf { variants, .. } => {
                for variant in variants {
                    for field in &variant.fields {
                        self.add(&field.schema);
                    }
                }
            }
            Schema::Option(inner) => {
                self.add(inner);
            }
            Schema::Vec(inner) => {
                self.add(inner);
            }
            Schema::Map(key_type, value_type) => {
                self.add(key_type);
                self.add(value_type);
            }
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String
            | Schema::Enum { .. } => {}
        }
        self.inner.add_only(schema);
    }
    pub fn result(self) -> HashMap<String, String> {
        self.inner.result()
    }
    pub fn write_to<P: AsRef<std::path::Path>>(self, dir: P) -> std::io::Result<()> {
        use std::path::Path;
        let dir = dir.as_ref();
        for (path, content) in self.result() {
            if let Some(parent) = Path::new(&path).parent() {
                std::fs::create_dir_all(dir.join(parent))?;
            }
            let mut file = std::fs::File::create(dir.join(path))?;
            use std::io::Write;
            file.write_all(content.as_bytes())?;
        }
        Ok(())
    }
}

pub fn add_all(files: &mut HashMap<String, String>, dir: &include_dir::Dir, prefix: &str) {
    for file in dir.files() {
        files.insert(
            format!("{}/{}", prefix, file.path().to_str().unwrap()),
            file.contents_utf8().unwrap().to_owned(),
        );
    }
    for subdir in dir.dirs() {
        add_all(files, subdir, prefix);
    }
}

pub struct Writer {
    content: String,
    ident_level: usize,
}

impl Writer {
    pub fn new() -> Self {
        Self {
            content: String::new(),
            ident_level: 0,
        }
    }
    pub fn inc_ident(&mut self) {
        self.ident_level += 1;
    }
    pub fn dec_ident(&mut self) {
        self.ident_level -= 1;
    }
    pub fn get(self) -> String {
        assert_eq!(self.ident_level, 0, "Incorrect indentation");
        self.content
    }
}

impl std::fmt::Write for Writer {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            if c != '\n' && self.content.chars().last().unwrap_or('\n') == '\n' {
                for _ in 0..self.ident_level * 4 {
                    self.content.push(' ');
                }
            }
            self.content.push(c);
        }
        Ok(())
    }
}
