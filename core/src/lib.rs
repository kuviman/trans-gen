use std::collections::{HashMap, HashSet};

use std::path::Path;
pub use trans;
pub use trans_schema;
use trans_schema::*;

#[derive(Debug)]
pub struct File {
    pub path: String,
    pub content: String,
}

#[derive(Debug)]
pub struct GenResult {
    pub files: Vec<File>,
}

impl GenResult {
    pub fn write_to<P: AsRef<Path>>(&self, target_dir: P) -> std::io::Result<()> {
        let target_dir = target_dir.as_ref();
        for file in &self.files {
            if let Some(parent) = Path::new(&file.path).parent() {
                std::fs::create_dir_all(target_dir.join(parent))?;
            }
            use std::io::Write;
            std::fs::File::create(target_dir.join(&file.path))?
                .write_all(file.content.as_bytes())?;
        }
        Ok(())
    }
}

impl From<HashMap<String, String>> for GenResult {
    fn from(file_map: HashMap<String, String>) -> Self {
        Self {
            files: file_map
                .into_iter()
                .map(|(path, content)| File { path, content })
                .collect(),
        }
    }
}

impl From<Vec<File>> for GenResult {
    fn from(files: Vec<File>) -> Self {
        Self { files }
    }
}

pub trait Generator {
    fn new(name: &str, version: &str) -> Self;
    fn add_only(&mut self, schema: &Schema);
    fn result(self) -> GenResult;
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
    pub fn result(self) -> GenResult {
        self.inner.result()
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
