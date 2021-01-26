pub use trans;

use anyhow::{anyhow, Context as _};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Write;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;
use templing::*;
use trans::*;

mod util;
use util::*;

pub mod gens;
pub mod testing;

pub use testing::{Test, TestExt, TestableGenerator};

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
    const NAME: &'static str;
    type Options: Default;
    fn new(name: &str, version: &str, options: Self::Options) -> Self;
    fn add_only(&mut self, schema: &trans::Schema);
    fn generate(self, extra_files: Vec<File>) -> GenResult;
}

pub trait RunnableGenerator: Generator {
    fn build_local(path: &Path) -> anyhow::Result<()>;
    fn run_local(path: &Path) -> anyhow::Result<Command>;
}

pub fn generate<T: Generator>(
    name: &str,
    version: &str,
    options: T::Options,
    schemas: &[std::sync::Arc<Schema>],
    extra_files: Vec<File>,
) -> GenResult {
    fn add<T: Generator>(generator: &mut T, added: &mut HashMap<Name, Schema>, schema: &Schema) {
        let schema_name = schema.full_name();
        if let Some(current) = added.get(&schema_name) {
            assert_eq!(
                current, schema,
                "Two schemas with same name but different structure"
            );
            return;
        }
        added.insert(schema_name, schema.clone());
        match schema {
            Schema::Struct(Struct { fields, .. }) => {
                for field in fields {
                    add(generator, added, &field.schema);
                }
            }
            Schema::OneOf { variants, .. } => {
                for variant in variants {
                    for field in &variant.fields {
                        add(generator, added, &field.schema);
                    }
                }
            }
            Schema::Option(inner) => {
                add(generator, added, inner);
            }
            Schema::Vec(inner) => {
                add(generator, added, inner);
            }
            Schema::Map(key_type, value_type) => {
                add(generator, added, key_type);
                add(generator, added, value_type);
            }
            Schema::Bool
            | Schema::Int32
            | Schema::Int64
            | Schema::Float32
            | Schema::Float64
            | Schema::String
            | Schema::Enum { .. } => {}
        }
        generator.add_only(schema);
    }
    let mut generator = T::new(name, version, options);
    let mut added = HashMap::new();
    for schema in schemas {
        add(&mut generator, &mut added, schema);
    }
    generator.generate(extra_files)
}
