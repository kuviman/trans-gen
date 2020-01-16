use heck::*;
use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

pub use trans_schema_derive::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Name(String);

impl Name {
    pub fn new(name: String) -> Self {
        Self(name.to_camel_case())
    }
    pub fn raw(&self) -> String {
        self.0.clone()
    }
    pub fn snake_case(&self, conv: impl FnOnce(&str) -> String) -> String {
        conv(&self.0).to_snake_case()
    }
    pub fn camel_case(&self, conv: impl FnOnce(&str) -> String) -> String {
        conv(&self.0).to_camel_case()
    }
    pub fn shouty_snake_case(&self, conv: impl FnOnce(&str) -> String) -> String {
        conv(&self.0).to_shouty_snake_case()
    }
    pub fn mixed_case(&self, conv: impl FnOnce(&str) -> String) -> String {
        conv(&self.0).to_mixed_case()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Field {
    pub name: Name,
    pub schema: Arc<Schema>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub magic: Option<i32>,
    pub name: Name,
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Schema {
    Bool,
    Int32,
    Int64,
    Float32,
    Float64,
    String,
    Struct(Struct),
    OneOf {
        base_name: Name,
        variants: Vec<Struct>,
    },
    Option(Arc<Schema>),
    Vec(Arc<Schema>),
    Map(Arc<Schema>, Arc<Schema>),
    Enum {
        base_name: Name,
        variants: Vec<Name>,
    },
}

impl Schema {
    pub fn full_name(&self) -> Name {
        match self {
            Schema::Bool => Name("Bool".to_owned()),
            Schema::Int32 => Name("Int32".to_owned()),
            Schema::Int64 => Name("Int64".to_owned()),
            Schema::Float32 => Name("Float32".to_owned()),
            Schema::Float64 => Name("Float64".to_owned()),
            Schema::String => Name("String".to_owned()),
            Schema::Struct(Struct { name, .. }) => name.clone(),
            Schema::OneOf { base_name, .. } => base_name.to_owned(),
            Schema::Option(inner) => Name(format!("Opt{}", inner.full_name().0)),
            Schema::Vec(inner) => Name(format!("Vec{}", inner.full_name().0)),
            Schema::Map(key, value) => {
                Name(format!("Map{}{}", key.full_name().0, value.full_name().0))
            }
            Schema::Enum { base_name, .. } => base_name.clone(),
        }
    }
    pub fn hashable(&self) -> bool {
        match self {
            Self::Bool | Self::Int32 | Self::Int64 | Self::String => true,
            Self::Float32 | Self::Float64 => false,
            Self::Option(_) => false,
            Self::Struct(Struct { fields, .. }) => {
                fields.iter().all(|field| field.schema.hashable())
            }
            Self::OneOf { .. } => false,
            Self::Vec(_) => false,
            Self::Map(_, _) => false,
            Self::Enum { .. } => true,
        }
    }
}

pub trait Schematic {
    fn create_schema() -> Schema;
}

pub fn schema<T: Schematic + 'static>() -> Arc<Schema> {
    static MAP: Lazy<Mutex<HashSet<Arc<Schema>>>> = Lazy::new(|| Mutex::new(HashSet::new()));
    let schema = T::create_schema();
    if !MAP.lock().unwrap().contains(&schema) {
        let schema = Arc::new(T::create_schema());
        MAP.lock().unwrap().insert(schema);
    }
    MAP.lock().unwrap().get(&schema).unwrap().clone()
}

impl Schematic for bool {
    fn create_schema() -> Schema {
        Schema::Bool
    }
}

impl Schematic for usize {
    fn create_schema() -> Schema {
        Schema::Int32
    }
}

impl Schematic for i32 {
    fn create_schema() -> Schema {
        Schema::Int32
    }
}

impl Schematic for i64 {
    fn create_schema() -> Schema {
        Schema::Int64
    }
}

impl Schematic for f32 {
    fn create_schema() -> Schema {
        Schema::Float32
    }
}

impl Schematic for f64 {
    fn create_schema() -> Schema {
        Schema::Float64
    }
}

impl Schematic for String {
    fn create_schema() -> Schema {
        Schema::String
    }
}

impl<T: Schematic + 'static> Schematic for Option<T> {
    fn create_schema() -> Schema {
        Schema::Option(schema::<T>())
    }
}

impl<T: Schematic + 'static> Schematic for Vec<T> {
    fn create_schema() -> Schema {
        Schema::Vec(schema::<T>())
    }
}

impl<K: Schematic + 'static, V: Schematic + 'static> Schematic for HashMap<K, V> {
    fn create_schema() -> Schema {
        Schema::Map(schema::<K>(), schema::<V>())
    }
}

#[test]
fn test() {
    assert_eq!(*schema::<i32>(), Schema::Int32);
    assert_eq!(*schema::<i64>(), Schema::Int64);
    assert_eq!(*schema::<f32>(), Schema::Float32);
    assert_eq!(*schema::<f64>(), Schema::Float64);
    assert_eq!(*schema::<String>(), Schema::String);
}
