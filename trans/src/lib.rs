use byteorder::{ReadBytesExt, WriteBytesExt};
use heck::*;
use once_cell::sync::Lazy;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

pub use trans_derive::*;

pub mod prelude {
    pub use super::Trans;
    pub use trans_derive::*;
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

fn try_from<U: std::fmt::Debug + Copy, T: std::convert::TryFrom<U>>(
    value: U,
) -> std::io::Result<T> {
    T::try_from(value).map_err(|_| {
        std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("{:?} is invalid value", value),
        )
    })
}

pub trait Trans: Sized + 'static {
    fn create_schema() -> Schema;
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()>;
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self>;
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Name(String);

impl std::fmt::Debug for Name {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.0)
    }
}

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
pub struct LanguageDocumentation {
    pub language: String,
    pub text: String,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Documentation {
    pub languages: Vec<LanguageDocumentation>,
}

impl Documentation {
    pub fn get(&self, language: &str) -> Option<&str> {
        self.languages
            .iter()
            .find(|doc| doc.language == language)
            .map(|doc| doc.text.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Field {
    pub documentation: Documentation,
    pub name: Name,
    pub schema: Arc<Schema>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub magic: Option<i32>,
    pub documentation: Documentation,
    pub name: Name,
    pub fields: Vec<Field>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct EnumVariant {
    pub documentation: Documentation,
    pub name: Name,
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
        documentation: Documentation,
        base_name: Name,
        variants: Vec<Struct>,
    },
    Option(Arc<Schema>),
    Vec(Arc<Schema>),
    Map(Arc<Schema>, Arc<Schema>),
    Enum {
        documentation: Documentation,
        base_name: Name,
        variants: Vec<EnumVariant>,
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
    pub fn of<T: Trans>() -> Arc<Schema> {
        static MAP: Lazy<Mutex<HashSet<Arc<Schema>>>> = Lazy::new(|| Mutex::new(HashSet::new()));
        let schema = T::create_schema();
        if !MAP.lock().unwrap().contains(&schema) {
            let schema = Arc::new(T::create_schema());
            MAP.lock().unwrap().insert(schema);
        }
        MAP.lock().unwrap().get(&schema).unwrap().clone()
    }
}

macro_rules! impl_for_tuple {
    ($($name:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($name: Trans),*> Trans for ($($name,)*) {
            fn create_schema() -> Schema {
                todo!()
            }
            fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
                Ok(($(<$name as Trans>::read_from(reader)?,)*))
            }
            fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
                let ($($name,)*) = self;
                $($name.write_to(writer)?;)*
                Ok(())
            }
        }
    };
}

impl_for_tuple!();
impl_for_tuple!(A);
impl_for_tuple!(A, B);
impl_for_tuple!(A, B, C);
impl_for_tuple!(A, B, C, D);

impl Trans for bool {
    fn create_schema() -> Schema {
        Schema::Bool
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let value = reader.read_u8()?;
        match value {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Bool value should be 0 or 1",
            )),
        }
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_u8(if *self { 1 } else { 0 })
    }
}

impl Trans for usize {
    fn create_schema() -> Schema {
        Schema::Int32
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        Ok(try_from(i32::read_from(reader)?)?)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        try_from::<usize, i32>(*self)?.write_to(writer)
    }
}

impl Trans for i32 {
    fn create_schema() -> Schema {
        Schema::Int32
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_i32::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_i32::<byteorder::LittleEndian>(*self)
    }
}

impl Trans for i64 {
    fn create_schema() -> Schema {
        Schema::Int64
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_i64::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_i64::<byteorder::LittleEndian>(*self)
    }
}

impl Trans for f32 {
    fn create_schema() -> Schema {
        Schema::Float32
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_f32::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_f32::<byteorder::LittleEndian>(*self)
    }
}

impl Trans for f64 {
    fn create_schema() -> Schema {
        Schema::Float64
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_f64::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_f64::<byteorder::LittleEndian>(*self)
    }
}

impl Trans for String {
    fn create_schema() -> Schema {
        Schema::String
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let len = usize::read_from(reader)?;
        let mut buf = vec![0; len];
        reader.read_exact(&mut buf)?;
        String::from_utf8(buf).map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.len().write_to(writer)?;
        writer.write_all(self.as_bytes())
    }
}

impl<T: Trans> Trans for Option<T> {
    fn create_schema() -> Schema {
        Schema::Option(Schema::of::<T>())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let is_some = bool::read_from(reader)?;
        Ok(if is_some {
            Some(T::read_from(reader)?)
        } else {
            None
        })
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.is_some().write_to(writer)?;
        if let Some(value) = self {
            value.write_to(writer)?;
        }
        Ok(())
    }
}

impl<T: Trans> Trans for Vec<T> {
    fn create_schema() -> Schema {
        Schema::Vec(Schema::of::<T>())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let len = usize::read_from(reader)?;
        let mut result = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(T::read_from(reader)?);
        }
        Ok(result)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.len().write_to(writer)?;
        for item in self {
            item.write_to(writer)?;
        }
        Ok(())
    }
}

impl<K: Trans + Eq + std::hash::Hash, V: Trans> Trans for HashMap<K, V> {
    fn create_schema() -> Schema {
        Schema::Map(Schema::of::<K>(), Schema::of::<V>())
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let len = usize::read_from(reader)?;
        let mut result = Self::with_capacity(len);
        for _ in 0..len {
            result.insert(K::read_from(reader)?, V::read_from(reader)?);
        }
        Ok(result)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.len().write_to(writer)?;
        for (key, value) in self {
            key.write_to(writer)?;
            value.write_to(writer)?;
        }
        Ok(())
    }
}

#[test]
fn test() {
    assert_eq!(*Schema::of::<i32>(), Schema::Int32);
    assert_eq!(*Schema::of::<i64>(), Schema::Int64);
    assert_eq!(*Schema::of::<f32>(), Schema::Float32);
    assert_eq!(*Schema::of::<f64>(), Schema::Float64);
    assert_eq!(*Schema::of::<String>(), Schema::String);
}
