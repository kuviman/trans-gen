use super::*;

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
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
    pub documentation: Documentation,
    pub name: Name,
    pub fields: Vec<Field>,
}

impl Struct {
    pub fn hashable(&self) -> bool {
        self.fields.iter().all(|field| field.schema.hashable())
    }
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
            Self::Struct(struc) => struc.hashable(),
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