use super::*;

impl<K: Trans + Eq + std::hash::Hash, V: Trans> Trans for HashMap<K, V> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Map(Schema::of::<K>(version), Schema::of::<V>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut result = Self::with_capacity(len);
        for _ in 0..len {
            result.insert(
                K::read_from(reader, version)?,
                V::read_from(reader, version)?,
            );
        }
        Ok(result)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.len().write_to(writer, version)?;
        for (key, value) in self {
            key.write_to(writer, version)?;
            value.write_to(writer, version)?;
        }
        Ok(())
    }
}

#[test]
fn test_schema() {
    assert_eq!(
        *Schema::of::<HashMap<String, usize>>(&crate::version()),
        Schema::Map(Arc::new(Schema::String), Arc::new(Schema::Int32)),
    );
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &HashMap::<i32, i32>::new());
    test_serde_eq(&crate::version(), &{
        let mut map = HashMap::new();
        map.insert("hello".to_owned(), 1);
        map.insert("world".to_owned(), 2);
        map
    });
}
