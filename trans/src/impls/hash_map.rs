use super::*;

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
fn test_schema() {
    assert_eq!(
        *Schema::of::<HashMap<String, usize>>(),
        Schema::Map(Schema::of::<String>(), Schema::of::<usize>()),
    );
}

#[test]
fn test_serde() {
    crate::test_serde(&HashMap::<i32, i32>::new());
    crate::test_serde(&{
        let mut map = HashMap::new();
        map.insert("hello".to_owned(), 1);
        map.insert("world".to_owned(), 2);
        map
    });
}
