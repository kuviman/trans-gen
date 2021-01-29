use super::*;

impl<T: Trans> Trans for Vec<T> {
    fn create_schema(version: &Version) -> Schema {
        Schema::Vec(Schema::of::<T>(version))
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut result = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(T::read_from(reader, version)?);
        }
        Ok(result)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.len().write_to(writer, version)?;
        for item in self {
            item.write_to(writer, version)?;
        }
        Ok(())
    }
}

#[test]
fn test_schema() {
    assert_eq!(
        *Schema::of::<Vec<i32>>(&crate::version()),
        Schema::Vec(Arc::new(Schema::Int32))
    );
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &Vec::<String>::new());
    test_serde_eq(&crate::version(), &vec![1, 2, 3, 100500]);
}
