use super::*;

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

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<Vec<i32>>(), Schema::Vec(Schema::of::<i32>()));
}

#[test]
fn test_serde() {
    crate::test_serde(&Vec::<String>::new());
    crate::test_serde(&vec![1, 2, 3, 100500]);
}
