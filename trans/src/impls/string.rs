use super::*;

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

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<String>(), Schema::String);
}

#[test]
fn test_serde() {
    crate::test_serde(&String::new());
    crate::test_serde(&String::from("Hello, world!"));
}
