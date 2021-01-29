use super::*;

impl Trans for String {
    fn create_schema(_version: &Version) -> Schema {
        Schema::String
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        let mut buf = vec![0; len];
        reader.read_exact(&mut buf)?;
        String::from_utf8(buf).map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        self.len().write_to(writer, version)?;
        writer.write_all(self.as_bytes())
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<String>(&crate::version()), Schema::String);
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &String::new());
    test_serde_eq(&crate::version(), &String::from("Hello, world!"));
}
