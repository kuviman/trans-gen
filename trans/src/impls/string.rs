use super::*;

impl Trans for String {
    fn create_schema(_version: &Version) -> Schema {
        Schema::String
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        let len = usize::read_from(reader, version)?;
        fn read_bytes(reader: impl std::io::Read, n: usize) -> std::io::Result<Vec<u8>> {
            use std::io::Read;
            let mut buf = Vec::new();
            let read_bytes = reader.take(n as u64).read_to_end(&mut buf)?;
            if read_bytes != n {
                return Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof));
            }
            Ok(buf)
        }
        let buf = read_bytes(reader, len)?;
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

#[test]
fn test_oom() {
    <String as Trans>::read_from(
        &mut serialize(&crate::version(), &i32::MAX).unwrap().as_slice(),
        &crate::version(),
    )
    .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
    .unwrap();
}
