use super::*;

impl Trans for bool {
    fn create_schema(_version: &Version) -> Schema {
        Schema::Bool
    }
    fn read_from(reader: &mut dyn std::io::Read, _version: &Version) -> std::io::Result<Self> {
        let value = reader.read_u8()?;
        match value {
            0 => Ok(false),
            1 => Ok(true),
            value => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                error_format::invalid_bool(value),
            )),
        }
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, _version: &Version) -> std::io::Result<()> {
        writer.write_u8(if *self { 1 } else { 0 })
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<bool>(&crate::version()), Schema::Bool);
}

#[test]
fn test_invalid() {
    let value: u8 = 0xcd;
    deserialize::<bool>(&crate::version(), &[value])
        .ensure_err_contains(error_format::invalid_bool(value))
        .unwrap();
}

#[test]
fn test_serde() {
    test_serde_eq(&crate::version(), &false);
    test_serde_eq(&crate::version(), &true);
}
