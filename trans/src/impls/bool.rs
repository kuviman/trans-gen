use super::*;

impl Trans for bool {
    fn create_schema() -> Schema {
        Schema::Bool
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        let value = reader.read_u8()?;
        match value {
            0 => Ok(false),
            1 => Ok(true),
            value => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                err_fmt::invalid_bool(value),
            )),
        }
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_u8(if *self { 1 } else { 0 })
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<bool>(), Schema::Bool);
}

#[test]
fn test_invalid() {
    let value: u8 = 0xcd;
    deserialize::<bool>(&[value])
        .ensure_err_contains(err_fmt::invalid_bool(value))
        .unwrap();
}

#[test]
fn test_serde() {
    crate::test_serde(&false);
    crate::test_serde(&true);
}
