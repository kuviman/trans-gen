use super::*;

impl Trans for i64 {
    fn create_schema(_version: &Version) -> Schema {
        Schema::Int64
    }
    fn read_from(reader: &mut dyn std::io::Read, _version: &Version) -> std::io::Result<Self> {
        reader.read_i64::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, _version: &Version) -> std::io::Result<()> {
        writer.write_i64::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<i64>(&crate::version()), Schema::Int64);
}

#[test]
fn test_serde() {
    test_serde_eq::<i64>(&crate::version(), &0);
    test_serde_eq::<i64>(&crate::version(), &123);
    test_serde_eq::<i64>(&&crate::version(), &-456);
    test_serde_eq::<i64>(&crate::version(), &100500);
    test_serde_eq::<i64>(&crate::version(), &i64::MIN);
    test_serde_eq::<i64>(&crate::version(), &i64::MAX);
}
