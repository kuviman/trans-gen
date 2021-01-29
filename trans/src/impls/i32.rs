use super::*;

impl Trans for i32 {
    fn create_schema(_version: &Version) -> Schema {
        Schema::Int32
    }
    fn read_from(reader: &mut dyn std::io::Read, _version: &Version) -> std::io::Result<Self> {
        reader.read_i32::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, _version: &Version) -> std::io::Result<()> {
        writer.write_i32::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<i32>(&crate::version()), Schema::Int32);
}

#[test]
fn test_serde() {
    test_serde_eq::<i32>(&crate::version(), &0);
    test_serde_eq::<i32>(&crate::version(), &123);
    test_serde_eq::<i32>(&&crate::version(), &-456);
    test_serde_eq::<i32>(&crate::version(), &100500);
    test_serde_eq::<i32>(&crate::version(), &i32::MIN);
    test_serde_eq::<i32>(&crate::version(), &i32::MAX);
}
