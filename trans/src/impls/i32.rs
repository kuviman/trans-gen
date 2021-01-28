use super::*;

impl Trans for i32 {
    fn create_schema() -> Schema {
        Schema::Int32
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_i32::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_i32::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<i32>(), Schema::Int32);
}

#[test]
fn test_serde() {
    test_utils::test_serde::<i32>(&0);
    test_utils::test_serde::<i32>(&123);
    test_utils::test_serde::<i32>(&-456);
    test_utils::test_serde::<i32>(&100500);
    test_utils::test_serde::<i32>(&i32::MIN);
    test_utils::test_serde::<i32>(&i32::MAX);
}
