use super::*;

impl Trans for i64 {
    fn create_schema() -> Schema {
        Schema::Int64
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_i64::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_i64::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<i64>(), Schema::Int64);
}

#[test]
fn test_serde() {
    test_utils::test_serde::<i64>(&0);
    test_utils::test_serde::<i64>(&123);
    test_utils::test_serde::<i64>(&-456);
    test_utils::test_serde::<i64>(&100500);
    test_utils::test_serde::<i64>(&1_000_000_000_000_000_000);
    test_utils::test_serde::<i64>(&i64::MIN);
    test_utils::test_serde::<i64>(&i64::MAX);
}
