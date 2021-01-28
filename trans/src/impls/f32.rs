use super::*;

impl Trans for f32 {
    fn create_schema() -> Schema {
        Schema::Float32
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_f32::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_f32::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<f32>(), Schema::Float32);
}

#[test]
fn test_serde() {
    crate::test_serde::<f32>(&0.0);
    crate::test_serde::<f32>(&1.0);
    crate::test_serde::<f32>(&123.456);
    crate::test_serde::<f32>(&-100.5);
    crate::test_serde::<f32>(&f32::MIN);
    crate::test_serde::<f32>(&f32::MAX);
    crate::test_serde::<f32>(&f32::INFINITY);
    crate::test_serde::<f32>(&f32::NEG_INFINITY);
    crate::test_serde::<f32>(&std::f32::consts::PI);
    crate::test_serde::<f32>(&std::f32::consts::E);
    crate::test_serde::<f32>(&std::f32::consts::TAU);
    crate::test_serde::<f32>(&std::f32::consts::SQRT_2);
}
