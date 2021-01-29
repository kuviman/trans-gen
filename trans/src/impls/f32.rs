use super::*;

impl Trans for f32 {
    fn create_schema(_version: &Version) -> Schema {
        Schema::Float32
    }
    fn read_from(reader: &mut dyn std::io::Read, _version: &Version) -> std::io::Result<Self> {
        reader.read_f32::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, _version: &Version) -> std::io::Result<()> {
        writer.write_f32::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<f32>(&crate::version()), Schema::Float32);
}

#[test]
fn test_serde() {
    test_serde_eq::<f32>(&crate::version(), &0.0);
    test_serde_eq::<f32>(&crate::version(), &1.0);
    test_serde_eq::<f32>(&crate::version(), &123.456);
    test_serde_eq::<f32>(&crate::version(), &-100.5);
    test_serde_eq::<f32>(&crate::version(), &f32::MIN);
    test_serde_eq::<f32>(&crate::version(), &f32::MAX);
    test_serde_eq::<f32>(&crate::version(), &f32::INFINITY);
    test_serde_eq::<f32>(&crate::version(), &f32::NEG_INFINITY);
    test_serde_eq::<f32>(&crate::version(), &std::f32::consts::PI);
    test_serde_eq::<f32>(&crate::version(), &std::f32::consts::E);
    test_serde_eq::<f32>(&crate::version(), &std::f32::consts::TAU);
    test_serde_eq::<f32>(&crate::version(), &std::f32::consts::SQRT_2);
}
