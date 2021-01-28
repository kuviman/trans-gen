use super::*;

impl Trans for f64 {
    fn create_schema() -> Schema {
        Schema::Float64
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        reader.read_f64::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        writer.write_f64::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<f64>(), Schema::Float64);
}

#[test]
fn test_serde() {
    crate::test_serde::<f64>(&0.0);
    crate::test_serde::<f64>(&1.0);
    crate::test_serde::<f64>(&123.456);
    crate::test_serde::<f64>(&-100.5);
    crate::test_serde::<f64>(&f64::MIN);
    crate::test_serde::<f64>(&f64::MAX);
    crate::test_serde::<f64>(&f64::INFINITY);
    crate::test_serde::<f64>(&f64::NEG_INFINITY);
    crate::test_serde::<f64>(&std::f64::consts::PI);
    crate::test_serde::<f64>(&std::f64::consts::E);
    crate::test_serde::<f64>(&std::f64::consts::TAU);
    crate::test_serde::<f64>(&std::f64::consts::SQRT_2);
}
