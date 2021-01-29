use super::*;

impl Trans for f64 {
    fn create_schema(_version: &Version) -> Schema {
        Schema::Float64
    }
    fn read_from(reader: &mut dyn std::io::Read, _version: &Version) -> std::io::Result<Self> {
        reader.read_f64::<byteorder::LittleEndian>()
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, _version: &Version) -> std::io::Result<()> {
        writer.write_f64::<byteorder::LittleEndian>(*self)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<f64>(&crate::version()), Schema::Float64);
}

#[test]
fn test_serde() {
    test_serde_eq::<f64>(&crate::version(), &0.0);
    test_serde_eq::<f64>(&crate::version(), &1.0);
    test_serde_eq::<f64>(&crate::version(), &123.456);
    test_serde_eq::<f64>(&crate::version(), &-100.5);
    test_serde_eq::<f64>(&crate::version(), &f64::MIN);
    test_serde_eq::<f64>(&crate::version(), &f64::MAX);
    test_serde_eq::<f64>(&crate::version(), &f64::INFINITY);
    test_serde_eq::<f64>(&crate::version(), &f64::NEG_INFINITY);
    test_serde_eq::<f64>(&crate::version(), &std::f64::consts::PI);
    test_serde_eq::<f64>(&crate::version(), &std::f64::consts::E);
    test_serde_eq::<f64>(&crate::version(), &std::f64::consts::TAU);
    test_serde_eq::<f64>(&crate::version(), &std::f64::consts::SQRT_2);
}
