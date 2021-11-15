use super::*;

impl Trans for usize {
    fn create_schema(_version: &Version) -> Schema {
        Schema::Int32
    }
    fn read_from(reader: &mut dyn std::io::Read, version: &Version) -> std::io::Result<Self> {
        try_from(i32::read_from(reader, version)?)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write, version: &Version) -> std::io::Result<()> {
        try_from::<usize, i32>(*self)?.write_to(writer, version)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<usize>(&crate::version()), Schema::Int32);
}

#[test]
fn test_invalid_deserialize() {
    let value: i32 = -123;
    deserialize::<usize>(
        &crate::version(),
        &serialize(&crate::version(), &value).unwrap(),
    )
    .ensure_err_contains(error_format::invalid_value_of_type::<usize, _>(value))
    .unwrap();
}

#[test]
fn test_invalid_serialize() {
    let value = usize::MAX;
    serialize::<usize>(&crate::version(), &value)
        .ensure_err_contains(error_format::invalid_value_of_type::<i32, _>(value))
        .unwrap();
}

#[test]
fn test_serde() {
    test_serde_eq::<usize>(&crate::version(), &0);
    test_serde_eq::<usize>(&crate::version(), &123);
    test_serde_eq::<usize>(&crate::version(), &100500);
}
