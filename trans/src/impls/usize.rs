use super::*;

impl Trans for usize {
    fn create_schema() -> Schema {
        Schema::Int32
    }
    fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
        Ok(try_from(i32::read_from(reader)?)?)
    }
    fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
        try_from::<usize, i32>(*self)?.write_to(writer)
    }
}

#[test]
fn test_schema() {
    assert_eq!(*Schema::of::<usize>(), Schema::Int32);
}

#[test]
fn test_invalid_deserialize() {
    let value: i32 = -123;
    deserialize::<usize>(&serialize(&value).unwrap())
        .ensure_err_contains(err_fmt::invalid_value_of_type::<usize, _>(value))
        .unwrap();
}

#[test]
fn test_invalid_serialize() {
    let value = usize::MAX;
    serialize::<usize>(&value)
        .ensure_err_contains(err_fmt::invalid_value_of_type::<i32, _>(value))
        .unwrap();
}

#[test]
fn test_serde() {
    test_utils::test_serde::<usize>(&0);
    test_utils::test_serde::<usize>(&123);
    test_utils::test_serde::<usize>(&100500);
}
