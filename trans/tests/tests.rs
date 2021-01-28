use trans::prelude::*;
use trans::*;

#[test]
fn test_expected_eof() {
    deserialize::<bool>(b"\x00\xFF")
        .ensure_err_contains(err_fmt::expected_eof())
        .unwrap();
}

#[test]
fn test_failed_to_read_field_invalid_usize() {
    #[derive(Trans, Debug)]
    struct TestStruct<T> {
        field: T,
    }
    let value: i32 = -1;
    deserialize::<TestStruct<usize>>(&serialize(&TestStruct { field: value }).unwrap())
        .ensure_err_contains(err_fmt::read_field::<TestStruct<usize>>("field"))
        .unwrap()
        .ensure_err_contains(err_fmt::invalid_value_of_type::<usize, _>(value))
        .unwrap();
}

#[test]
fn test_failed_to_read_variant_field_eof() {
    #[derive(Trans, Debug)]
    enum TestEnum {
        Variant { field: String },
    }
    deserialize::<TestEnum>(&serialize::<i32>(&0).unwrap())
        .ensure_err_contains(err_fmt::read_variant_field::<TestEnum>("Variant", "field"))
        .unwrap()
        .ensure_err_kind(std::io::ErrorKind::UnexpectedEof)
        .unwrap();
}
